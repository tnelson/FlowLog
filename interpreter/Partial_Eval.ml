(****************************************************************)
(* Flowlog's core evaluation and compilation code               *)
(****************************************************************)

open Flowlog_Types
open Flowlog_Packets
open Flowlog_Helpers
open NetCore_Types
open ExtList.List
open Printf
open Xsb_Communication
open Flowlog_Thrift_Out
open Partial_Eval_Validation

let policy_recreation_thunk: (unit -> unit) option ref = ref None;;

(* Map query formula to results and time obtained (floating pt in seconds) *)
let remote_cache: (term list list* float) FmlaMap.t ref = ref FmlaMap.empty;;

(* (unit->unit) option *)
(* Trigger thunk to refresh policy in stream. *)
let refresh_policy = ref None;;

(* action_atom list = atom *)
let fwd_actions = ref [];;

(* Push function for packet stream. Used to emit rather than forward. *)
(* Not sure why the input can be option. *)
let emit_push: ((NetCore_Types.switchId * NetCore_Types.portId * Packet.bytes) option -> unit) option ref = ref None;;

let guarded_refresh_policy () : unit = 
  match !refresh_policy with
    | None -> printf "Policy has not been created yet. Error!\n%!"
    | Some f -> f();;

let guarded_emit_push (swid: switchId) (pt: portId) (bytes: Packet.bytes): unit = 
  match !emit_push with
    | None -> printf "Packet stream has not been created yet. Error!\n%!"
    | Some f -> f (Some (swid,pt,bytes));;

let ms_on_packet_processing = ref 0.;;
let counter_inc_pkt = ref 0;;
let counter_inc_all = ref 0;;
let counter_pols_pushed = ref 0;;
let longest_used_packet_ms = ref 0.;;
let last_policy_pushed = ref (Action([]));;

exception ContradictoryActions of (string * string);;

(***************************************************************************************)

let rec get_state_maybe_remote (p: flowlog_program) (f: formula): term list list =
  match f with 
    | FAtom(modname, relname, args) when (is_remote_table p relname) ->
      (* Is this query still in the cache? Then just use it. *)
      if FmlaMap.mem f !remote_cache then
      begin
        let (cached_results, _) = FmlaMap.find f !remote_cache in
          cached_results
      end
      (* Otherwise, need to call out to the blackbox. *)
      else
      begin
        let remtbl = get_remote_table p relname in
        match remtbl.source with
          | RemoteTable(qryname, (ip, port), refresh) ->            
            (* qryname, not relname, when querying *)
            printf "REMOTE STATE --- REFRESHING: %s\n%!" (string_of_formula f);
            let bb_results = Flowlog_Thrift_Out.doBBquery qryname ip port args in
            let bb_termlists = (map (fun tlist -> map reassemble_xsb_term tlist) bb_results) in
            let bb_formulas = (map (fun tlist -> FAtom(modname, relname, tlist)) bb_termlists) in
              remote_cache := FmlaMap.add f (bb_termlists, Unix.time()) !remote_cache;
              iter Communication.assert_formula bb_formulas;
              bb_termlists

          | _ -> failwith "get_state_maybe_remote"
      end
    | FAtom(modname, relname, args) ->          
        Communication.get_state f
    | _ -> failwith "get_state_maybe_remote";;

(* get_state_maybe_remote calls out to BB and updates the cache IF UNCACHED. *)
let pre_load_all_remote_queries (p: flowlog_program): unit =
  (*printf ">>> %s\n%!" (String.concat "," (map string_of_formula (get_atoms_used_in_bodies p)));*)
  let remote_fmlas = 
    filter (function | FAtom(modname, relname, args) when (is_remote_table p relname) -> true
                     | _-> false)
           (get_atoms_used_in_bodies p) in           
    iter (fun f -> ignore (get_state_maybe_remote p f)) remote_fmlas;;    


(* Replace state references with constant matrices *)
let rec partial_evaluation (p: flowlog_program) (incpkt: string) (f: formula): formula = 
  (* assume valid clause body for PE *)
  match f with 
    | FTrue -> f
    | FFalse -> f
    | FEquals(t1, t2) -> f
    | FAnd(f1, f2) -> FAnd(partial_evaluation p incpkt f1, partial_evaluation p incpkt f2)
    | FNot(innerf) -> 
        let peresult = partial_evaluation p incpkt innerf in
        (match peresult with | FTrue -> FFalse | FFalse -> FTrue | _ -> FNot(peresult))
    | FOr(f1, f2) -> failwith "partial_evaluation: OR"              
    | FAtom(modname, relname, tlargs) ->  
      (*printf ">> partial_evaluation on atomic %s\n%!" (string_of_formula f);*)      

      (* No longer need to lock the mutex here, because called from respond_to_notification *)
      let xsbresults: (term list list) = get_state_maybe_remote p f in
      (*printf "DISJUNCTS FROM ATOM: %d\n%!" (length xsbresults);       *)        
        let disjuncts = map 
          (fun tl -> build_and (reassemble_xsb_equality incpkt tlargs tl)) 
          xsbresults in
        let fresult = build_or disjuncts in
        if !global_verbose >= 3 then 
          printf "<< partial evaluation result (converted from xsb) for %s\n    was: %s\n%!" (string_of_formula f) (string_of_formula fresult);        
        fresult;;

(***************************************************************************************)

let rec build_unsafe_switch_actions (oldpkt: string) (body: formula): action =
  let create_port_actions (actlist: action) (lit: formula): action =
    let no_contradiction_or_repetition (aval: string): bool = 
      for_all
        (function 
          (* Don't just check for <> pts. Prevent adding repetitions of same action.
             I.e., once you have a physical port, you never get another one. *)
          (* NetCore_Pattern.Physical(Int32.of_string aval)*)
          | SwitchAction(pat) ->
            (match pat.outPort with
              | NetCore_Pattern.Physical(aval2) ->                 
                if pat.outPort <> NetCore_Pattern.Physical(Int32.of_string aval) then
                  raise (ContradictoryActions(aval, Int32.to_string aval2))
                else 
                  false (* repetition, don't repeat! *)
              | _ -> true)
          | _ -> true)
        actlist
    in

    match lit with 
    | FFalse -> raise UnsatisfiableFlag
    | FTrue -> actlist
    (* old.locpt != new.locpt ---> allports (meaning: all but incoming) *)
    | FNot(FEquals(TField(var1, fld1), TField(var2, fld2))) -> 
      if var1 = oldpkt && fld1 = "locpt" && var2 <> oldpkt && fld2 = "locpt" then         
        [allportsatom] @ actlist 
      else if var2 = oldpkt && fld2 = "locpt" && var1 <> oldpkt && fld1 = "locpt" then 
        [allportsatom] @ actlist 
      else failwith ("create_port_actions: bad negation: "^(string_of_formula body))

    | FEquals(TField(var1, fld1), TField(var2, fld2)) ->
      if fld1 <> fld2 then 
        failwith ("create_port_actions: invalid fields: "^fld1^" "^fld2)
      else 
        actlist
    
    | FEquals(TField(avar, afld), TConst(aval)) 
    | FEquals(TConst(aval), TField(avar, afld)) -> 
      (* CHECK FOR CONTRADICTION WITH PRIOR ACTIONS! *)        
      if avar <> oldpkt && afld = "locpt" && (no_contradiction_or_repetition aval) then                  
        [SwitchAction({id with outPort = NetCore_Pattern.Physical(Int32.of_string aval)})]
        @ actlist      
      else       
        actlist

    (* If PE has left a disjunction or negated disjunction (or a standalone negated tuple),
        then it doesn't involve newpkt so ignore this conjunct. *)
    | FOr(_, _) -> actlist
    | FNot(FOr(_, _)) -> actlist
    | FNot(FAnd(_,_)) -> actlist

      (* remember: only called for FORWARD/EMIT rules. so safe to do this: *)
    | FNot(FEquals(TField(avar, afld), TConst(aval)))
    | FNot(FEquals(TConst(aval), TField(avar, afld))) -> 
        actlist


    | _ -> failwith ("create_port_actions: bad lit: "^(string_of_formula lit)) in



  (** Add NetCore action to set a packet's header fields during forwarding
      TODO(adf): add support for VLAN, VLAN PCP, IP ToS, and transport srcPort &
                 dstPort once Flowlog includes in the packet types *)
  let enhance_action_atom (afld: string) (aval: string) (anact: action_atom): action_atom =
  match anact with
    | SwitchAction(oldout) ->
      (match afld with 
        | "locpt" -> anact
        | "dlsrc" -> SwitchAction({oldout with outDlSrc = Some (None, Int64.of_string aval) })
        | "dldst" -> SwitchAction({oldout with outDlDst = Some (None, Int64.of_string aval) })
        | "dltyp" -> failwith ("OpenFlow 1.0 does not allow this field to be updated")
        | "nwsrc" -> SwitchAction({oldout with outNwSrc = Some (None, (Int32.of_string aval)) })
        | "nwdst" -> SwitchAction({oldout with outNwDst = Some (None, (Int32.of_string aval)) })
        | "nwproto" -> failwith ("OpenFlow 1.0 does not allow this field to be updated")
        | _ -> failwith ("enhance_action_atom unknown afld: "^afld^" -> "^aval))
    | _ -> failwith ("enhance_action_atom non SwitchAction: "^afld^" -> "^aval) in

  let create_mod_actions (actlist: action) (lit: formula): action =
    match lit with 
    | FFalse -> actlist
    | FTrue -> actlist
    | FNot(_) -> actlist
    | FEquals(TField(var1, fld1), TField(var2, fld2)) ->
      failwith ("create_mod_actions: invalid equality "^(string_of_formula body))

    | FEquals(TField(avar, afld), TConst(aval)) 
    | FEquals(TConst(aval), TField(avar, afld)) -> 
      if avar <> oldpkt then         
        (* since this is a FORWARD, must be over whatever we named newpkt *)
        map (enhance_action_atom afld aval) actlist
      else       
        actlist (* ignore involving newpkt *)

    | _ -> failwith ("create_mod_actions: "^(string_of_formula body)) in  

  (* list of SwitchAction(output)*)
  (* - this is only called for FORWARDING rules. so only newpkt should be involved *)
  (* - assume: no negated equalities except the special case pkt.locpt != newpkt.locpt *)  
  let atoms = conj_to_list body in
   (* printf "  >> build_switch_actions: %s\n%!" (String.concat " ; " (map (string_of_formula ~verbose:true) atoms));*)
    (* if any actions are false, folding is invalidated *)    
    try
      let port_actions = fold_left create_port_actions [] atoms in      
      let complete_actions = fold_left create_mod_actions port_actions atoms in      
      complete_actions
    with UnsatisfiableFlag -> [];;

open NetCore_Pattern
open NetCore_Wildcard

(* worst ocaml error ever: used "val" for varname. *)

let build_unsafe_switch_pred (oldpkt: string) (body: formula): pred =    
let field_to_pattern (fld: string) (aval:string): NetCore_Pattern.t =         
  match fld with (* switch handled via different pred type *)
    | "locpt" -> {all with ptrnInPort = WildcardExact (Physical(Int32.of_string aval)) }
    | "dlsrc" -> {all with ptrnDlSrc = WildcardExact (Int64.of_string aval) }
    | "dldst" -> {all with ptrnDlDst = WildcardExact (Int64.of_string aval) }
    | "dltyp" -> {all with ptrnDlTyp = WildcardExact (int_of_string aval) }
    | "nwsrc" -> {all with ptrnNwSrc = WildcardExact (Int32.of_string aval) }
    | "nwdst" ->  {all with ptrnNwDst = WildcardExact (Int32.of_string aval) }
    | "nwproto" -> {all with ptrnNwProto = WildcardExact (int_of_string aval) }
    | "tpsrc" -> {all with ptrnTpSrc = WildcardExact (int_of_string aval) }
    | "tpdst" ->  {all with ptrnTpDst = WildcardExact (int_of_string aval) }
    | _ -> failwith ("field_to_pattern: "^fld^" -> "^aval) in
    (* TODO: dlVLan, dlVLanPCP *)

  let rec eq_to_pred (eqf: formula): pred option =
    match eqf with
      | FNot(innerf) ->        
        (match eq_to_pred innerf with
        | None -> None
        | Some(p) -> 
          if p = Everything then Some Nothing
          else if p = Nothing then Some Everything
          else Some(Not(p)))

      (* only match oldpkt.<field> here*)        
      | FEquals(TConst(aval), TField(varname, fld)) when varname = oldpkt ->
        if fld = "locsw" then Some(OnSwitch(Int64.of_string aval))  
        else Some(Hdr(field_to_pattern fld aval))
      | FEquals(TField(varname, fld),TConst(aval)) when varname = oldpkt ->
        if fld = "locsw" then Some(OnSwitch(Int64.of_string aval))  
        else Some(Hdr(field_to_pattern fld aval))

      | FTrue -> Some(Everything)
      | FFalse -> Some(Nothing)   

      (* PE may leave a disjunction or negated disjunction if it doesn't involve newpkt
         (meaning it needs processing here) and inside the disj are conjunctions over tuples!

         Thus, this function is now terribly named... :-)  *)

      | FOr(f1, f2) -> 
          let p1 = eq_to_pred f1 in
          let p2 = eq_to_pred f2 in
          (match (p1, p2) with 
            | (None, None) -> None
            | (None, Some(apred))
            | (Some(apred), None) -> Some(apred)
            | (Some(apred1), Some(apred2)) -> Some(Or(apred1, apred2)))   
      | FAnd(f1, f2) -> 
          let p1 = eq_to_pred f1 in
          let p2 = eq_to_pred f2 in
         (match (p1, p2) with 
            | (None, None) -> None
            | (None, Some(apred))
            | (Some(apred), None) -> Some(apred)
            | (Some(apred1), Some(apred2)) -> Some(And(apred1, apred2)))  

      | _ -> None (* something for action, not pred *) in
      (*| _  -> failwith ("build_switch_pred: "^(string_of_formula ~verbose:true eqf)) in*)

  (* After PE, should be only equalities and negated equalities. Should be just a conjunction *)
  let eqlist = conj_to_list body in 
    let predlist = filter_map eq_to_pred eqlist in
      fold_left (fun acc pred -> match pred with 
              | Nothing -> Nothing
              | Everything -> acc
              | _ when acc = Everything -> pred 
              | _ when acc = Nothing -> acc 
              | _ -> And(acc, pred)) Everything predlist;; 

let is_all_ports_atom (a: action_atom): bool = 
  a = allportsatom;;

let get_physical_port_atom (a: action_atom): Int32.t option =
  match a with 
    | SwitchAction(swa) -> 
      (match swa.outPort with 
        | Physical(aval) -> Some(aval)
        | _ -> None)
    | _ -> None;;

let is_physical_port_atom (a: action_atom): bool =
  match get_physical_port_atom a with
    | Some(_) -> true
    | _ -> false;;

let handle_all_and_port_together (oldpkt: string) (apred: pred) (acts: action_atom list): (pred * action) =
  (* If both allportsatom and physical(x) appear in acts, 
     (1) remove allportsatom from acts
     (2) add oldpt != x to pred *)
  if exists is_all_ports_atom acts && 
     exists is_physical_port_atom acts then     
     let avalopt = get_physical_port_atom (find is_physical_port_atom acts) in 
     match avalopt with 
      | None -> failwith "handle_all_and_port_together"
      | Some(aval) -> 
        let newpred = And(apred, Not(Hdr({all with ptrnInPort = WildcardExact (Physical(aval))}))) in 
        let newacts = remove acts allportsatom in
          (*printf "Safe pred/act pair: %s THEN %s \n%!" (NetCore_Pretty.string_of_pred newpred) (NetCore_Pretty.string_of_action newacts);*)
          (newpred, newacts)
  else
  begin
    (*printf "(Was already) safe pred/act pair: %s THEN %s \n%!" (NetCore_Pretty.string_of_pred apred) (NetCore_Pretty.string_of_action acts);*)
    (apred, acts)
  end;;

(* Side effect: reads current state in XSB *)
(* Note: if given a non-packet-triggered clause, this function will happily compile it, but the trigger relation will be empty in current state
   and this reduce the clause to <false>. If the caller wants efficiency, it should pass only packet-triggered clauses. *)
let pkt_triggered_clause_to_netcore (p: flowlog_program) (callback: get_packet_handler option) (tcl: triggered_clause): (pred * action) list =   
    if !global_verbose > 4 then (match callback with 
      | None -> printf "\n--- Packet triggered clause to netcore (FULL COMPILE) on: \n%s\n%!" (string_of_triggered_clause tcl)
      | Some(_) -> printf "\n--- Packet triggered clause to netcore (~CONTROLLER~) on: \n%s\n%!" (string_of_triggered_clause tcl));

    match tcl.clause.head with 
      | FAtom(_, _, headargs) ->        
        (* Do partial evaluation. Need to know which terms are of the incoming packet.
          All others can be factored out. *)
        let pebody = partial_evaluation p tcl.oldpkt tcl.clause.body in
                
        (* partial eval may insert disjunctions because of multiple tuples to match 
           so we need to pull those disjunctions up and create multiple policies 
           since there may be encircling negation, also need to call nnf *)
        (* !! Don't need to NNF since no newpkts under negation *)

        (* todo: this is pretty inefficient for large numbers of tuples. do better? *)
        
        (*let bodies = disj_to_list (disj_to_top (nnf pebody)) in *)
        let bodies = disj_to_list (disj_to_top ~ignore_negation:true pebody) in 
        
        (*printf "bodies after nnf/disj_to_top = %s\n%!" (String.concat "   \n " (map string_of_formula bodies));*)
        (* anything not the old packet is a RESULT variable.
           Remember that we know this clause is packet-triggered, but
           we have no constraints on what gets produced. Maybe a bunch of 
           non-packet variables e.g. +R(x, y, z) ... *)

         (*printf "BODIES from PE of single clause: %d\n%!" (length bodies);       *)

        let result = 
          match callback with
            | None -> map (fun body ->  
                let unsafe_pred = build_unsafe_switch_pred tcl.oldpkt body in
                let unsafe_acts = build_unsafe_switch_actions tcl.oldpkt body in
                  (* Need to deal with cases where all-ports and physical(x)
                     coexist. Remember that all-ports FORBIDS input port! *)
                  handle_all_and_port_together tcl.oldpkt unsafe_pred unsafe_acts)
                          bodies                   
            | Some(f) -> 
              (* Action is always sending to controller. So don't extract action *)
              let bigpred = fold_left (fun acc body -> Or(acc, build_unsafe_switch_pred tcl.oldpkt body)) Nothing bodies in
                [(bigpred, [ControllerAction(f)])] in      

          result          
      | _ -> failwith "pkt_triggered_clause_to_netcore";;

(* return the union of policies for each clause *)
(* Side effect: reads current state in XSB *)
let pkt_triggered_clauses_to_netcore (p: flowlog_program) (clauses: triggered_clause list) (callback: get_packet_handler option): pol =  
  (*printf "ENTERING pkt_triggered_clauses_to_netcore!\n%!";*)
  let pre_unique_pas = appendall (map (pkt_triggered_clause_to_netcore p callback) clauses) in
  
  let clause_pas = unique ~cmp:(fun pair1 pair2 -> 
                              let (pp1, pa1) = pair1 in 
                              let (pp2, pa2) = pair2 in 
                                (safe_compare_actions pa1 pa2) && (smart_compare_preds pp1 pp2))   
                  pre_unique_pas in

  (*printf "Done creating clause_pas! %d members.\n%!" (length clause_pas);*)
  (*let separate_disjunct_pair (ap, aa) =
    match ap with 
      | Or(ap1, ap2) -> map (fun newpred -> (newpred, aa)) (gather_predicate_or ap) 
      | _ -> [(ap, aa)] in

      (* so many unique calls ---> expensive. TODO. change to sets. *)
  let pre_unique_disj_pas = (appendall (map separate_disjunct_pair clause_pas)) in
  let disj_pas = unique ~cmp:(fun pair1 pair2 -> 
                              let (pp1, pa1) = pair1 in 
                              let (pp2, pa2) = pair2 in 
                                (safe_compare_actions pa1 pa2) && (smart_compare_preds pp1 pp2))  
                 pre_unique_disj_pas in
  printf "Done creating disj_pas! %d members. before unique check was %d\n%!" (length disj_pas) (length pre_unique_disj_pas);*)

  (*iter (fun (ap, aa) -> write_log (sprintf "!!! %s %s\n%!"
                        (NetCore_Pretty.string_of_pred ap) 
                        (NetCore_Pretty.string_of_action aa)) ) clause_pas;
  iter (fun (ap, aa) -> write_log (sprintf "--- %s %s\n%!"
                        (NetCore_Pretty.string_of_pred ap) 
                        (NetCore_Pretty.string_of_action aa)) ) disj_pas;*)
  let or_of_preds_for_action (a: action): pred =
    fold_left (fun acc (smallpred, act) -> 
              if not (safe_compare_actions a act) then acc 
              else if acc = Nothing then smallpred
              else if smallpred = Nothing then acc
              else Or(acc, smallpred)) 
            Nothing
            clause_pas in

  (*let ite_of_preds_for_action (a: action): pol =    
    fold_left (fun acc (smallpred, act) ->               
              if not (safe_compare_actions a act) then acc 
              else 
              begin
                let simppred = simplify_netcore_predicate smallpred in 
                  if simppred = Nothing then acc
                  else ITE(simppred, Action(act), acc)
              end) 
            (Action([]))
            disj_pas in   *)

    if length clause_pas = 0 then 
      Action([])
    else if length clause_pas = 1 then
      let (pred, acts) = (hd clause_pas) in
        Seq(Filter(pred), Action(acts))
    else 
    begin                      
      let actionsused = unique ~cmp:safe_compare_actions 
                          (map (fun (ap, aa) -> aa) clause_pas) in 
      let actionswithphysicalports = filter (fun alst -> not (mem allportsatom alst)) actionsused in 

      (*printf "actionsued = %s\nactionswithphysicalports = %s\n%!"
       (String.concat ";" (map NetCore_Pretty.string_of_action actionsused))
       (String.concat ";" (map NetCore_Pretty.string_of_action actionswithphysicalports));*)

      (* Build a single union over policies for each distinct action *)
      (* if we get dup packets, make certain || isn't getting compiled to bag union in netcore *)
      let singleunion = fold_left 
                (fun (acc: pol) (aportaction: action) ->  
                  let newpred = simplify_netcore_predicate (or_of_preds_for_action aportaction) in
                  (*let newpol = ite_of_preds_for_action aportaction in*)
                 (*  write_log (sprintf "ite_of_preds_for_action: %s = %s" 
                    (NetCore_Pretty.string_of_action aportaction)
                    (NetCore_Pretty.string_of_pol newpol));*)
                  if newpred = Nothing then acc
                  else 
                    let newpol = Seq(Filter(newpred), Action(aportaction)) in 
                    (*let newpiece = ITE(newpred, Action(aportaction), Action([])) in *)
                      if acc = Action([]) then 
                        newpol
                      else
                        Union(acc, newpol))       
                (Action([]))
                actionswithphysicalports in

      (* the "allports" action must always be checked first*)
      (* If not all-ports, can safely union without overlap *)  
      let allportspred = simplify_netcore_predicate (or_of_preds_for_action [allportsatom]) in
      if allportspred <> Nothing then 
        ITE(allportspred, Action([allportsatom]), singleunion)          
      else
        singleunion
    end;;

(* Side effect: reads current state in XSB *)
(* Set up policies for all packet-triggered clauses *)
let program_to_netcore (p: flowlog_program) (callback: get_packet_handler): (pol * pol) =  
  (* posn 1: fully compilable packet-triggered. 
     posn 2: pre-weakened, non-fully-compilable packet-triggered *)
    (pkt_triggered_clauses_to_netcore p 
      p.can_fully_compile_to_fwd_clauses 
      None,
     pkt_triggered_clauses_to_netcore p 
      p.weakened_cannot_compile_pt_clauses
     (Some callback));;
  
let forward_packet (p: flowlog_program) (ev: event): unit =
  printf "forwarding: %s\n%!" (string_of_event p ev);
  write_log (sprintf ">>> forwarding: %s\n%!" (string_of_event p ev));
  (* TODO use allpackets here. compilation uses it, but XSB returns every port individually. *)
  printf "WARNING: field modifications not yet supported in netcore.\n%!";  
  fwd_actions := 
    SwitchAction({id with outPort = Physical(Int32.of_string (get_field ev "locpt" None))})
    :: !fwd_actions;;

let emit_packet (p: flowlog_program) (ev: event): unit =  
  printf "emitting: %s\n%!" (string_of_event p ev);
  write_log (sprintf ">>> emitting: %s\n%!" (string_of_event p ev));
  let swid = (Int64.of_string (get_field ev "locsw" None)) in
  let pt = (Int32.of_string (get_field ev "locpt" None)) in
  
  (* TODO: confirm dltyp/nwProto etc. are consistent with whatever type of packet we're producing 
     At the moment, someone can emit_arp with dlTyp = 0x000 or something dumb like that. *)
  guarded_emit_push swid pt (marshal_packet ev);;  

let send_event (p: flowlog_program) (ev: event) (ip: string) (pt: string): unit =
  printf "sending: %s\n%!" (string_of_event p ev);  
  write_log (sprintf ">>> sending: %s\n%!" (string_of_event p ev));
  doBBnotify ev ip pt;;

let event_with_field (p: flowlog_program) (ev_so_far : event) (fieldn: string) (avalue: term) : event =    
  match avalue with 
    | TConst(x) -> {ev_so_far with values=(StringMap.add fieldn x ev_so_far.values)}
    (* If returned a TVar, then need to flag to use the default value *)
    | TVar(x) -> {ev_so_far with values=(StringMap.add fieldn "" ev_so_far.values)}
    | _ -> failwith ("event_with_field:"^(string_of_term avalue));;

let prepare_output (p: flowlog_program) (incoming_event: event) (defn: outgoing_def): (event * spec_out) list =  
    let fieldnames = (match defn.outarity,defn.react with
                          | FixedEvent(evname),_ -> get_fields_for_type p evname
                          (*| AnyFields,OutPrint -> init (length tup) (fun i -> "x"^(string_of_int i))  *)
                          | SameAsOnFields,OutForward -> get_valid_fields_for_input_rel p incoming_event.typeid
                          | _ -> failwith "prepare_tuple") in

    let prepare_tuple (tup: term list): (event * spec_out) =
      (*printf "PREPARING OUTPUT... tuple: %s\n%!" (String.concat ";" (map string_of_term tup));*)
      (* arglist orders the xsb results. assigns says how to use them, spec how to send them. *)
      let initev = (match defn.react with 
                | OutForward -> {typeid = incoming_event.typeid; values=StringMap.empty}      
                | OutEmit(typ) -> {typeid = typ; values=StringMap.empty}      
                | OutLoopback -> failwith "loopback unsupported currently"
                | OutPrint -> failwith "print unsupported currently"
                | OutSend(outtype, _, _) -> {typeid=outtype; values=StringMap.empty}) in                   
      let ev = fold_left2 (event_with_field p) initev fieldnames tup in          
        (ev, defn.react) in
      
      (* query xsb for this output relation *)        
      let xsb_results = (Communication.get_state (FAtom("", defn.outname, map (fun s -> TVar(s)) fieldnames))) in                    
      (* return the results to be executed later *)
      map prepare_tuple xsb_results;;

let execute_output (p: flowlog_program) ((ev, spec): event * spec_out) : unit =
  match spec with 
   | OutForward -> forward_packet p ev
   | OutEmit(_) -> emit_packet p ev
   | OutPrint -> printf "PRINT RULE FIRED: %s\n%!" (string_of_event p ev)
   | OutLoopback -> failwith "loopback unsupported currently"
   | OutSend(_, ip, pt) -> send_event p ev ip pt;;

(* XSB query on plus or minus for table *)
let change_table_how (p: flowlog_program) (toadd: bool) (tbldecl: table_def): formula list =
  let relname,argtypes = tbldecl.tablename, tbldecl.tablearity in
    let modrelname = if toadd then (plus_prefix^"_"^relname) else (minus_prefix^"_"^relname) in
    let varlist = init (length argtypes) (fun i -> TVar("X"^string_of_int i)) in
    let xsb_results = Communication.get_state (FAtom("", modrelname, varlist)) in
    map (fun tup -> FAtom("", relname, tup)) xsb_results;;

let expire_remote_state_in_xsb (p: flowlog_program) : unit =

  (* The cache is keyed by rel/tuple. So R(X, 1) is a DIFFERENT entry from R(1, X). *)
  let expire_remote_if_time (p:flowlog_program) (keyfmla: formula) (values: ((term list) list * float)): unit =  
    if !global_verbose > 1 then printf "expire_remote_if_time %s\n%!" (string_of_formula keyfmla);
    let (xsb_results, timestamp) = values in   
    match keyfmla with
      | FAtom(modname, relname, args) -> 
      begin
        let remtbl = get_remote_table p relname in
         match remtbl.source with
          | RemoteTable(qryname, (ip, port), refresh) ->  
          begin
            match refresh with 
              | RefreshTimeout(num, units) when units = "seconds" -> 
                (* expire every num units. TODO: suppt more than seconds *)
                if Unix.time() > ((float_of_int num) +. timestamp) then begin
                  if !global_verbose > 1 then printf "REMOTE STATE --- Expiring remote for formula (duration expired): %s\n%!" 
                        (string_of_formula keyfmla);
                  remote_cache := FmlaMap.remove keyfmla !remote_cache;
                  iter (fun tup -> Communication.retract_formula (FAtom(modname, relname, tup))) xsb_results
                end else 
                  if !global_verbose > 1 then printf "REMOTE STATE --- Allowing relation to remain: %s %s %s\n%!" 
                    (string_of_formula keyfmla) (string_of_int num) (string_of_float timestamp);
                  ();
              | RefreshPure -> 
                (* never expire pure tables *) 
                (); 
              | RefreshEvery -> 
                (* expire everything under this table, every evaluation cycle *)
                if !global_verbose > 1 then printf "REMOTE STATE --- Expiring remote for formula: %s\n%!" (string_of_formula keyfmla);
                remote_cache := FmlaMap.remove keyfmla !remote_cache;
                iter (fun tup -> Communication.retract_formula (FAtom(modname, relname, tup))) xsb_results;
              | RefreshTimeout(_,_) -> failwith "expire_remote_state_in_xsb: bad timeout" 
          end
        | _ -> failwith "expire_remote_state_in_xsb: bad defn_decl" 
      end
      | _ -> failwith "expire_remote_state_in_xsb: bad key formula" in 

    FmlaMap.iter (expire_remote_if_time p) !remote_cache;;

  (* ASSUMPTION: event ids all same as their incoming relation name*)
let inc_event_to_relnames (p: flowlog_program) (notif: event): string list =
  (built_in_supertypes notif.typeid);;

let get_output_defns_triggered (p: flowlog_program) (notif: event): outgoing_def list =    
  let inrelnames = inc_event_to_relnames p notif in
  let outrelnames = fold_left (fun acc inrel -> (Hashtbl.find_all p.memos.out_triggers inrel ) @ acc) [] inrelnames in
  let possibly_triggered = filter (fun def -> mem def.outname outrelnames) p.outgoings in
    (*printf "possibly triggered: %s\n%!" (String.concat ",\n" (map string_of_reactive possibly_triggered));*)
    possibly_triggered;;

let get_local_tables_triggered (p: flowlog_program) (sign: bool) (notif: event): table_def list =    
  let inrelnames = inc_event_to_relnames p notif in
  let outrelnames = fold_left 
    (fun acc inrel -> (Hashtbl.find_all 
                        (if sign then p.memos.insert_triggers else p.memos.delete_triggers) inrel) @ acc) [] inrelnames in
  let possibly_triggered = filter (fun def -> mem def.tablename outrelnames) p.tables in
    (*printf "possibly triggered: %s\n%!" (String.concat ",\n" (map string_of_declaration possibly_triggered));*)
    possibly_triggered;;

(* Returns list of names of tables that have been modified *)
let respond_to_notification (p: flowlog_program) (notif: event): string list =
  try      
      Mutex.lock xsbmutex;
      ignore (Lwt_mutex.lock lwt_xsbmutex); (* may not be safe to ignore *)

      write_log "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<";
      write_log (sprintf "<<< incoming: %s" (string_of_event p notif));
      counter_inc_all := !counter_inc_all + 1;      

  (*printf "~~~~ RESPONDING TO NOTIFICATION ABOVE ~~~~~~~~~~~~~~~~~~~\n%!";*)

  (* populate the EDB with event *) 
    Communication.assert_event_and_subevents p notif;


    (* Expire remote state if needed*)
    expire_remote_state_in_xsb p;    

    (* Since we can't hook XSB's access to these relations,
       over-generalize and ask for all the fmlas that can possibly be needed.
       For instance, if foo(X, pkt.dlSrc) is used, ask for foo(X,Y) *)
    pre_load_all_remote_queries p;

    (* for all declared tables +/- *)
    let triggered_insert_table_decls = get_local_tables_triggered p true notif in
    let triggered_delete_table_decls = get_local_tables_triggered p false notif in 
    let to_assert = flatten (map (change_table_how p true) triggered_insert_table_decls) in
    let to_retract = flatten (map (change_table_how p false) triggered_delete_table_decls) in
    if !global_verbose >= 2 && (length to_assert > 0 || length to_retract > 0) then 
    begin 
      printf "  *** WILL ADD: %s\n%!" (String.concat " ; " (map string_of_formula to_assert));
      printf "  *** WILL DELETE: %s\n%!" (String.concat " ; " (map string_of_formula to_retract));
    end;
    write_log (sprintf "  *** WILL ADD: %s\n%!" (String.concat " ; " (map string_of_formula to_assert)));
    write_log (sprintf "  *** WILL DELETE: %s\n%!" (String.concat " ; " (map string_of_formula to_retract)));


    
   (**********************************************************)
   (* Prepare packets/events to be sent. *)
   (* This must be done BEFORE xsb is updated (output + updates must
      be computed from same EDB, and BEFORE we retract the event *)
   (* for all declared outgoing events ...*)
    let outgoing_defns = (get_output_defns_triggered p notif) in
    let prepared_output = flatten (map (prepare_output p notif) outgoing_defns) in
   (**********************************************************)


    (* depopulate event EDB *)
    Communication.retract_event_and_subevents p notif;      

    (* Return the tables that have actually being modified.
       TODO: not as smart as it could be: we aren't checking whether this stuff actually *changes the state*,
             just whether facts are being asserted/retracted. *)
    let modifications = map atom_to_relname (to_assert @ (subtract to_retract to_assert)) in

   (* Now that all the queries are completed, actually do stuff. *)

   (**********************************************************)
   (* UPDATE STATE IN XSB as dictated by +/- results stored before
      Semantics demand that retraction happens before assertion here! *)
   (* THIS MUST HAPPEN BEFORE POLICY IS UPDATED *)
    iter Communication.retract_formula to_retract;
    iter Communication.assert_formula to_assert;
    if !global_verbose >= 2 then
    begin      
      Xsb.debug_print_listings();
      Communication.get_and_print_xsb_state p;
    end;    
   (**********************************************************)   

   (**********************************************************)
   (* UPDATE POLICY ON SWITCHES  (use the NEW state)         *)
   (* Don't recreate a policy if there are no state changes! *)
    (match !policy_recreation_thunk with
      | Some(t) when (length modifications) > 0 -> t();
      | Some(t) -> (); (* we have a thunk, but no updates are necessary *)
      | None -> ());    
   (**********************************************************)

   (**********************************************************)
   (* Finally actually send output *)
    iter (execute_output p) prepared_output;
   (**********************************************************) 

    printf "~~~~~~~~~~~~~~~~~~~FINISHED EVENT (%d total, %d packets) ~~~~~~~~~~~~~~~\n%!"
          !counter_inc_all !counter_inc_pkt;

   (* Unlock the mutex. Make sure nothing uses XSB outside of this.*)
   Lwt_mutex.unlock lwt_xsbmutex;
   Mutex.unlock xsbmutex;  

    modifications (* return tables that may have changed *)

  with
   | Not_found -> 
       Communication.retract_event_and_subevents p notif;  
       Lwt_mutex.unlock lwt_xsbmutex; 
       Mutex.unlock xsbmutex;
       if !global_verbose > 0 then printf "Nothing to do for this event.\n%!"; [];
   | exn -> 
      begin  
        Format.printf "Unexpected exception on event. Event was: %s\n Exception: %s\n----------\n%s\n%!"
          (string_of_event p notif)
          (Printexc.to_string exn)
          (Printexc.get_backtrace ());  
        Xsb.halt_xsb();    
        Lwt_mutex.unlock lwt_xsbmutex;
        Mutex.unlock xsbmutex;        
        exit(101);        
      end;;

(* Frenetic reports every switch has a port 65534. *)
let lies_port = Int32.of_string "65534";;

(* If notables is true, send everything to controller *)
let make_policy_stream (p: flowlog_program) 
                       (notables: bool) 
                       (reportallpackets: bool) =  
  (* stream of policies, with function to push new policies on *)
  let (policies, push) = Lwt_stream.create () in

    let rec switch_event_handler (swev: switchEvent): unit =
      match swev with
      | SwitchUp(sw, feats) ->         
        let sw_string = Int64.to_string sw in        
        let notifs = 
          filter_map (fun portid -> 
            if portid <> lies_port then 
              Some {typeid="switch_port"; 
                    values=construct_map [("sw", sw_string);
                                          ("pt", (Int32.to_string portid))]}
            else None)
            feats.ports in
        printf "SWITCH %Ld connected. Flowlog events triggered: %s\n%!" sw (String.concat ", " (map (string_of_event p) notifs));
        List.iter (fun notif -> ignore (respond_to_notification p notif)) notifs;        

      | SwitchDown(swid) -> 
        let sw_string = Int64.to_string swid in        
        let notif = {typeid="switch_down"; values=construct_map [("sw", sw_string)]} in          
          printf "SWITCH %Ld went down. Triggered: %s\n%!" swid (string_of_event p notif);
          ignore(respond_to_notification p notif);                
    and

    reportPacketCallback (sw: switchId) (pt: port) (pkt: Packet.packet) : NetCore_Types.action =   
      printf "[REPORT ONLY] Packet arrived on switch %Ld, port %s.\n%s\n%!" 
        sw (NetCore_Pretty.string_of_port pt) (Packet.to_string pkt);
      [] 

    and 
    switch_event_handler_policy = HandleSwitchEvent(switch_event_handler) 
    and
    report_all_packets_policy = Action[ControllerAction(reportPacketCallback)]
    and
    internal_policy () = (if reportallpackets then Union(switch_event_handler_policy, report_all_packets_policy) 
                          else switch_event_handler_policy)
    and 
    (* the thunk needs to know the pkt callback, the pkt callback invokes the thunk. so need "and" *)
    trigger_policy_recreation_thunk (): unit = 
      write_log (sprintf "** policy recreation thunk triggered.\n");
      if not notables then
      begin
        (* Update the policy *)
        let (newfwdpol, newnotifpol) = program_to_netcore p updateFromPacket in

   (*     printf "NEW FWD policy: %s\n%!" (NetCore_Pretty.string_of_pol newfwdpol);
        printf "NEW NOTIF policy: %s\n%!" (NetCore_Pretty.string_of_pol newnotifpol);
     *)   
        let newpol = Union(Union(newfwdpol, newnotifpol), internal_policy()) in 
          (* Since can't compare functions, need to use custom comparison *)                    
          if not (safe_compare_pols newpol !last_policy_pushed) then
          begin
            counter_pols_pushed := !counter_pols_pushed + 1;
          
            printf "PUSHING NEW POLICY (number %d)!\n%!" !counter_pols_pushed;
            push (Some newpol); 
            last_policy_pushed := newpol;
            printf "PUSHED NEW POLICY!\n%!";
            write_log (sprintf "Pushed new policy (number %d).\n%!" !counter_pols_pushed);
            write_log (sprintf "NEW FWD policy: %s\n%!" (NetCore_Pretty.string_of_pol newfwdpol));
            write_log (sprintf "NEW NOTIF policy: %s\n%!" (NetCore_Pretty.string_of_pol newnotifpol));

          end
          else
          begin
            write_log (sprintf "NEW POLICY was the same. Did not push (last was number %d).\n" !counter_pols_pushed);
            printf "NEW POLICY was the same. Did not push (last was number %d).\n%!" !counter_pols_pushed;
          end
      end
      else
        if notables then printf "\n*** FLOW TABLE COMPILATION DISABLED! ***\n%!";
        (*DO NOT CALL THIS: push None*)
    and
      
    (* The callback to be invoked when the policy says to send pkt to controller *)
    (* callback here. *)
    updateFromPacket (sw: switchId) (pt: port) (pkt: Packet.packet) : NetCore_Types.action =        
      (* Update the policy via the push function *)
      let startt = Unix.gettimeofday() in 
      (*printf "Packet in on switch %Ld.\n%s\n%!" sw (Packet.to_string pkt);*)
      counter_inc_pkt := !counter_inc_pkt + 1;
      fwd_actions := []; (* populated by things respond_to_notification calls *)

      (* Parse the packet and send it to XSB. Deal with the results *)
      let notif = (pkt_to_event sw pt pkt) in           
        printf "~~~ Incoming Notif:\n %s\n%!" (string_of_event p notif);
        ignore (respond_to_notification p notif);

        if !global_verbose >= 1 then
        begin
          let used = (Unix.gettimeofday() -. startt) in
            
            ms_on_packet_processing := !ms_on_packet_processing +. used;
            
            if !longest_used_packet_ms < used then 
              longest_used_packet_ms := used;

            (* Keep an eye on XSB's status *)
            (*if (!counter_inc_pkt mod 200) = 199 then
              Xsb.print_statistics();*)

            printf "Time used: %fs. Average: %fs. Longest: %fs.\n%!"
               used (!ms_on_packet_processing /. (float_of_int !counter_inc_pkt)) !longest_used_packet_ms;
            printf "Asserts: %d. Retracts: %d. Send_asserts: %d. Send_queries: %d\n%!" 
              !count_assert_formula !count_retract_formula !count_send_assert !count_send_query;
        end;
        if !global_verbose >= 2 then
          printf "actions will be = %s\n%!" (NetCore_Pretty.string_of_action !fwd_actions);        
        (* This callback returns an action set to Frenetic. *) 
        !fwd_actions in

    (* For use elsewhere: call this to trigger policy update on switches. *)
    policy_recreation_thunk := Some trigger_policy_recreation_thunk;

    if not notables then
    begin
      let (initfwdpol, initnotifpol) = program_to_netcore p updateFromPacket in
      printf "INITIAL FWD policy is:\n%s\n%!" (NetCore_Pretty.string_of_pol initfwdpol);
      printf "INITIAL NOTIF policy is:\n%s\n%!" (NetCore_Pretty.string_of_pol initnotifpol);
      let initpol = Union(Union(initfwdpol, initnotifpol), internal_policy()) in            
        (trigger_policy_recreation_thunk, NetCore_Stream.from_stream initpol policies)
    end else begin      
      let initpol = Union(switch_event_handler_policy, Action([ControllerAction(updateFromPacket)])) in
        (trigger_policy_recreation_thunk, NetCore_Stream.from_stream initpol policies)
    end;;

