open Flowlog_Types
open Flowlog_Helpers
open NetCore_Types
open ExtList.List
open Printf
open Xsb_Communication
open Flowlog_Thrift_Out
open Packet

(* output verbosity *)
(* 0 = default, no debug info at all *)
(* 2 adds XSB listings between events *)
(* 3 adds weakening and partial-evaluation info *)
(* 10 = even XSB messages *)
let global_verbose = ref 0;;

(* XSB is shared state. We also have the remember_for_forwarding and packet_queue business *)
let xsbmutex = Mutex.create();;

(* Map query formula to results and time obtained (floating pt in seconds) *)
let remote_cache = ref FmlaMap.empty;;

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


let counter_inc_pkt = ref 0;;
let counter_inc_all = ref 0;;
let counter_pols_pushed = ref 0;;

let last_policy_pushed = ref (Action([]));;

exception IllegalFieldModification of formula;;
exception IllegalAssignmentViaEquals of formula;;
exception IllegalAtomMustBePositive of formula;;
exception IllegalExistentialUse of formula;;
exception IllegalModToNewpkt of (term * term);;
exception IllegalEquality of (term * term);;

exception ContradictoryActions of (string * string);;

let legal_field_to_modify (fname: string): bool =
	mem fname legal_to_modify_packet_fields;;

(* 2 & 3 *) 
let rec forbidden_assignment_check (newpkt: string) (f: formula) (innot: bool): unit = 
    
    let check_netcore_temp_limit_eq (t1: term) (t2: term): unit = 
      match (t1, t2) with 
      | (TField(v1, f1), TConst(cstr)) ->
        (* can't modify packet fields right now. *CAN* set the port, of course. *)
        if (v1 = newpkt) && (f1 <> "locpt") then raise (IllegalModToNewpkt(t1,t2))
      | _ -> ()
    in

 	  let check_legal_newpkt_fields = function  
							| TField(varname, fld)
                when varname = newpkt -> 
      				   			if not (legal_field_to_modify fld) then
      	 					   		raise (IllegalFieldModification f)
      	 					| _ -> () 
      	 				in
    (* use of negation on equality: ok if [pkt.x = 5], [new.pt = old.pt] *)
    let check_legal_negation (t1: term) (t2: term): unit =
      let dangerous = (match (t1, t2) with 
          | (TField(v1, f1), TField(v2, f2)) ->
            f1 <> "locpt" || f2 <> "locpt"
          | (TField(v1, f1), TConst(cstr)) -> v1 = newpkt
          | _ -> false) in 
      (*(printf "check_legal_negation: %s %s %b %b\n%!" (string_of_term t1) (string_of_term t2) innot dangerous);*)
      if (innot && dangerous) then raise (IllegalEquality(t1,t2))
    in

    let check_not_same_pkt (t1: term) (t2: term): unit =
      match (t1, t2) with 
        | (TField(v, f), TField(v2, f2)) when v = v2 && f2 <> f ->
              raise (IllegalEquality(t1,t2))
        | _ -> ()
    in


    let check_same_field_if_newpkt (t1:term) (t2:term) : unit = 
    	match (t1, t2) with
			   | (TField(var1, fld1), TField(var2, fld2))
			     	when var1 = newpkt || var2 = newpkt ->
			       	if fld1 <> fld2 then raise (IllegalAssignmentViaEquals f)
      	 | (TField(var1, fld1), TVar(_)) 
            when var1 = newpkt -> raise (IllegalAssignmentViaEquals f)
      	 | (TVar(_), TField(var1, fld2)) 
            when var1 = newpkt -> raise (IllegalAssignmentViaEquals f)
      	 | (TField(var1, fld1), TConst(_)) -> ()	
      	 | (TConst(_), TField(var2, fld2)) -> ()
      	 | _ -> ()	      	 	
      	 	in

	match f with
		| FTrue -> ()
    	| FFalse -> ()
    	| FAnd(f1, f2) ->
      		 forbidden_assignment_check newpkt f1 innot;
      		 forbidden_assignment_check newpkt f2 innot;
      	| FOr(f1, f2) -> 
      		 forbidden_assignment_check newpkt f1 innot;
      		 forbidden_assignment_check newpkt f2 innot;
      	| FNot(f) -> forbidden_assignment_check newpkt f (not innot);
    	| FEquals(t1, t2) -> 
        (* ALLOWED: 
        (1) equality: between new and old, same field. NOT negated
        (2) equality: special case NOT(newpkt.locPt = pkt.locPt)
        (3) equality: new = const
        (4) equality: old = const *)
        check_legal_negation t1 t2; (* if negated, must be special case *)
    		check_legal_newpkt_fields t1; (* not trying to set an unsettable field *)
    		check_legal_newpkt_fields t2;
    		check_same_field_if_newpkt t1 t2; (* can't swap fields, etc. w/o controller *)
        check_not_same_pkt t1 t2;
        check_netcore_temp_limit_eq t1 t2;

      	| FAtom(modname, relname, tlargs) ->       		
      		(* new field must be legal for modification by openflow *)
      		iter check_legal_newpkt_fields tlargs;
      		(* if involves a newpkt, must be positive *)    
      		if (innot && (ExtList.List.exists (function | TField(fvar, _) when fvar = newpkt -> true | _ -> false) tlargs)) then  	
      			raise (IllegalAtomMustBePositive f);;      		

(* returns list of existentials used by f. Will throw exception if re-used in new *atomic* fmla.
	assume this is a clause fmla (no disjunction) *)
let rec common_existential_check (newpkt: string) (sofar: string list) (f: formula): string list =
  (* If extending this method beyond just FORWARD clauses, remember that any var in the head
      is "safe" to cross literals. *)
	let ext_helper (t: term): (string list) =
		match t with 
			| TVar(v) -> 
				if mem v sofar then raise (IllegalExistentialUse f)
				else [v]
			| TConst(_) -> []
			(* | TField(,_) -> [] *)
      (* netcore limitation in recent version, going away soon *)
      | TField(fvar,ffld) -> 
          if fvar = newpkt && ffld <> "locpt" then 
            raise (IllegalModToNewpkt(t, t))
          else [] 
		in

	match f with
		| FTrue -> []
   	| FFalse -> []
   	| FAnd(f1, f2) ->
     		 let lhs_uses = common_existential_check newpkt sofar f1 in
     		 	(* unique is in ExtLst.List --- removes duplicates *)
     		 	unique (lhs_uses @ common_existential_check newpkt (lhs_uses @ sofar) f2)
  	| FOr(f1, f2) -> failwith "common_existential_check"      		 
  	| FNot(f) -> common_existential_check newpkt sofar f 
   	| FEquals(t1, t2) -> 
   		(* equals formulas don't represent a join. do nothing *)
   		sofar
   	| FAtom(modname, relname, tlargs) ->  
    		unique (flatten (map ext_helper tlargs));;	

let validate_fwd_clause (cl: clause): unit =
  (*printf "Validating clause: %s\n%!" (string_of_clause cl);*)
	match cl.head with 
		| FAtom("", "forward", [TVar(newpktname)]) ->
      ignore (common_existential_check newpktname [] cl.body);  
			forbidden_assignment_check newpktname cl.body false;    
      printf "Forward clause was valid.\n%!";
		| _ -> failwith "validate_clause";;

(***************************************************************************************)

let rec get_state_maybe_remote (p: flowlog_program) (f: formula): (string list) list =
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
        match get_remote_table p relname with
          | (ReactRemote(relname, qryname, ip, port, refresh), DeclRemoteTable(drel, dargs)) ->            
            (* qryname, not relname, when querying *)
            printf "REMOTE STATE --- REFRESHING: %s\n%!" (string_of_formula f);
            let bb_results = Flowlog_Thrift_Out.doBBquery qryname ip port args in
              remote_cache := FmlaMap.add f (bb_results, Unix.time()) !remote_cache;
              iter Communication.assert_formula 
                (map (reassemble_xsb_atom modname relname) bb_results);
              bb_results 

          | _ -> failwith "get_state_maybe_remote"
      end
    | FAtom(modname, relname, args) ->          
        Communication.get_state f
    | _ -> failwith "get_state_maybe_remote";;

(* get_state_maybe_remote calls out to BB and updates the cache IF UNCACHED. *)
let pre_load_all_remote_queries (p: flowlog_program): unit =
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
      Mutex.lock xsbmutex;
      let xsbresults: (string list) list = get_state_maybe_remote p f in
      (*printf "DISJUNCTS FROM ATOM: %d\n%!" (length xsbresults);       *)
        Mutex.unlock xsbmutex;         
        let disjuncts = map 
          (fun sl -> build_and (reassemble_xsb_equality incpkt tlargs sl)) 
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

(*
  TODO: 
  outDlVlan : dlVlan match_modify;
  outDlVlanPcp : dlVlanPcp match_modify;  
}*)


  (* TODO: Netcore will support this soon (8/31) *)
  (*let enhance_action_atom (afld: string) (aval: string) (anact: action_atom): action_atom =
  match anact with
    SwitchAction(oldout) ->
      match afld with 
        | "locpt" -> SwitchAction({oldout with outPort = NetCore_Pattern.Physical(Int32.of_string aval)})
        | "dlsrc" -> SwitchAction({oldout with outDlSrc = (Int64.of_string aval) })
        | "dldst" -> SwitchAction({oldout with outDlDst = (Int64.of_string aval) })
        | "dltyp" -> SwitchAction({oldout with outDlTyp = (int_of_string aval) })
        | "nwsrc" -> SwitchAction({oldout with outNwSrc = (Int32.of_string aval) })
        | "nwdst" -> SwitchAction({oldout with outNwDst = (Int32.of_string aval) })
        | "nwproto" -> SwitchAction({oldout with outNwProto = (int_of_string aval) })
        | _ -> failwith ("enhance_action_atom: "^afld^" -> "^aval) in

  let create_mod_actions (actlist: action) (lit: formula): action =
    match lit with 
    | FFalse -> actlist
    | FTrue -> failwith "create_mod_actions: passed true"
    | FNot(_) -> 
      failwith ("create_mod_actions: bad negation: "^(string_of_formula body))
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
*)

  (* list of SwitchAction(output)*)
  (* - this is only called for FORWARDING rules. so only newpkt should be involved *)
  (* - assume: no negated equalities except the special case pkt.locpt != newpkt.locpt *)  
  let atoms = conj_to_list body in
   (* printf "  >> build_switch_actions: %s\n%!" (String.concat " ; " (map (string_of_formula ~verbose:true) atoms));*)
    (* if any actions are false, folding is invalidated *)    
    try
      let port_actions = fold_left create_port_actions [] atoms in
      let complete_actions = port_actions in (*fold_left create_mod_actions port_actions atoms in*)      
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

(* todo: lots of code overlap in these functions. should unify *)
(* removes the packet_in atom (since that's meaningless here). 
   returns the var the old packet was bound to, and the trimmed fmla *)
let rec trim_packet_from_body (body: formula): (string * formula) =
  match body with
    | FTrue -> ("", body)
    | FFalse -> ("", body)
    | FEquals(t1, t2) -> ("", body)
    | FAnd(f1, f2) -> 
      let (var1, trimmed1) = trim_packet_from_body f1 in
      let (var2, trimmed2) = trim_packet_from_body f2 in
      let trimmed = if trimmed1 = FTrue then 
                      trimmed2 
                    else if trimmed2 = FTrue then
                      trimmed1 
                    else
                      FAnd(trimmed1, trimmed2) in
      if (var1 = var2) || var1 = "" then
        (var2, trimmed)
      else if var2 = "" then
        (var1, trimmed)
      else failwith ("trim_packet_from_clause: multiple variables used in packet_in: "^var1^" and "^var2)
    | FNot(f) ->
      let (v, t) = trim_packet_from_body f in
        (v, FNot(t))
    | FOr(f1, f2) -> failwith "trim_packet_from_clause"              
    | FAtom("", relname, [TVar(varstr)]) when relname = packet_in_relname ->  
      (varstr, FTrue)
    | _ -> ("", body);;    

  	
(***************************************************************************************)

(* Used to pre-filter controller notifications as much as possible *)
let rec strip_to_valid (oldpkt: string) (cl: clause): clause =
  (* for now, naive solution: remove offensive literals outright. easy to prove correctness 
    of goal: result is a fmla that is implied by the real body. *)
    (* acc contains formula built so far, plus the variables seen *)
    let safeargs = (match cl.head with | FAtom(_, _, args) -> args | _ -> failwith "strip_to_valid") in
      if !global_verbose >= 3 then
        printf "   --- WEAKENING: Checking clause body in case need to weaken: %s\n%!" (string_of_formula cl.body);
      let may_strip_literal (acc: formula * term list) (lit: formula): (formula * term list) = 
        let (fmlasofar, seen) = acc in         

        let not_is_oldpkt_field (atomarg: term): bool = 
          match atomarg with
            | TField(fvar, ffld) when oldpkt = fvar -> false
            | _ -> true in

        match lit with 
        | FTrue -> acc
        | FFalse -> (FFalse, seen)
        | FNot(FTrue) -> (FFalse, seen)
        | FNot(FFalse) -> acc

        (* pkt.x = pkt.y  <--- can't be done in OF 1.0. just remove. *)
        | FEquals(TField(v, f), TField(v2, f2)) when v = v2 && f2 <> f -> 
          begin 
            if !global_verbose >= 3 then
              printf "Removing atom: %s\n%!" (string_of_formula lit);
            acc end
        | FNot(FEquals(TField(v, f), TField(v2, f2))) when v = v2 && f2 <> f -> 
          begin
            if !global_verbose >= 3 then 
              printf "Removing atom: %s\n%!" (string_of_formula lit);
            acc end            

        (* If this atom involves an already-seen variable not in tlargs, remove it *)
        | FAtom (_, _, atomargs)  
        | FNot(FAtom (_, _, atomargs)) ->
          if length (list_intersection (subtract (filter not_is_oldpkt_field atomargs) safeargs) seen) > 0 then 
          begin
            (* removing this atom, so a fresh term shouldnt be remembered *)
            if !global_verbose >= 3 then
              printf "Removing atom: %s\n%!" (string_of_formula lit);
            acc
          end 
          else if fmlasofar = FTrue then
             (lit, (unique (seen @ atomargs)))
          else 
            (FAnd(fmlasofar, lit), (unique (seen @ atomargs))) 

        | FOr(_, _) -> failwith "may_strip_literal: unsupported disjunction"

        (* everything else, just build the conjunction *)        
        | _ -> 
          if fmlasofar = FTrue then (lit, seen)
          else (FAnd(fmlasofar, lit), seen)
        in 

      let literals = conj_to_list cl.body in
        match literals with 
        | [] -> cl
        | _ ->
          let (final_formula, seen) = fold_left may_strip_literal (FTrue, []) literals in
          (*printf "   --- Final body was:%s\n%!" (string_of_formula final_formula);*)
          {head = cl.head; orig_rule = cl.orig_rule; body = final_formula};;

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
(* Throws exception rather than using option type: more granular error result *)
let pkt_triggered_clause_to_netcore (p: flowlog_program) (callback: get_packet_handler option) (cl: clause): (pred * action) list =   
   (* (match callback with 
      | None -> printf "\n--- Packet triggered clause to netcore (FULL COMPILE) on: \n%s\n%!" (string_of_clause cl)
      | Some(_) -> printf "\n--- Packet triggered clause to netcore (~CONTROLLER~) on: \n%s\n%!" (string_of_clause cl));
*)
    match cl.head with 
      | FAtom(_, _, headargs) ->
        let (oldpkt, trimmedbody) = trim_packet_from_body cl.body in 
        (*printf "Trimmed packet from body: (%s, %s)\n%!" oldpkt (string_of_formula trimmedbody);*)

        (* Get a new fmla that is implied by the original, and can be compiled to a pred *)
        let safebody = (match callback with
          | Some(f) -> (strip_to_valid oldpkt {head = cl.head; orig_rule = cl.orig_rule; body = trimmedbody}).body
          | None ->    trimmedbody) in

        (* Do partial evaluation. Need to know which terms are of the incoming packet.
          All others can be factored out. *)
        let pebody = partial_evaluation p oldpkt safebody in
                
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
                let unsafe_pred = build_unsafe_switch_pred oldpkt body in
                let unsafe_acts = build_unsafe_switch_actions oldpkt body in
                  (* Need to deal with cases where all-ports and physical(x)
                     coexist. Remember that all-ports FORBIDS input port! *)
                  handle_all_and_port_together oldpkt unsafe_pred unsafe_acts)
                          bodies                   
            | Some(f) -> 
              (* Action is always sending to controller. So don't extract action *)
              let bigpred = fold_left (fun acc body -> Or(acc, build_unsafe_switch_pred oldpkt body)) Nothing bodies in
                [(bigpred, [ControllerAction(f)])] in      

          result          
      | _ -> failwith "pkt_triggered_clause_to_netcore";;

(* return the union of policies for each clause *)
(* Side effect: reads current state in XSB *)
let pkt_triggered_clauses_to_netcore (p: flowlog_program) (clauses: clause list) (callback: get_packet_handler option): pol =  
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

let debug = true;;

let can_compile_clause_to_fwd (cl: clause): bool =  
  try 
    if is_forward_clause cl then 
    begin
        validate_fwd_clause cl;
        true
    end
    else
      false
  with (* catch only "expected" exceptions *)
    | IllegalFieldModification(_) -> if debug then printf "IllegalFieldModification\n%!"; false
    | IllegalAssignmentViaEquals(_) -> if debug then printf "IllegalAssignmentViaEquals\n%!"; false
    | IllegalAtomMustBePositive(_) -> if debug then printf "IllegalAtomMustBePositive\n%!"; false
    | IllegalExistentialUse(_) -> if debug then printf "IllegalExistentialUse\n%!"; false
    | IllegalModToNewpkt(_, _) -> if debug then printf "IllegalModToNewpkt\n%!"; false
    | IllegalEquality(_,_) -> if debug then printf "IllegalEquality\n%!"; false;;

(* Side effect: reads current state in XSB *)
(* Set up policies for all packet-triggered clauses *)
let program_to_netcore (p: flowlog_program) (callback: get_packet_handler): (pol * pol) =
  (*printf "\n\n---------------------------------\nCompiling as able...\n%!";  *)
    (pkt_triggered_clauses_to_netcore p 
      p.can_fully_compile_to_fwd_clauses 
      None,
     pkt_triggered_clauses_to_netcore p 
     (subtract p.clauses p.can_fully_compile_to_fwd_clauses)
     (Some callback));;

let pkt_to_event (sw : switchId) (pt: port) (pkt : Packet.packet) : event =       
   let isIp = ((Packet.dlTyp pkt) = 0x0800) in
   let isArp = ((Packet.dlTyp pkt) = 0x0806) in
   let values = [
    ("locsw", Int64.to_string sw);
    ("locpt", NetCore_Pretty.string_of_port pt);
    ("dlsrc", Int64.to_string pkt.Packet.dlSrc);
    ("dldst", Int64.to_string pkt.Packet.dlDst);
    ("dltyp", string_of_int (Packet.dlTyp pkt));
    (* nwSrc/nwDst will throw an exception if you call them on an unsuitable packet *)
    ("nwsrc", if (isIp || isArp) then Int32.to_string (Packet.nwSrc pkt) else "0");
    ("nwdst", if (isIp || isArp) then Int32.to_string (Packet.nwDst pkt) else "0");
    ("nwproto", if isIp then (string_of_int (Packet.nwProto pkt)) else "arp")
    ] in
    (*let _ = if debug then print_endline ("pkt to term list: " ^ (Type_Helpers.list_to_string Type_Helpers.term_to_string ans)) in
    let _ = if debug then print_endline ("dlTyp: " ^ (string_of_int (dlTyp pkt_payload))) in*)    
    {typeid="packet"; values=construct_map values};;

(* TODO: ugly func, should be cleaned up.*)
(* augment <ev_so_far> with assignment <assn>, using tuple <tup> for values *)
let event_with_assn (p: flowlog_program) (arglist: string list) (tup: string list) (ev_so_far : event) (assn: assignment): event =
  (* for this assignment, plug in the appropriate value in tup *)
  (*printf "event_with_assn %s %s %s %s\n%!" (String.concat ";" tup) (string_of_event ev_so_far) assn.afield assn.atupvar;*)
  (*let fieldnames = (get_fields_for_type p ev_so_far.typeid) in  *)  
  (* fieldnames is fields of *event*. don't use that here. 
     e.g. 'time=t' will error, expecting time, not t.*)
  try    
    let (index, _) = (findi (fun idx ele -> ele = assn.atupvar) arglist) in  
    let const = (nth tup index) in 
      {ev_so_far with values=(StringMap.add assn.afield const ev_so_far.values)}
  with Not_found -> 
    begin
      printf "Error assigning event field <%s> from variable <%s>: did not find that variable.\n%!" assn.afield assn.atupvar;
      exit(102)
    end;;

let forward_packet (ev: event): unit =
  printf "forwarding: %s\n%!" (string_of_event ev);
  write_log (sprintf ">>> forwarding: %s\n%!" (string_of_event ev));
  (* TODO use allpackets here. compilation uses it, but XSB returns every port individually. *)
  printf "WARNING: field modifications not yet supported in netcore.\n%!";  
  fwd_actions := 
    SwitchAction({id with outPort = Physical(Int32.of_string (get_field ev "locpt"))}) 
    :: !fwd_actions;;

let emit_packet (ev: event): unit =  
  printf "emitting: %s\n%!" (string_of_event ev);
  write_log (sprintf ">>> emitting: %s\n%!" (string_of_event ev));
  let swid = (Int64.of_string (get_field ev "locsw")) in
  let pt = (Int32.of_string (get_field ev "locpt")) in
  let dlSrc = Int64.of_string (get_field ev "dlsrc") in 
  let dlDst = Int64.of_string (get_field ev "dldst") in 
  let dlTyp = int_of_string (get_field ev "dltyp") in   
    
  (* todo: higher-layer stuff if the dltyp matches. ip or arp *)
  (*let nwSrc = Int64.of_string (get_field notif "NWSRC") in 
  let nwDst = Int64.of_string (get_field notif "NWDST") in 
  let nwProto = Int64.of_string (get_field notif "NWPROTO") in *)
        
  let pktbytes = Packet.marshal(
          {Packet.dlSrc = dlSrc; Packet.dlDst = dlDst;
           Packet.dlVlan = None; Packet.dlVlanPcp = 0;
           nw = Packet.Unparsable(dlTyp, Cstruct.create(0))
          }) in
    guarded_emit_push swid pt pktbytes;;

let send_event (ev: event) (ip: string) (pt: string): unit =
  printf ">>> sending: %s\n%!" (string_of_event ev);  
  write_log (sprintf "sending: %s\n%!" (string_of_event ev));
  doBBnotify ev ip pt;;

let execute_output (p: flowlog_program) (defn: sreactive): unit =  
  match defn with 
    | ReactOut(relname, argstrlist, outtype, assigns, spec) ->
     
      let execute_tuple (tup: string list): unit =
        printf "EXECUTING OUTPUT... tuple: %s\n%!" (String.concat ";" tup);
        (* arglist orders the xsb results. assigns says how to use them, spec how to send them. *)
        let initev = (match spec with 
                  | OutForward | OutEmit -> {typeid = "packet"; values=StringMap.empty}      
                  | OutLoopback -> failwith "loopback unsupported currently"
                  | OutSend(ip, pt) -> {typeid=outtype; values=StringMap.empty}) in                
        let ev = fold_left (event_with_assn p argstrlist tup) initev assigns in          
          match spec with 
            | OutForward -> forward_packet  ev
            | OutEmit -> emit_packet ev
            | OutLoopback -> failwith "loopback unsupported currently"
            | OutSend(ip, pt) -> send_event ev ip pt in

      (* query xsb for this output relation *)  
      let xsb_results = Communication.get_state (FAtom("", relname, map (fun s -> TVar(s)) argstrlist)) in        
        (* execute the results *)
        iter execute_tuple xsb_results 
    | _ -> failwith "execute_output";;

(* XSB query on plus or minus for table *)
let change_table_how (p: flowlog_program) (toadd: bool) (tbldecl: sdecl): formula list =
  match tbldecl with
    | DeclTable(relname, argtypes) -> 
      let modrelname = if toadd then (plus_prefix^"_"^relname) else (minus_prefix^"_"^relname) in
      let varlist = init (length argtypes) (fun i -> TVar("X"^string_of_int i)) in
      let xsb_results = Communication.get_state (FAtom("", modrelname, varlist)) in
      map (fun strtup -> FAtom("", relname, map (fun sval -> TConst(sval)) strtup)) xsb_results
    | _ -> failwith "change_table_how";;

let expire_remote_state_in_xsb (p: flowlog_program) : unit =

  (* The cache is keyed by rel/tuple. So R(X, 1) is a DIFFERENT entry from R(1, X). *)
  let expire_remote_if_time (p:flowlog_program) (keyfmla: formula) (values: ((string list) list * float)): unit =  
    printf "expire_remote_if_time %s\n%!" (string_of_formula keyfmla);
    let (xsb_results, timestamp) = values in   
    match keyfmla with
      | FAtom(modname, relname, args) -> 
      begin
      match get_remote_table p relname with
        | (ReactRemote(relname, qryname, ip, port, refresh), DeclRemoteTable(drel, dargs)) ->
          begin
            match refresh with 
              | RefreshTimeout(num, units) when units = "seconds" -> 
                (* expire every num units. TODO: suppt more than seconds *)
                if Unix.time() > ((float_of_int num) +. timestamp) then begin
                  printf "REMOTE STATE --- Expiring remote for formula (duration expired): %s\n%!" 
                        (string_of_formula keyfmla);
                  remote_cache := FmlaMap.remove keyfmla !remote_cache;
                  iter (fun tup -> Communication.retract_formula (reassemble_xsb_atom modname drel tup)) xsb_results
                end else 
                  printf "REMOTE STATE --- Allowing relation to remain: %s %s %s\n%!" 
                    (string_of_formula keyfmla) (string_of_int num) (string_of_float timestamp);
                  ();
              | RefreshPure -> 
                (* never expire pure tables *) 
                (); 
              | RefreshEvery -> 
                (* expire everything under this table, every evaluation cycle *)
                printf "REMOTE STATE --- Expiring remote for formula: %s\n%!" (string_of_formula keyfmla);
                remote_cache := FmlaMap.remove keyfmla !remote_cache;
                iter (fun tup -> Communication.retract_formula (reassemble_xsb_atom modname drel tup)) xsb_results;
              | RefreshTimeout(_,_) -> failwith "expire_remote_state_in_xsb: bad timeout" 
          end
        | _ -> failwith "expire_remote_state_in_xsb: bad defn_decl" 
      end
      | _ -> failwith "expire_remote_state_in_xsb: bad key formula" in 

    FmlaMap.iter (expire_remote_if_time p) !remote_cache;;

(* separate to own module once works for sw/pt *)
let respond_to_notification (p: flowlog_program) (notif: event): unit =
  try
      Mutex.lock xsbmutex;
      counter_inc_all := !counter_inc_all + 1;

      write_log (sprintf "<<< incoming: %s" (string_of_event notif));

  (*printf "~~~~ RESPONDING TO NOTIFICATION ABOVE ~~~~~~~~~~~~~~~~~~~\n%!";*)

  (* populate the EDB with event *) 
    Communication.assert_event p notif;

    (* Expire remote state if needed*)
    expire_remote_state_in_xsb p;    

    (* Since we can't hook XSB's access to these relations,
       over-generalize and ask for all the fmlas that can possibly be needed.
       For instance, if foo(X, pkt.dlSrc) is used, ask for foo(X,Y) *)
    pre_load_all_remote_queries p;

    (* for all declared outgoing events ...*)
    let outgoing_defns = get_output_defns p in
      iter (execute_output p) outgoing_defns;

    (* for all declared tables +/- *)
    let table_decls = get_local_tables p in
    let to_assert = flatten (map (change_table_how p true) table_decls) in
    let to_retract = flatten (map (change_table_how p false) table_decls) in
    if !global_verbose >= 2 && (length to_assert > 0 || length to_retract > 0) then 
    begin 
      printf "  *** WILL ADD: %s\n%!" (String.concat " ; " (map string_of_formula to_assert));
      printf "  *** WILL DELETE: %s\n%!" (String.concat " ; " (map string_of_formula to_retract));
    end;
    write_log (sprintf "  *** WILL ADD: %s\n%!" (String.concat " ; " (map string_of_formula to_assert)));
    write_log (sprintf "  *** WILL DELETE: %s\n%!" (String.concat " ; " (map string_of_formula to_retract)));
    (* update state as dictated by +/-
      Semantics demand that retraction happens before assertion here! *)
    iter Communication.retract_formula to_retract;
    iter Communication.assert_formula to_assert;
    
    if !global_verbose >= 2 then
      Xsb.debug_print_listings();   

    (* depopulate event EDB *)
    Communication.retract_event p notif;  

    Mutex.unlock xsbmutex;  
    printf "~~~~~~~~~~~~~~~~~~~FINISHED EVENT (%d total, %d packets) ~~~~~~~~~~~~~~~\n%!"
          !counter_inc_all !counter_inc_pkt;
  with
   | Not_found -> Mutex.unlock xsbmutex; printf "Nothing to do for this event.\n%!";
   | exn -> 
      begin  
      Format.printf "Unexpected exception on event: %s\n----------\n%s\n%!"
        (Printexc.to_string exn)
        (Printexc.get_backtrace ());  
        Xsb.halt_xsb();    
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
        printf "SWITCH %Ld connected. Flowlog events triggered: %s\n%!" sw (String.concat ", " (map string_of_event notifs));
        List.iter (fun notif -> respond_to_notification p notif) notifs;
        trigger_policy_recreation_thunk()

      | SwitchDown(swid) -> 
        let sw_string = Int64.to_string swid in        
        let notif = {typeid="switch_down"; values=construct_map [("sw", sw_string)]} in          
          printf "SWITCH %Ld went down. Triggered: %s\n%!" swid (string_of_event notif);
          respond_to_notification p notif;
          trigger_policy_recreation_thunk()        
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
    updateFromPacket (sw: switchId) (pt: port) (pkt: Packet.packet) : NetCore_Types.action =  

      (* Update the policy via the push function *)
      let startt = Unix.gettimeofday() in 
      (*printf "Packet in on switch %Ld.\n%s\n%!" sw (Packet.to_string pkt);*)
      counter_inc_pkt := !counter_inc_pkt + 1;
      fwd_actions := []; (* populated by things respond_to_notification calls *)

      (* Parse the packet and send it to XSB. Deal with the results *)
      let notif = (pkt_to_event sw pt pkt) in           
        printf "... notif: %s\n%!" (string_of_event notif);
        respond_to_notification p notif;      
        trigger_policy_recreation_thunk();

        if !global_verbose >= 1 then
          printf "Time used: %fs\n%!" (Unix.gettimeofday() -. startt);
        if !global_verbose >= 2 then
          printf "actions will be = %s\n%!" (NetCore_Pretty.string_of_action !fwd_actions);        
        (* This callback returns an action set to Frenetic. *) 
        !fwd_actions in

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

