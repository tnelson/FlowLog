open Flowlog_Types
open Flowlog_Helpers
open NetCore_Types
open ExtList.List
open Printf
open Xsb_Communication
open OpenFlow0x01



(* 
  ALLOWED atomic fmlas:

  // assume in clause body. assume NNF. assume vars have been substituted out as much as possible
  // means that such vars that DO occur can never appear as x=y or x=const.
  
  
  (5) atom: 
*)

(* FORBIDDEN (pass to controller always):
% (1) joins over existentials [can do better here, no hard constraint]
% (2) assigns to newpkt fields not supported for modif. in OF, like nwProto [up to openflow to allow]
% (3) assigns to allowed newpkt field in pkt-dependent way, not state-dep way 
%    e.g. newpkt.dlSrc = pkt.dlDst
%    but R(newpkt.dlSrc, pkt.dlDst) is ok
%    ^^^ this may result in multiple packets being sent. but that's fine.
%    SPECIAL CASE: newpkt.locPt != pkt.locPt is allowed. Means to use AllPorts.
% (4) pkt.x = pkt.y --- can't do equality in netcore preds
*)

(* Any assignment to a newpkt field must either be 
  (1) the trivial old-field assignment WITHOUT a surrounding not
  (2) a positive relational binding (no surrounding NOT)      
  (3) special case NOT newpkt.locPt = oldpkt.locPt *)

exception IllegalFieldModification of formula;;
exception IllegalAssignmentViaEquals of formula;;
exception IllegalAtomMustBePositive of formula;;
exception IllegalExistentialUse of formula;;
exception IllegalModToNewpkt of (term * term);;
exception IllegalEquality of (term * term);;

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
      		if (innot && (ExtList.List.exists (function | TField(newpkt, _) -> true | _ -> false) tlargs)) then  	
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
          if fvar = newpkt then 
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

let validate_clause (cl: clause): unit =
  printf "Validating clause: %s\n%!" (string_of_clause cl);
	match cl.head with 
		| FAtom("", "do_forward", [TVar(newpktname)]) ->
      ignore (common_existential_check newpktname [] cl.body);  
			forbidden_assignment_check newpktname cl.body false
		| _ -> failwith "validate_clause";;

(***************************************************************************************)

(* Replace state references with constant matrices *)
let rec partial_evaluation (f: formula): formula = 
  (* assume valid clause body for PE *)
  match f with 
    | FTrue -> f
    | FFalse -> f
    | FEquals(t1, t2) -> f
    | FAnd(f1, f2) -> FAnd(partial_evaluation f1, partial_evaluation f2)
    | FNot(f) -> FNot(partial_evaluation f)
    | FOr(f1, f2) -> failwith "partial_evaluation"              
    | FAtom(modname, relname, tlargs) ->  
      printf "partial_evaluation on atomic %s\n%!" (string_of_formula f);
      let xsbresults: (string list) list = Communication.get_state f in
        (*iter (fun sl -> printf "result: %s\n%!" (String.concat "," sl)) results;*)
        let disjuncts = map 
          (fun sl -> build_and (reassemble_xsb_equality tlargs sl)) 
          xsbresults in
        let fresult = build_or disjuncts in
        printf "... result was: %s\n%!" (string_of_formula fresult);
        fresult;;

(***************************************************************************************)

let rec build_switch_actions (oldpkt: string) (body: formula): action =
  let create_port_actions (actlist: action) (lit: formula): action =
    match lit with 
    | FFalse -> actlist
    | FTrue -> failwith "create_port_actions: passed true"
    | FNot(FEquals(TField(var1, fld1), TField(var2, fld2))) -> 
      if var1 = oldpkt && fld1 = "locpt" && fld2 = "locpt" then         
        [SwitchAction({id with outPort = NetCore_Pattern.All})] @ actlist 
      else if var2 = oldpkt && fld2 = "locpt" && fld1 = "locpt" then 
        [SwitchAction({id with outPort = NetCore_Pattern.All})] @ actlist 
      else failwith ("create_port_actions: bad negation: "^(string_of_formula body))

    | FEquals(TField(var1, fld1), TField(var2, fld2)) ->
      if fld1 <> fld2 then 
        failwith ("create_port_actions: invalid fields: "^fld1^" "^fld2)
      else 
        actlist
    
    | FEquals(TField(avar, afld), TConst(aval)) 
    | FEquals(TConst(aval), TField(avar, afld)) -> 
      if afld = "locpt" then         
        [SwitchAction({id with outPort = NetCore_Pattern.Physical(Int32.of_string aval)})] @ actlist 
      else       
        actlist
    | _ -> failwith ("create_port_actions: "^(string_of_formula body)) in

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
    printf "  >> build_switch_actions: %s\n%!" (String.concat " ; " (map (string_of_formula ~verbose:true) atoms));
    let port_actions = fold_left create_port_actions [] atoms in
    let complete_actions = port_actions in (*fold_left create_mod_actions port_actions atoms in*)
  (* TODO: This includes dealing with special case pkt.locPt != newpkt.locPt *)      
      complete_actions;;

open NetCore_Pattern
open NetCore_Wildcard


(* worst ocaml error ever: used "val" for varname. *)

let build_switch_pred (oldpkt: string) (body: formula): pred =  
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
      | FNot(atom) -> 
        (match eq_to_pred atom with
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
      | _ -> None (* something for action, not pred *) in
      (*| _  -> failwith ("build_switch_pred: "^(string_of_formula ~verbose:true eqf)) in*)

  (* After PE, should be only equalities and negated equalities. Should be just a conjunction *)
  let eqlist = conj_to_list body in 
    let predlist = filter_map eq_to_pred eqlist in
      fold_left (fun acc pred -> match pred with 
              | Nothing -> Nothing
              | Everything -> acc
              | _ when acc = Everything -> pred 
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
      else failwith "trim_packet_from_clause: multiple variables used in packet_in"    
    | FNot(f) ->
      let (v, t) = trim_packet_from_body f in
        (v, FNot(t))
    | FOr(f1, f2) -> failwith "trim_packet_from_clause"              
    | FAtom("", packet_in_relname, [TVar(varstr)]) ->  
      (varstr, FTrue)
    | _ -> ("", body);;    


let policy_of_conjunction (oldpkt: string) (callback: get_packet_handler option) (body: formula): pol = 
  (* can't just say "if action is fwd, it's a fwd clause" because 
     this may be an approximation to an un-compilable clause, which needs
     interpretation at the controller! Instead trust callback to carry that info. *)  
      
  let my_action_list = match callback with
            | Some(f) -> [ControllerAction(f)]            
            | _ -> build_switch_actions oldpkt body in 

  let mypred = build_switch_pred oldpkt body in 

  Seq(Filter(mypred), Action(my_action_list));;
(*    ITE(mypred,
        Action(my_action_list), 
        Action([]));;*)
  	
(***************************************************************************************)

(* Used to pre-filter controller notifications as much as possible *)
let rec strip_to_valid (oldpkt: string) (cl: clause): clause =
  (* for now, naive solution: remove offensive literals outright. easy to prove correctness 
    of goal: result is a fmla that is implied by the real body. *)
    (* acc contains formula built so far, plus the variables seen *)
    let safeargs = (match cl.head with | FAtom(_, _, args) -> args | _ -> failwith "strip_to_valid") in
      printf "   --- ENFORCING VALIDITY: Removing literals from clause body: %s\n%!" (string_of_formula cl.body);
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
          begin printf "Removing atom: %s\n%!" (string_of_formula lit); acc end
        | FNot(FEquals(TField(v, f), TField(v2, f2))) when v = v2 && f2 <> f -> 
          begin printf "Removing atom: %s\n%!" (string_of_formula lit); acc end            

        (* If this atom involves an already-seen variable not in tlargs, remove it *)
        | FAtom (_, _, atomargs)  
        | FNot(FAtom (_, _, atomargs)) ->
          if length (list_intersection (subtract (filter not_is_oldpkt_field atomargs) safeargs) seen) > 0 then 
          begin
            (* removing this atom, so a fresh term shouldnt be remembered *)
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
          printf "   --- Final body was:%s\n%!" (string_of_formula final_formula);
          {head = cl.head; orig_rule = cl.orig_rule; body = final_formula};;

(* Side effect: reads current state in XSB *)
(* Throws exception rather than using option type: more granular error result *)
let pkt_triggered_clause_to_netcore (callback: get_packet_handler option) (cl: clause): pol =   
    printf "\n--- Packet triggered clause to netcore on: \n%s\n%!" (string_of_clause cl);
    match cl.head with 
      | FAtom(_, _, _) ->
        let (oldpkt, trimmedbody) = trim_packet_from_body cl.body in 
        printf "Trimmed packet from body: (%s, %s)\n%!" oldpkt (string_of_formula trimmedbody);

        (* Get a new fmla that is implied by the original, and can be compiled to a pred *)
        let safebody = (match callback with
          | Some(f) -> (strip_to_valid oldpkt {head = cl.head; orig_rule = cl.orig_rule; body = trimmedbody}).body
          | None ->    trimmedbody) in

        (* Do partial evaluation *)
        let pebody = partial_evaluation safebody in
                
        (* partial eval may insert disjunctions because of multiple tuples to match 
           so we need to pull those disjunctions up and create multiple policies *)
        (* todo: this is pretty inefficient for large numbers of tuples. do better? *)
        let bodies = disj_to_list (disj_to_top pebody) in 
        (* anything not the old packet is a RESULT variable.
           Remember that we know this clause is packet-triggered, but
           we have no constraints on what gets produced. Maybe a bunch of 
           non-packet variables e.g. +R(x, y, z) ... *)
        let result = fold_left (fun acc body -> 
                         (Union (acc, policy_of_conjunction oldpkt callback body)))
                      (policy_of_conjunction oldpkt callback (hd bodies))
                      (tl bodies) in 
          (*printf "--- Result policy: %s\n%!" (NetCore_Pretty.string_of_pol result);          *)
          result
      | _ -> failwith "pkt_triggered_clause_to_netcore";;


(* return the union of policies for each clause *)
(* Side effect: reads current state in XSB *)
let pkt_triggered_clauses_to_netcore (clauses: clause list) (callback: get_packet_handler option): pol =
  let clause_pols = map (pkt_triggered_clause_to_netcore callback) clauses in
    if length clause_pols = 0 then 
      Filter(Nothing)
    else 
      fold_left (fun acc pol -> Union(acc, pol)) 
        (hd clause_pols) (tl clause_pols);;  

let can_compile_clause (cl: clause): bool =  
  try 
    validate_clause cl;
    true
  with (* catch only "expected" exceptions *)
    | IllegalFieldModification(_) -> false
    | IllegalAssignmentViaEquals(_) -> false
    | IllegalAtomMustBePositive(_) -> false
    | IllegalExistentialUse(_) -> false
    | IllegalModToNewpkt(_, _) -> false
    | IllegalEquality(_,_) -> false;;

(* Side effect: reads current state in XSB *)
(* Set up policies for all packet-triggered clauses *)
let program_to_netcore (p: flowlog_program) (callback: get_packet_handler): (pol * pol) =
  printf "\n\n---------------------------------\nCompiling as able...\n%!";
  let fwd_clauses = (filter is_forward_clause p.clauses) in
  (*iter (fun cl -> printf "    FWD: %s\n%!" (string_of_clause cl)) fwd_clauses;*)
  let non_fwd_clauses_by_packets = (filter is_packet_triggered_clause (subtract p.clauses fwd_clauses)) in
  let can_fully_compile_fwd_clauses = (filter can_compile_clause fwd_clauses) in
  
  let cant_compile_fwd_clauses = subtract fwd_clauses can_fully_compile_fwd_clauses in
    (pkt_triggered_clauses_to_netcore can_fully_compile_fwd_clauses None,
     pkt_triggered_clauses_to_netcore (cant_compile_fwd_clauses @ non_fwd_clauses_by_packets) (Some callback));;

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
let event_with_assn (p: flowlog_program) (tup: string list) (ev_so_far : event) (assn: assignment): event =
  (* for this assignment, plug in the appropriate value in tup *)
  let fieldnames = (get_fields_for_type p ev_so_far.typeid) in
  let (index, _) = (findi (fun idx ele -> ele = assn.atupvar) fieldnames) in
  let const = (nth tup index) in 
  {ev_so_far with values=(StringMap.add assn.afield const ev_so_far.values)};;

let forward_packet (context: (switchId * port * Packet.packet) option) (ev: event): unit =
  printf "forwarding: %s\n%!" (string_of_event ev);
  ();;

let emit_packet (ev: event): unit =
  printf "emitting: %s\n%!" (string_of_event ev);
  ();;

let send_event (ev: event) (ip: string) (pt: string): unit =
  printf "sending: %s\n%!" (string_of_event ev);
  ();;


let execute_output (p: flowlog_program) (context: (switchId * port * Packet.packet) option) (defn: sreactive): unit =
  match defn with 
    | ReactOut(relname, arglist, outtype, assigns, spec) ->
     
      let execute_tuple (tup: string list): unit =
        (* arglist orders the xsb results. assigns says how to use them, spec how to send them. *)
        let initev = (match spec with 
                  | OutForward | OutEmit -> {typeid = "packet"; values=StringMap.empty}      
                  | OutLoopback -> failwith "loopback unsupported currently"
                  | OutSend(ip, pt) -> {typeid=outtype; values=StringMap.empty}) in        
        let ev = fold_left (event_with_assn p tup) initev assigns in
          match spec with 
            | OutForward -> forward_packet context ev
            | OutEmit -> emit_packet ev
            | OutLoopback -> failwith "loopback unsupported currently"
            | OutSend(ip, pt) -> send_event ev ip pt in

      (* query xsb for this output relation *)  
      let xsb_results = Communication.get_state (FAtom("", relname, map (fun s -> TVar(s)) arglist)) in        
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



(* separate to own module once works for sw/pt *)
let respond_to_notification (p: flowlog_program) (notif: event) (context: (switchId * port * Packet.packet) option): unit =
  printf "~~~~ RESPONDING TO NOTIFICATION ABOVE ~~~~~~~~~~~~~~~~~~~\n%!";
  (* populate the EDB with event *)  
  Communication.assert_event p notif;

  (* Update remote state if needed*)
  (* TODO *)

  (* for all declared outgoing events ...*)
  let outgoing_defns = get_output_defns p in
    iter (execute_output p context) outgoing_defns;

  (* for all declared tables +/- *)
  let table_decls = get_local_tables p in
  let to_assert = flatten (map (change_table_how p true) table_decls) in
  let to_retract = flatten (map (change_table_how p false) table_decls) in
  printf "  *** WILL ADD: %s\n%!" (String.concat " ; " (map string_of_formula to_assert));
  printf "  *** WILL DELETE: %s\n%!" (String.concat " ; " (map string_of_formula to_retract));
  (* update state as dictated by +/- *)
  iter Communication.assert_formula to_assert;
  iter Communication.retract_formula to_retract;

  Xsb.debug_print_listings();

  (* depopulate event EDB *)
  Communication.retract_event p notif;
  printf "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n%!";
  ();;

let make_policy_stream (p: flowlog_program) =  

  (* stream of policies, with function to push new policies on *)
  let (policies, push) = Lwt_stream.create () in

    let rec switch_event_handler (swev: switchEvent): unit =
      match swev with
      | SwitchUp(sw, feats) ->         
        let sw_string = Int64.to_string sw in  
        let notifs = map (fun portid -> {typeid="switch_port"; 
                                          values=construct_map [("sw", sw_string); ("pt", (Int32.to_string portid))] }) feats.ports in
        printf "SWITCH %Ld connected. Flowlog events triggered: %s\n%!" sw (String.concat ", " (map string_of_event notifs));
        List.iter (fun notif -> respond_to_notification p notif None) notifs;
        trigger_policy_recreation_thunk()

      | SwitchDown(swid) -> 
        printf "WARNING: switch down. Currently unsupported. TODO: create new event type in Flowlog.\n%!";
        ()
    and

    switch_event_handler_policy = HandleSwitchEvent(switch_event_handler) 
    and

    (* the thunk needs to know the pkt callback, the pkt callback invokes the thunk. so need "and" *)
    trigger_policy_recreation_thunk (): unit = 
      (* Update the policy *)
      let (newfwdpol, newnotifpol) = program_to_netcore p updateFromPacket in
      printf "NEW FWD policy: %s\n%!" (NetCore_Pretty.string_of_pol newfwdpol);
      printf "NEW NOTIF policy: %s\n%!" (NetCore_Pretty.string_of_pol newnotifpol);
      let newpol = Union(Union(newfwdpol, newnotifpol), switch_event_handler_policy) in
      (*let newpol = Action([ControllerAction(updateFromPacket); (SwitchAction({id with outPort = NetCore_Pattern.All}))]) in*)
        push (Some newpol);              
    and
      
    (* The callback to be invoked when the policy says to send pkt to controller *)
    updateFromPacket (sw: switchId) (pt: port) (pkt: Packet.packet) : NetCore_Types.action =    
      (* Update the policy via the push function *)
      printf "Packet in on switch %Ld.\n%s\n%!" sw (Packet.to_string pkt);

      (* Parse the packet and send it to XSB. Deal with the results *)
      let notif = (pkt_to_event sw pt pkt) in           
        printf "... notif: %s\n%!" (string_of_event notif);
        respond_to_notification p notif (Some (sw, pt, pkt));      
        trigger_policy_recreation_thunk();
        (* Do nothing more: (the callback returns action set) *) 
        [] in
  
    let (initfwdpol, initnotifpol) = program_to_netcore p updateFromPacket in
    printf "INITIAL FWD policy is:\n%s\n%!" (NetCore_Pretty.string_of_pol initfwdpol);
    printf "INITIAL NOTIF policy is:\n%s\n%!" (NetCore_Pretty.string_of_pol initnotifpol);
    let initpol = Union(Union(initfwdpol, initnotifpol), switch_event_handler_policy) in      
    (*let initpol = Action([ControllerAction(updateFromPacket); (SwitchAction({id with outPort = NetCore_Pattern.All}))]) in*)
      (* cargo-cult hacking invocation. why call this? *)
      (trigger_policy_recreation_thunk, NetCore_Stream.from_stream initpol policies);;


