open Flowlog_Types
open Flowlog_Helpers
open NetCore_Types
open ExtList.List
open Printf
open Xsb_Communication

let is_forward_clause (cl: clause): bool =
	match cl.head with 
	| FAtom("", "forward", _) -> true
	| _ -> false;;

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
exception IllegalEquality of (term * term);;

let packet_fields = ["locSw";"locPt";"dlSrc";"dlDst";"dlTyp";"nwSrc";"nwDst";"nwProto"];;
let legal_to_modify_packet_fields = ["locPt";"dlSrc";"dlDst";"dlTyp";"nwSrc";"nwDst"];;

let legal_field_to_modify (fname: string): bool =
	mem fname legal_to_modify_packet_fields;;

(* 2 & 3 *) 
let rec forbidden_assignment_check (newpkt: string) (f: formula) (innot: bool): unit = 
 	  let check_legal_newpkt_fields = function  
							| TField(varname, fld)
                when varname = newpkt -> 
      				   			if not (legal_field_to_modify fld) then
      	 					   		raise (IllegalFieldModification f)
      	 					| _ -> () 
      	 				in
    (* use of negation on equality: ok if [pkt.x = 5], [new.pt = old.pt], [newpkt.x = 5] *)
    let check_legal_negation (t1: term) (t2: term): unit =
      let dangerous = (match (t1, t2) with 
          | (TField(v1, f1), TField(v2, f2)) ->
            f1 <> "locPt" || f2 <> "locPt"
          | (TField(v1, f1), TConst(cstr)) -> v1 = newpkt
          | _ -> false) in 
      (*(printf "check_legal_negation: %s %s %b %b\n%!" (string_of_term t1) (string_of_term t2) innot dangerous);*)
      if (innot && dangerous) then raise (IllegalEquality(t1,t2))
    in

    let check_not_same_pkt (t1: term) (t2: term): unit =
      match (t1, t2) with 
        | (TField(v, f), TField(v2, f2)) when v = v2 ->
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

      	| FAtom(modname, relname, tlargs) ->       		
      		(* new field must be legal for modification by openflow *)
      		iter check_legal_newpkt_fields tlargs;
      		(* if involves a newpkt, must be positive *)    
      		if (innot && (ExtList.List.exists (function | TField(newpkt, _) -> true | _ -> false) tlargs)) then  	
      			raise (IllegalAtomMustBePositive f);;      		

(* returns list of existentials used by f. Will throw exception if re-used in new *atomic* fmla.
	assume this is a clause fmla (no disjunction) *)
let rec common_existential_check (sofar: string list) (f: formula): string list =
	let ext_helper (t: term): (string list) =
		match t with 
			| TVar(v) -> 
				if mem v sofar then raise (IllegalExistentialUse f)
				else [v]
			| TConst(_) -> []
			| TField(_,_) -> [] 
		in

	match f with
		| FTrue -> []
   	| FFalse -> []
   	| FAnd(f1, f2) ->
     		 let lhs_uses = common_existential_check sofar f1 in
     		 	(* unique is in ExtLst.List --- removes duplicates *)
     		 	unique (lhs_uses @ common_existential_check (lhs_uses @ sofar) f2)
  	| FOr(f1, f2) -> failwith "common_existential_check"      		 
  	| FNot(f) -> common_existential_check sofar f 
   	| FEquals(t1, t2) -> 
   		(* equals formulas don't represent a join. do nothing *)
   		sofar
   	| FAtom(modname, relname, tlargs) ->  
    		unique (flatten (map ext_helper tlargs));;	

let validate_clause (cl: clause): unit =
	ignore (common_existential_check [] cl.body);	
	match cl.head with 
		| FAtom("", "forward", [TVar(newpktname)]) ->
			forbidden_assignment_check newpktname cl.body false
		| _ -> failwith "validate_clause";;

(***************************************************************************************)

(* Replace state references with constant matrices *)
let rec partial_evaluation (f: formula): formula = 
  (* assume valid clause body for PE *)
  printf "partial_evaluation on %s\n%!" (string_of_formula f);
  match f with 
    | FTrue -> f
    | FFalse -> f
    | FEquals(t1, t2) -> f
    | FAnd(f1, f2) -> FAnd(partial_evaluation f1, partial_evaluation f2)
    | FNot(f) -> FNot(partial_evaluation f)
    | FOr(f1, f2) -> failwith "partial_evaluation"              
    | FAtom(modname, relname, tlargs) ->  
      let xsbresults: (string list) list = Communication.get_state f in
        (*iter (fun sl -> printf "result: %s\n%!" (String.concat "," sl)) results;*)
        let disjuncts = map 
          (fun sl -> build_and (reassemble_xsb_equality tlargs sl)) 
          xsbresults in
        let fresult = build_or disjuncts in
        printf "... result was: %s\n%!" (string_of_formula fresult);
        fresult;;

(***************************************************************************************)

let flat_policy_from_flat_clause (cl: clause) (callback: get_packet_handler option): pol = 
  (* can't just say "if action is fwd, it's a fwd clause" because 
     this may be an approximation to an un-compilable clause, which needs
     interpretation at the controller! *)  

  let my_action_list = match callback with
              | Some(f) -> [ControllerAction(f)]
              | _ ->  [] in (* TODO: convert to Fwd switch action (set!) *)

  let mypred = Nothing in (* TODO *)
  (* do nothing if pred isn't matched*)  
    ITE(mypred,
        Action(my_action_list), 
        Action([]));;

	Filter(Nothing);;

(***************************************************************************************)

(* Side effect: reads current state in XSB *)
(* Throws exception rather than using option type: more granular error result *)
let clause_to_netcore (callback: get_packet_handler option) (cl: clause): pol =
	(* Step 1: validate clause set: no forbidden joins or assignments 
    this is now done at start of process to separate what can be compiled from what can't *)
  (* )  validate_clause cl ;*)  
	(* Step 2: perform partial evaluation [side effect: access XSB state] *)
  (* Step 3: produce a flat netcore policy 
   (This includes dealing with special case pkt.locPt != newpkt.locPt *)
	let pecl = { head = cl.head; orig_rule = cl.orig_rule; body = partial_evaluation cl.body } in  
		flat_policy_from_flat_clause pecl callback;;

(* Used to pre-filter controller notifications as much as possible *)
let strip_to_valid (cl: clause): clause =
  cl;; (* TODO: remove atoms that make clause body uncompilable *)

(* return the union of policies for each clause *)
(* Side effect: reads current state in XSB *)
let clauses_to_netcore (clauses: clause list) (callback: get_packet_handler option): pol =
  let clause_pols = match callback with
      | Some(f) -> map (clause_to_netcore callback) (map strip_to_valid clauses)
      | None ->    map (clause_to_netcore None) clauses in
    if length clause_pols = 0 then 
      Filter(Nothing)
    else 
      fold_left (fun acc pol -> Union(acc, pol)) 
        (hd clause_pols) (tl clause_pols);;  

let can_compile_clause (cl: clause): bool =
  try 
    validate_clause cl;
    true
  with 
    | IllegalFieldModification(_) -> false
    | IllegalAssignmentViaEquals(_) -> false
    | IllegalAtomMustBePositive(_) -> false
    | IllegalExistentialUse(_) -> false
    | IllegalEquality(_,_) -> false;;

let subtract (biglst: 'a list) (toremove: 'a list): 'a list =
  (filter (fun ele -> not (mem ele toremove)) biglst);;

(* Side effect: reads current state in XSB *)
let program_to_netcore (p: flowlog_program) (callback: get_packet_handler): (pol * pol) =
  let fwd_clauses = (filter is_forward_clause p.clauses) in
  let non_fwd_clauses = subtract p.clauses fwd_clauses in
  let can_compile_fwd_clauses = (filter can_compile_clause fwd_clauses) in
  let cant_compile_fwd_clauses = subtract fwd_clauses can_compile_fwd_clauses in
    (clauses_to_netcore can_compile_fwd_clauses None,
     clauses_to_netcore (cant_compile_fwd_clauses @ non_fwd_clauses) (Some callback));;

let make_policy_stream (p: flowlog_program) =
  printf "Making policy and stream...\n%!";

  (* stream of policies, with function to push new policies on *)
  let (policies, push) = Lwt_stream.create () in

  (* The callback to be invoked when the policy says to send pkt to controller *)
  let rec updateFromPacket (sw: switchId) (pt: port) (pkt: Packet.packet) : NetCore_Types.action =    
    (* Update the policy via the push function *)
    printf "Packet in on switch %Ld.\n%s\n%!" sw (Packet.to_string pkt);
    let (newfwdpol, newnotifpol) = program_to_netcore p updateFromPacket in
    let newpol = Union(newfwdpol, newnotifpol) in
      push (Some newpol);        
      printf "NEW policy is:\n%s\n%!" (NetCore_Pretty.string_of_pol newpol);
      (* Do nothing more *) 
      [] in
  
    let (initfwdpol, initnotifpol) = program_to_netcore p updateFromPacket in
    let initpol = Union(initfwdpol, initnotifpol) in
      printf "INITIAL policy is:\n%s\n%!" (NetCore_Pretty.string_of_pol initpol);
      (* cargo-cult hacking invocation. why call this? *)
      NetCore_Stream.from_stream initpol policies;;


