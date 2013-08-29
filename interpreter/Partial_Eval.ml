open Flowlog_Types
open NetCore_Types
open ExtList.List
open Printf

let is_forward_clause (cl: clause): bool =
	match cl.head with 
	| FAtom("", "forward", _) -> true
	| _ -> false;;

(* FORBIDDEN (pass to controller always):
% (1) joins over existentials [can do better]
% (2) assigns to newpkt fields not supported for modif. in OF [up to openflow to allow]
% (3) assigns to allowed newpkt field in pkt-dependent way, not state-dep way 
%    e.g. newpkt.dlSrc = pkt.dlDst
%    but R(newpkt.dlSrc, pkt.dlDst) is ok
%    ^^^ this may result in multiple packets being sent. but that's fine.
%    SPECIAL CASE: newpkt.locPt != pkt.locPt. Means to use AllPorts.
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

let packet_fields = ["locSw";"locPt";"dlSrc";"dlDst";"dlTyp";"nwSrc";"nwDst";"nwProto"];;
let legal_to_modify_packet_fields = ["locPt";"dlSrc";"dlDst";"dlTyp";"nwSrc";"nwDst"];;

let legal_field_to_modify (fname: string): bool =
	mem fname legal_to_modify_packet_fields;;

(* 2 & 3 *) 
let rec forbidden_assignment_check (newpkt: string) (f: formula) (innot: bool): unit = 
	let check_legal_fields = function  
							| TField(newpkt, fld) -> 
      				   			if not (legal_field_to_modify fld) then
      	 					   		raise (IllegalFieldModification f)
      	 					| _ -> () 
      	 				in
    let check_same_field_if_newpkt (newpkt: string) (t1:term) (t2:term) : unit = 
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
    		check_legal_fields t1; 
    		check_legal_fields t2;
    		check_same_field_if_newpkt newpkt t1 t2;
      	| FAtom(modname, relname, tlargs) ->       		
      		(* new field must be legal for modification by openflow *)
      		iter check_legal_fields tlargs;
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
  printf "MISSING: check to disallow pkt.x = pkt.y checks. these won't fit into OF either.\n%!";
	ignore (common_existential_check [] cl.body);	
	match cl.head with 
		| FAtom("", "forward", [TVar(newpktname)]) ->
			forbidden_assignment_check newpktname cl.body false
		| _ -> failwith "validate_clause";;

(***************************************************************************************)

let partial_evaluation (cl: clause): clause = 
	cl;;

(***************************************************************************************)

let flat_policy_from_flat_clause (cl: clause): pol = 	
	Filter(Nothing);;

(***************************************************************************************)

(* Side effect: reads current state in XSB *)
(* Throws exception rather than using option type: more granular error result *)
let clause_to_netcore (cl: clause): pol =
	(* Step 1: validate clause set: no forbidden joins or assignments *)
    validate_clause cl ;
	(* Step 2: perform partial evaluation [side effect: access XSB state] *)
	let pecl = partial_evaluation cl in
	(* Step 3: produce a flat netcore policy 
	   (This includes dealing with special case pkt.locPt != newpkt.locPt *)
		flat_policy_from_flat_clause pecl;;

(* return the union of policies for each clause *)
let forwarding_behavior_to_netcore (p: flowlog_program): pol =	
	let fwd_clauses = (filter is_forward_clause p.clauses) in
	let clause_pols = map clause_to_netcore fwd_clauses in
		if length clause_pols = 0 then 
			Filter(Nothing)
		else 
			fold_left (fun acc pol -> Union(acc, pol)) 
				(hd clause_pols) (tl clause_pols);;

