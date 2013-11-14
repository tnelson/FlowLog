(****************************************************************)
(* Flowlog's core evaluation and compilation code               *)
(****************************************************************)

open Flowlog_Types
open Flowlog_Packets
open Flowlog_Helpers
open ExtList.List
open Printf

exception IllegalFieldModification of formula;;
exception IllegalAssignmentViaEquals of formula;;
exception IllegalAtomMustBePositive of formula;;
exception IllegalExistentialUse of formula;;
exception IllegalModToNewpkt of (term * term);;
exception IllegalEquality of (term * term);;
exception NonTableField of formula;;

let legal_field_to_modify (fname: string): bool =
	mem fname legal_to_modify_packet_fields;;

let compilable_field_to_test (fname: string): bool =
  mem fname packet_fields;;

let rec forbidden_assignment_check (newpkt: string) (f: formula) (innot: bool): unit = 
    
    let check_netcore_temp_limit_eq (t1: term) (t2: term): unit = 
      match (t1, t2) with 
      | (TField(v1, f1), TConst(cstr)) ->
        (* can't modify packet fields right now. *CAN* set the port, of course. *)
        if (v1 = newpkt) && (f1 <> "locpt") then raise (IllegalModToNewpkt(t1,t2))
      | _ -> ()
    in

 	  let check_legal_pkt_fields = function  
							| TField(varname, fld)
                (* newpkt: must be legal to modify *)
                when varname = newpkt -> 
      				   			if not (legal_field_to_modify fld) then
      	 					   		raise (IllegalFieldModification f)
              | TField(varname, fld) ->
                 (* any term: cannot compile if involves non-base fieldnames *)
                 if not (compilable_field_to_test fld) then
                    raise (NonTableField f)
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
    		check_legal_pkt_fields t1; (* not trying to set an unsettable field *)
    		check_legal_pkt_fields t2;
    		check_same_field_if_newpkt t1 t2; (* can't swap fields, etc. w/o controller *)
        check_not_same_pkt t1 t2;
        check_netcore_temp_limit_eq t1 t2;

      	| FAtom(modname, relname, tlargs) ->       		
      		(* new field must be legal for modification by openflow *)
      		iter check_legal_pkt_fields tlargs;
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

let can_compile_clause_to_fwd (cl: clause): bool =  
  let debug = true in 
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
    | NonTableField(_) -> if debug then printf "NonTableField\n%!"; false
    | IllegalModToNewpkt(_, _) -> if debug then printf "IllegalModToNewpkt\n%!"; false
    | IllegalEquality(_,_) -> if debug then printf "IllegalEquality\n%!"; false;;


(************ WEAKENING ************)

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
    (* Don't remove non-packet input tables. Those flag caller that the clause is not packet-triggered *)   
    | FAtom("", relname, [TVar(varstr)]) when (is_packet_in_table relname) ->  
      (varstr, FTrue)
    | _ -> ("", body);;    
    
(***************************************************************************************)

(* Used to pre-filter controller notifications as much as possible *)
let weaken_uncompilable_packet_triggered_clause (oldpkt: string) (cl: clause): clause =
  (* for now, naive solution: remove offensive literals outright. easy to prove correctness 
    of goal: result is a fmla that is implied by the real body. *)
    (* acc contains formula built so far, plus the variables seen *)
    let safeargs = (match cl.head with | FAtom(_, _, args) -> args | _ -> failwith "strip_incoming_atom") in
      if !global_verbose >= 3 then
        printf "   --- WEAKENING: Checking clause body in case need to weaken: %s\n%!" (string_of_formula cl.body);
      
      (* 'seen' keeps track of the terms used in relational lits so far *)
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
        | FEquals(TField(v, f), TField(v2, f2)) 
        | FNot(FEquals(TField(v, f), TField(v2, f2))) when v = v2 && f2 <> f -> 
          begin
            if !global_verbose >= 3 then 
              printf "Removing atom (pkt equality): %s\n%!" (string_of_formula lit);
            acc end            

          (* weaken if referring to a non-table packet field (like ARP fields) *)
        | FEquals(TField(_, fld), _) 
        | FNot(FEquals(TField(_, fld), _)) 
        | FEquals(_, TField(_, fld)) 
        | FNot(FEquals(_,TField(_, fld)))
          when not (compilable_field_to_test fld) -> 
          begin
            if !global_verbose >= 3 then 
              printf "Removing atom (not compilable; equality): %s\n%!" (string_of_formula lit);
            acc end            
        | FAtom(_,_,args) 
        | FNot(FAtom(_,_,args)) 
          when exists (function | TField(_, fld) -> not (compilable_field_to_test fld) | _ -> false) args -> 
          begin 
            if !global_verbose >= 3 then 
              printf "Removing atom (not compilable; atomic): %s\n%!" (string_of_formula lit);
            acc end                      

        (* If this atom involves an already-seen variable not in tlargs, remove it *)
        | FAtom (_, _, atomargs)  
        | FNot(FAtom (_, _, atomargs)) ->
          if length (list_intersection (subtract (filter not_is_oldpkt_field atomargs) safeargs) seen) > 0 then 
          begin
            (* removing this atom, so a fresh term shouldnt be remembered *)
            if !global_verbose >= 3 then
              printf "Removing atom (already seen): %s\n%!" (string_of_formula lit);
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

