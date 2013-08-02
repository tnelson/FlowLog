open Types;;

let debug = true;;

(* Sets of terms, rather than lists *)   
module TermSet = Set.Make( 
  struct
    let compare = Pervasives.compare
    type t = Types.term
  end )

(* Provides printing functions and conversion functions both for pretty printing and communication with XSB. *)
module Type_Helpers = struct

	(* True if string str1 ends with string str2 *)
	let ends_with (str1 : string) (str2 : string) : bool = 
		if String.length str2 > String.length str1
		then false
		else (String.sub str1 ((String.length str1) - (String.length str2)) (String.length str2)) = str2;;
    
	let list_to_string (conversion : 'a -> string) (l : 'a list) : string = 
		String.concat "," (List.filter (fun s -> s <> "") (List.map conversion l));;

	let blackbox_name (bb : Types.blackbox) : string =
		match bb with Types.BlackBox(name, _) -> name;;

	let term_type_name (t : Types.term_type) : string =
		match t with
		| Types.Type(name, _) -> name;
		| Types.Term_defer(name) -> "term_defer_" ^ name;;

	let type_of_term (t : Types.term) : Types.term_type =
		match t with
		| Types.Constant(_, ttype) -> ttype;
		| Types.Variable(_, ttype) -> ttype;
		| Types.Field_ref(_, _) -> Types.raw_type;;

	let term_to_string (t : Types.term) : string = 
		match t with
		| Types.Constant(values, Types.Type(n, _)) -> (list_to_string (fun str -> str) values) ; 		
		| Types.Variable(name, Types.Type(_, fields)) -> list_to_string (fun field -> name ^ "_" ^ field) fields;
		| Types.Field_ref(name, field) -> name ^ "_" ^ field;
		| Types.Variable(name, Types.Term_defer(str)) -> name^": DEFER: "^str(*failwith ("not a valid variable: "^name^" with defer: "^str);*)
		| Types.Constant(values, Types.Term_defer(str)) -> (list_to_string (fun str-> str) values)^": DEFER: "^str (*failwith ("not a valid constant with defer: "^str);;  *)

	let bool_to_string (b : bool) : string =
		match b with
		| true -> "";
		| false -> "not ";;

    (* TODO: note that using PLUS etc. prevents treating +learned as a helper, which we could do before *)

	let clause_type_to_string (cls_type : Types.clause_type) : string =
		match cls_type with
		| Types.Plus -> "plus";
		| Types.Minus -> "minus";
		| Types.Helper -> "helper";
		| Types.Action -> "action";;

	let signature_name (s : Types.signature) : string = 
		match s with Types.Signature(cls_type, module_name, name, args) ->
		let name_list = [clause_type_to_string cls_type; module_name ^ "/" ^ name] @ (List.map (fun t -> term_type_name (type_of_term t)) args) in
		List.fold_right (fun str acc -> str ^ "_" ^ acc) name_list "";;

	let clause_signature (cls : Types.clause) : string =
		match cls with Types.Clause(s, _) -> signature_name s;;

	let signature_to_string (s : Types.signature) : string =
		match s with Types.Signature(_, _, _, args) ->
		(signature_name s) ^ "(" ^ (list_to_string term_to_string args) ^ ")";;

	(* note that the name of a relation includes the prefix (before the dot) *)
	let atom_to_string (a : Types.atom) : string =
		match a with
		| Types.Equals(sgn, t1, t2) -> (bool_to_string sgn) ^ (term_to_string t1) ^ " = " ^ (term_to_string t2);
		| Types.Apply(sgn, module_name, name, tl) ->  (bool_to_string sgn) ^ (signature_to_string (Types.Signature(Types.Helper, module_name, name, tl)));		
		| Types.Bool(b) -> string_of_bool b;;

    (* the empty CONJUNCTION is true, not false *)
	let clause_to_string (cls : Types.clause) : string = 
		match cls with Types.Clause(s, body) ->
		match body with
		| [] -> (signature_to_string s) ^ " :- true";
		| _ -> (signature_to_string s) ^ " :- " ^ (list_to_string atom_to_string body);;


	let get_blackbox (prgm : Types.program) (name : string) : Types.blackbox =	    
		match prgm with Types.Program(_, _, blackboxes, _, _) ->
		match List.filter (function Types.BlackBox(n, _) -> n = name) blackboxes with
		| [] -> Printf.printf "Unknown blackbox %s\n%!" name;
		        raise (Failure ("No such black box as " ^ name));
		| h :: _ -> h;;

end

module Parse_Helpers = struct
(* need post_process prgm and import prgm prgm list*)

	let process_atom_names (prgm : Types.program) (a : Types.atom) : Types.atom =
		match prgm with Types.Program(prgm_name, _, _, _, _) ->
		match a with
		| Types.Apply(b, "", name, tl) -> Types.Apply(b, prgm_name, name, tl);
		| Types.Apply(b, module_name, name, tl) -> Types.Apply(b, module_name, name, tl);
		| _ -> a;;

	let process_signature_names (prgm : Types.program) (s : Types.signature) : Types.signature =
		match prgm with Types.Program(prgm_name, _, _, _, _) ->
		match s with 
		| Types.Signature(cls_type, "", name, tl) -> Types.Signature(cls_type, prgm_name, name, tl);
		| Types.Signature(cls_type, module_name, name, tl) -> Types.Signature(cls_type, module_name, name, tl);;
	
	let process_clause_names (prgm : Types.program) (cls : Types.clause) : Types.clause =
		match cls with Types.Clause(s, al) -> Types.Clause(process_signature_names prgm s, List.map (process_atom_names prgm) al);;

	let rec list_contains (l : 'a list) (equiv : 'a -> 'a -> bool) (item : 'a) =
		match l with
		| [] -> false;
		| h :: t -> if equiv h item then true else list_contains t equiv item;;

	let rec drop (l : 'a list) (n : int) : 'a list = 
		if n <= 0 then l else
		match l with
		| [] -> [];
		| h :: t -> drop t (n - 1);;

  (* Need to tell XSB about state relations as they appear. If we 
     assert((r(X) :- p(X))). and then do r(X). we get an error. *)

	let process_clause_list (prgm : Types.program) (clauses : Types.clause list) : Types.clause list =
		let equiv = fun cls1 cls2 -> Type_Helpers.clause_signature cls1 = Type_Helpers.clause_signature cls2 in
		let fixed_clauses = List.map (process_clause_names prgm) clauses in
		(List.fold_right (fun cls acc -> match cls with Types.Clause(Types.Signature(cls_type, module_name, name, args), _) -> match cls_type with
			| Types.Plus -> let new_clause = Types.Clause(Types.Signature(Types.Helper, module_name, name, drop args 1), [Types.Bool(false)]) in
				if list_contains fixed_clauses equiv new_clause then acc else new_clause :: acc;
			| Types.Minus -> let new_clause = Types.Clause(Types.Signature(Types.Helper, module_name, name, drop args 1), [Types.Bool(false)]) in
				if list_contains fixed_clauses equiv new_clause then acc else new_clause :: acc;
			| _ -> acc;) fixed_clauses []) @ fixed_clauses;;


	let process_program_names (prgm : Types.program) : Types.program =
		match prgm with Types.Program(name, modules, blackboxes, types, clauses) ->
		Types.Program(name, modules, blackboxes, types, process_clause_list prgm clauses);;

	let rec remove_duplicates (l : 'a list) : 'a list =
		match l with
		| [] -> [];
		| h :: t -> if List.mem h t then (remove_duplicates t) else h :: (remove_duplicates t);;

	let rec fail_if_duplicates (l : 'a list) (a_to_str : 'a -> string) : unit =
		match l with
		| [] -> ();
		| h :: t -> (match List.filter (fun a -> a_to_str a = a_to_str h) t with
			| [] -> fail_if_duplicates t a_to_str;
			| _ -> raise (Failure ("multiple definitions of " ^ (a_to_str h) ^ ".")););;



	let import (main : Types.program) (imports : Types.program list) : Types.program =
		List.fold_right (fun prgm acc -> 
		match acc with Types.Program(acc_name, acc_modules, acc_blackboxes, acc_types, acc_clauses) ->
		match prgm with Types.Program(prgm_name, prgm_modules, prgm_blackboxes, prgm_types, prgm_clauses) ->
		let modules = remove_duplicates (prgm_modules @ acc_modules) in
		let blackboxes = prgm_blackboxes @ acc_blackboxes in
		let types = prgm_types @ acc_types in
		fail_if_duplicates blackboxes Type_Helpers.blackbox_name;
		fail_if_duplicates types Type_Helpers.term_type_name;
		let clauses = (List.map (fun cls -> match cls with
			| Types.Clause(Types.Signature(Types.Action, module_name, name, args), body) -> Types.Clause(Types.Signature(Types.Helper, module_name, name, args), body);
			| _ -> cls;) prgm_clauses) @ acc_clauses in
		Types.Program(acc_name, modules, blackboxes, types, clauses)) imports main;;


	let process_term_type (prgm : Types.program) (s : Types.signature) (var_name : string) (ttype : Types.term_type) : Types.term_type =		
		match prgm with Types.Program(_, _, _, types, _) ->
		match ttype with
		| Types.Type(tname, _) -> ttype;
		| Types.Term_defer("") -> 		  
		  (match s with Types.Signature(_, _, _, args) ->			
		 	match List.filter (function Types.Variable(name,_) -> name = var_name; | _ -> false;) args with
			| [] -> Types.raw_type;
			| Types.Variable(_, t) :: _ -> (match t with | Types.Term_defer(_) -> Types.raw_type; | _ -> t;);
			| _ -> raise (Failure "cannot have a constant in a signature in a program")); 
		| Types.Term_defer(type_name) ->		    	        
			(match List.filter (function | Types.Type(name, fields) -> name = type_name; | _ -> false;) types with
			| [] -> raise (Failure ("type " ^ type_name ^ " was not declared"));
			| h :: _ -> h;);;

	let process_term (prgm : Types.program) (s : Types.signature) (t : Types.term) : Types.term =
	    (*if debug then Printf.printf "Processing term: %s\n%!" (Type_Helpers.term_to_string t);*)
		match t with
		| Types.Constant(sl, ttype) -> Types.Constant(sl, process_term_type prgm s "" ttype);
		| Types.Variable(vn, ttype) -> Types.Variable(vn, process_term_type prgm s vn ttype);
		| _ -> t;;

	let process_atom (prgm : Types.program) (s : Types.signature) (a : Types.atom) : Types.atom =
	   (* Printf.printf "   *** pa: %s\n%!" (Type_Helpers.atom_to_string a); *)
		match prgm with Types.Program(prgm_name, _, _, _, _) ->
		match a with
		| Types.Equals(b, t1, t2) -> Types.Equals(b, process_term prgm s t1, process_term prgm s t2);
		| Types.Apply(b, module_name, name, tl) -> Types.Apply(b, module_name, name, List.map (process_term prgm s) tl);		
		| _ -> a;;

	let process_signature (prgm : Types.program) (s : Types.signature) : Types.signature =
		match prgm with Types.Program(prgm_name, _, _, _, _) ->
		match s with 
		| Types.Signature(cls_type, module_name, name, tl) -> Types.Signature(cls_type, module_name, name, List.map (process_term prgm s) tl);;

     (* expand out notif. vars into field refs *)
    let expand_var (arg: Types.term): Types.term list =
      match arg with 
      | Types.Constant(_, _) -> [arg];
      | Types.Variable(vname, Types.Type("raw", ["VALUE"])) -> 
        [arg] (* don't expand out raw variables! why couldn't i just stick Types.raw_type here? *)
      | Types.Variable(vname, Types.Type(_,fields)) -> 
        List.map (fun fld -> Types.Field_ref(vname, fld)) fields
      | _ -> failwith "expand_var---unexpected term type";;

    let prep_for_non_helper args = 
      (List.fold_right (fun arg accum -> (expand_var arg) @ accum) (List.tl args) [],
       List.fold_right (fun aterm sofar -> TermSet.add aterm sofar)
                       (expand_var (List.hd args))
                       TermSet.empty);;         

    let prep_for_true_helper args = 
      (List.fold_right (fun arg accum -> arg :: accum) args [], 
	   TermSet.empty);;       

     (* First component: needs constraint. Second component: treat as constant (triggers) *)
	let flatten_terms_to_constrain (signat: Types.signature): Types.term list * TermSet.t =
	  match signat with 
	    (* plus, action -- leave the trigger free to be unconstrained 
	       but everything else needs to be constrained. *)
        (* same for minus, even though it might seem otherwise. minus is still a relation
            that needs to be evaluated by XSB. *)
        | Types.Signature(Types.Plus, _, _, args)  
        | Types.Signature(Types.Minus, _, _, args) 
        | Types.Signature(Types.Action, _, _, args) ->  prep_for_non_helper args;                  

         (* helper: everything needs to be constrained, unless the first arg is non-raw
            (in which case this is an action/plus/minus that got imported and helperized. *)         
        | Types.Signature(Types.Helper, _, _, args) -> 
            match (List.hd args) with 
            | Types.Variable(_, Types.Type("raw", ["VALUE"])) -> prep_for_true_helper args;
            | Types.Variable(_, _) -> prep_for_non_helper args;
            | _ -> prep_for_true_helper args;;

     let constrain_term (signat : Types.signature) (to_constrain: Types.term) : Types.atom =	       
		match signat with 
        | Types.Signature(ctype, modname, relname, args) -> 
          if relname = "forward" then 	(* If forward, default to same in head. *)   	       
            match to_constrain with 
            | Types.Field_ref(_, fld) ->
              (match List.hd args with 
              | Types.Variable(vname, _) ->
                  Types.Equals(true, to_constrain, Types.Field_ref(vname, fld))
              | _ -> failwith "constrain_term on forward: args error. expected first arg to be variables")            
            | _ -> failwith "constrain_term on forward: to_constrain error. Expected all needed constraints to be Field_refs"
      	  else if relname = "emit" then  (* If emit, default to 0. *)  
      	    Types.Equals(true, to_constrain, Types.Constant(["0"], Types.raw_type))  	
      	  else 
      	    let msg = ("Unconstrained term "^(Type_Helpers.term_to_string to_constrain)^" in clause with signature "^Type_Helpers.signature_to_string signat) in 
      	    Printf.printf "%s\n%!" msg;
      	    raise (Failure msg);;

    let is_constant_term t =
      match t with 
      | Types.Constant(_,_) -> true
      | _ -> false;;

    let find_constrained_terms_single (atom: Types.atom) (accum: TermSet.t): TermSet.t =
      if debug then Printf.printf "-- FCTS %s\n%!" (Type_Helpers.atom_to_string atom);
      (* For action/plus/minus clauses, the accum must be pre-populated with the trigger fields 
          (or else this block will just return accum) *)
      match atom with
        | Types.Equals(sign, t1, t2) -> 
          if not sign then accum
          else if is_constant_term t1 then TermSet.add t2 accum
          else if is_constant_term t2 then TermSet.add t1 accum
          else if TermSet.mem t1 accum then TermSet.add t2 accum
          else if TermSet.mem t2 accum then TermSet.add t1 accum 
          else accum
		| Types.Apply(sign, _, _, tl) -> 
          if not sign then accum
          else List.fold_right (fun aterm sofar -> TermSet.add aterm sofar) tl accum
		| Types.Bool(b) -> accum;;

    (* Iterate to fixpoint to catch cross-constraints. E.g. x = 5, y = x. *)
    let rec find_constrained_terms (atomlst: Types.atom list) (accum: TermSet.t): TermSet.t =      
      let result = List.fold_right find_constrained_terms_single atomlst accum in
        if debug then 
            Printf.printf "Iterating find_constrained_terms. Result=[%s]\n%!"
            (Type_Helpers.list_to_string Type_Helpers.term_to_string (TermSet.elements result));
        if TermSet.equal result accum then result
        else find_constrained_terms atomlst result;;

	(* If a clause doesn't constrain everything in the head, do something. 
	   For now, "something" is a kludge. TODO. *)
	(* TODO: "otherwise same" syntax. this is too ad-hoc *)
	let check_complete_atoms (signat: Types.signature) (atomlst : Types.atom list): Types.atom list =	
		if debug then Printf.printf "[Preprocessing] In check_complete_atoms. List was: [%s]\n%!" (Type_Helpers.list_to_string Type_Helpers.atom_to_string atomlst);	
		match atomlst with 
		| [Types.Bool(false)] -> atomlst;
		| _ -> 
		let all_flattened_split = flatten_terms_to_constrain signat in
		match all_flattened_split with (all_flat_to_constrain_terms, to_ignore_terms_set) -> 
		let constrained_terms = find_constrained_terms atomlst to_ignore_terms_set in
		let must_constrain = List.filter (fun t -> not (TermSet.mem t constrained_terms)) 
		                                 all_flat_to_constrain_terms in    
		  if debug then 
		    Printf.printf "[Preprocessing] MUST CONSTRAIN: [%s]\n%!"   
		                 (Type_Helpers.list_to_string Type_Helpers.term_to_string must_constrain); 
      	  let newatoms: Types.atom list = List.map (constrain_term signat) must_constrain in
      	    newatoms @ atomlst;;

	let process_clause (prgm : Types.program) (cls : Types.clause) : Types.clause =
		match cls with Types.Clause(signat, atomlst) -> 
		  let fixed_sig = process_signature prgm signat in
		  let processed_atoms = List.map (process_atom prgm fixed_sig) atomlst in		  
		  (*if debug then 
		    Printf.printf "[Preprocessing] Processed clause. Fixed signature: %s\n%!" (Type_Helpers.signature_to_string fixed_sig);
		    Printf.printf "[Preprocessing] Atoms processed. They are now:\n [%s]\n%!" 
		      (Type_Helpers.list_to_string Type_Helpers.atom_to_string processed_atoms);*)
		  let completed_atoms = check_complete_atoms fixed_sig processed_atoms in
		  Types.Clause(fixed_sig, completed_atoms);;

	let process_program_types (prgm : Types.program) : Types.program =
		match prgm with Types.Program(name, modules, blackboxes, types, clauses) ->		
		Types.Program(name, modules, blackboxes, types, List.map (process_clause prgm) clauses);;


end