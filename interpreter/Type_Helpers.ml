open Types;;

let debug = true;;

(* Provides printing functions and conversion functions both for pretty printing and communication with XSB. *)
module Type_Helpers = struct

	(* True if string str1 ends with string str2 *)
	let ends_with (str1 : string) (str2 : string) : bool = 
		if String.length str2 > String.length str1
		then false
		else (String.sub str1 ((String.length str1) - (String.length str2)) (String.length str2)) = str2;;

	let list_to_string (conversion : 'a -> string) (l : 'a list) : string = 
		let ans = List.fold_right (fun x acc -> (conversion x) ^ "," ^ acc) l "" in
		if ans = "" then ans else String.sub ans 0 (String.length ans - 1);;

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
		| Types.Constant(values, _) -> list_to_string (fun str -> str) values; 
		| Types.Variable(name, Types.Type(_, fields)) -> list_to_string (fun field -> name ^ "_" ^ field) fields;
		| Types.Field_ref(name, field) -> name ^ "_" ^ field;
		| Types.Variable(name, Types.Term_defer(str)) -> failwith ("not a valid term: "^name^" with defer: "^str);;
		(*| _ -> failwith "Not a valid term";;*)

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


	let clause_to_string (cls : Types.clause) : string = 
		match cls with Types.Clause(s, body) ->
		match body with
		| [] -> (signature_to_string s) ^ " :- false";
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

	let process_clause_list (prgm : Types.program) (clauses : Types.clause list) : Types.clause list =
		let equiv = fun cls1 cls2 -> Type_Helpers.clause_signature cls1 = Type_Helpers.clause_signature cls2 in
		let fixed_clauses = List.map (process_clause_names prgm) clauses in
		(List.fold_right (fun cls acc -> match cls with Types.Clause(Types.Signature(cls_type, module_name, name, args), _) -> match cls_type with
			| Types.Plus -> let new_clause = Types.Clause(Types.Signature(Types.Helper, module_name, name, drop args 1), []) in
				if list_contains fixed_clauses equiv new_clause then acc else new_clause :: acc;
			| Types.Minus -> let new_clause = Types.Clause(Types.Signature(Types.Helper, module_name, name, drop args 1), []) in
				if list_contains fixed_clauses equiv new_clause then acc else new_clause :: acc;
			| _ -> acc;) fixed_clauses []) @ fixed_clauses;;


	let process_program_names (prgm : Types.program) : Types.program =
		match prgm with Types.Program(name, modules, blackboxes, types, clauses) ->
		Types.Program(name, modules, blackboxes, types, process_clause_list prgm clauses);;

	let rec remove_duplicates (l : 'a list) : 'a list =
		match l with
		[] -> [];
		| h :: t -> if List.mem h t then (remove_duplicates t) else h :: (remove_duplicates t);;

	let import (main : Types.program) (imports : Types.program list) : Types.program =
		List.fold_right (fun prgm acc -> 
		match acc with Types.Program(acc_name, acc_modules, acc_blackboxes, acc_types, acc_clauses) ->
		match prgm with Types.Program(prgm_name, prgm_modules, prgm_blackboxes, prgm_types, prgm_clauses) ->
		let modules = remove_duplicates (prgm_modules @ acc_modules) in
		let blackboxes = prgm_blackboxes @ acc_blackboxes in
		let types = prgm_types @ acc_types in
		let clauses = (List.map (fun cls -> match cls with
			| Types.Clause(Types.Signature(Types.Action, module_name, name, args), body) -> Types.Clause(Types.Signature(Types.Helper, module_name, name, args), body);
			| _ -> cls;) prgm_clauses) @ acc_clauses in
		Types.Program(acc_name, modules, blackboxes, types, clauses)) imports main;;

	let process_term_type (prgm : Types.program) (s : Types.signature) (var_name : string) (ttype : Types.term_type) : Types.term_type =		
		match prgm with Types.Program(_, _, _, types, _) ->
		match ttype with
		| Types.Type(tname, _) -> ttype;
		| Types.Term_defer("") -> (match s with Types.Signature(_, _, _, args) ->			
			match List.filter (function Types.Variable(name,_) -> name = var_name; | _ -> false;) args with
			| [] -> Types.raw_type;
			| Types.Variable(_, t) :: _ -> (match t with | Types.Term_defer(_) -> Types.raw_type; | _ -> t;);
			| _ -> raise (Failure "cannot have a constant in a signature in a program")); 
		| Types.Term_defer(type_name) ->		    
			(match List.filter (function | Types.Type(name, fields) -> name = type_name; | _ -> false;) types with
			| [] -> raise (Failure ("type " ^ type_name ^ " was not declared"));
			| h :: _ -> h;);;

	let process_term (prgm : Types.program) (s : Types.signature) (t : Types.term) : Types.term =
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
	
	let process_clause (prgm : Types.program) (cls : Types.clause) : Types.clause =
		match cls with Types.Clause(s, al) -> let fixed_sig = process_signature prgm s in
		Types.Clause(fixed_sig, List.map (process_atom prgm fixed_sig) al);;

	let process_program_types (prgm : Types.program) : Types.program =
		match prgm with Types.Program(name, modules, blackboxes, types, clauses) ->
		Types.Program(name, modules, blackboxes, types, List.map (process_clause prgm) clauses);;


end