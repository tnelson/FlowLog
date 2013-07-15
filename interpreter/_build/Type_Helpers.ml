open Flowlog_Types;;

(* Provides printing functions and conversion functions both for pretty printing and communication with XSB. *)
module Type_Helpers = struct

	let list_to_string (conversion : 'a -> string) (l : 'a list) : string = 
		let ans = List.fold_right (fun x acc -> (conversion x) ^ "," ^ acc) l "" in
		if ans = "" then ans else String.sub ans 0 (String.length ans - 1);;

	let notif_var_name (n : Types.notif_var) : string =
		match n with Types.Notif_var(_, str) -> str;;

	let notif_var_to_terms (n : Types.notif_var) : Types.term list =
		match n with Types.Notif_var(Types.Type(_, fields), _) ->
		List.map (fun field -> Types.Field_ref(n, field)) fields;;

	let term_to_string (t : Types.term) : string = 
		match t with
		| Types.Constant(c) -> c; 
		| Types.Variable(v) -> v;
		| Types.Field_ref(notif, field) -> (notif_var_name notif) ^ "_" ^ field;;

	let notif_var_to_string (n : Types.notif_var) : string = 
		list_to_string term_to_string (notif_var_to_terms n);;

	let notif_type_to_string (ntype : Types.notif_type) : string = 
		match ntype with Types.Type(name, fields) -> name ^ ": {" ^ (list_to_string (fun str -> str) fields) ^ "}";;

	let notif_val_to_string (n : Types.notif_val) : string =
		match n with Types.Notif_val(ntype, terms) -> (notif_type_to_string ntype) ^ " : " ^ (list_to_string term_to_string terms);;

	let blackbox_name (bb : Types.blackbox) =
		match bb with
		| Types.Internal_BB(name) -> name;
		| Types.External_BB(name, _, _) -> name;;

	let atom_to_string (a : Types.atom) : string =
		match a with
		| Types.Equals(t1, t2) -> (term_to_string t1) ^ " = " ^ (term_to_string t2);
		| Types.Apply(str, tl) -> str ^ "(" ^ (list_to_string term_to_string tl) ^ ")";
		| Types.Query(bb, name, tl) -> (blackbox_name bb) ^ "_" ^ name ^ "(" ^ (list_to_string term_to_string tl) ^ ")";
		| Types.Bool(b) -> string_of_bool b;;

	let literal_to_string (l : Types.literal) : string = 
		match l with
		| Types.Pos(a) -> atom_to_string a;
		| Types.Neg(a) -> "not(" ^ (atom_to_string a) ^ ")";;
	
	let get_atom (lit : Types.literal) : Types.atom =
		match lit with
		| Types.Pos(a) -> a;
		| Types.Neg(a) -> a;;

	let argument_to_string (arg : Types.argument) : string =
		match arg with
		| Types.Arg_notif(n) -> notif_var_to_string n;
		| Types.Arg_term(t) -> term_to_string t;;

	let arguments_to_terms (args : Types.argument list) : Types.term list =
		List.fold_right (fun arg acc ->
		match arg with
		| Types.Arg_term(t) -> t :: acc;
		| Types.Arg_notif(n) -> (notif_var_to_terms n) @ acc;) args [];;

	let rec drop (l : 'a list) (n : int) : 'a list = 
		if n <= 0 then l else
		match l with
		| [] -> [];
		| h :: t -> drop t (n - 1);;

	let rec take (l : 'a list) (n : int) : 'a list =
		if n <= 0 then [] else
		match l with
		| [] -> [];
		| h :: t -> h :: take t (n-1);;

	let terms_to_notif_val (ntype : Types.notif_type) (terms : Types.term list) : Types.notif_val =
		match ntype with Types.Type(_, fields) ->
		if (List.length terms = List.length fields) 
		then Types.Notif_val(ntype, terms) 
		else raise (Failure "Tried to create notification with wrong number of terms.");;

	let clause_name (cls : Types.clause) : string = 
		match cls with
		| Types.PlusClause(name,_, _) -> name;
		| Types.MinusClause(name, _, _) -> name;
		| Types.HelperClause(name, _, _) -> name;
		| Types.NotifClause(bb, _, _) -> blackbox_name bb;;

	let clause_arguments (cls : Types.clause) : Types.argument list = 
		match cls with
		| Types.PlusClause(_, args, _) -> args;
		| Types.MinusClause(_, args, _) -> args;
		| Types.HelperClause(_, args, _) -> args;
		| Types.NotifClause(_, args, _) -> args;;

	let clause_body (cls : Types.clause) : Types.literal list = 
		match cls with
		| Types.PlusClause(_, _, body) -> body;
		| Types.MinusClause(_, _, body) -> body;
		| Types.HelperClause(_, _, body) -> body;
		| Types.NotifClause(_, _, body) -> body;;

	let clause_to_string (cls : Types.clause) : string =
		let name, args, body = (clause_name cls, clause_arguments cls, clause_body cls) in
		if body = [] then name ^ "(" ^ (list_to_string argument_to_string args) ^ ") :- false"
		else name ^ "(" ^ (list_to_string argument_to_string args) ^ ") :- " ^
			(list_to_string literal_to_string body);;

	let relation_body (rel : Types.relation) : Types.clause list =
		match rel with
		| Types.PlusRelation(_, _, body) -> body;
		| Types.MinusRelation(_, _, body) -> body;
		| Types.HelperRelation(_, _, body) -> body;
		| Types.NotifRelation(_, _, body) -> body;;

end

module Parsing = struct
	(* raised on errors in parsing or converting from Syntax to Types. *)
	exception Parse_error of string;;

	(* These functions process a newly parsed program and make all relations have the module come before it. *)
	let process_notif_type_name (fn : string -> string) (nt : Syntax.notif_type) : Syntax.notif_type =
		match nt with Syntax.Type(name, fields) -> Syntax.Type(fn name, fields);;

	let process_notif_var_name (fn : string -> string) (nv : Syntax.notif_var) : Syntax.notif_var = 
		match nv with Syntax.Notif_var(type_name, var_name) -> Syntax.Notif_var(fn type_name, var_name);;

	let process_atom_name (fn : string -> string) (a : Syntax.atom) : Syntax.atom = 
		match a with
		| Syntax.Apply(name, tl) -> Syntax.Apply(fn name, tl);
		| _ -> a;;

	let process_literal_name (fn : string -> string) (lit : Syntax.literal) : Syntax.literal = 
		match lit with
		| Syntax.Pos(a) -> Syntax.Pos(process_atom_name fn a);
		| Syntax.Neg(a) -> Syntax.Neg(process_atom_name fn a);;

	let process_argument_name (fn : string -> string) (arg : Syntax.argument) : Syntax.argument =
		match arg with
		| Syntax.Arg_notif(nv) -> Syntax.Arg_notif(process_notif_var_name fn nv);
		| _ -> arg;;

	(* Notice that NotifClauses don't get their names changed. *)
	let process_clause_name (fn : string -> string) (cls : Syntax.clause) : Syntax.clause = 
		match cls with 
		|Syntax.PlusClause(name, args, body) -> Syntax.PlusClause(fn name, List.map (process_argument_name fn) args, List.map (process_literal_name fn) body);
		|Syntax.MinusClause(name, args, body) -> Syntax.MinusClause(fn name, List.map (process_argument_name fn) args, List.map (process_literal_name fn) body);
		|Syntax.HelperClause(name, args, body) -> Syntax.HelperClause(fn name, List.map (process_argument_name fn) args, List.map (process_literal_name fn) body);
		|Syntax.NotifClause(name, args, body) -> Syntax.NotifClause(name, List.map (process_argument_name fn) args, List.map (process_literal_name fn) body);;

	let process_name (name : string) (str : string) : string =
		if not (String.contains str '/') then str ^ "/" ^ name else str;;

	let make_Program (name : string) (modules : string list) (blackboxes : Syntax.blackbox list) (ntypes : Syntax.notif_type list) (clauses : Syntax.clause list) : Syntax.program =
		Syntax.Program(name, modules, blackboxes, List.map (process_notif_type_name (process_name name)) ntypes, List.map (process_clause_name (process_name name)) clauses)

	let rec remove_duplicates (l : 'a list) : 'a list =
		match l with
		[] -> [];
		| h :: t -> if List.mem h t then (remove_duplicates t) else h :: (remove_duplicates t);;

	let import (main : Syntax.program) (imports : Syntax.program list) : Syntax.program =
		List.fold_right (fun prgm acc -> 
		match acc with Syntax.Program(acc_name, acc_modules, acc_blackboxes, acc_ntypes, acc_clauses) ->
		match prgm with Syntax.Program(prgm_name, prgm_modules, prgm_blackboxes, prgm_ntypes, prgm_clauses) ->
		let modules = remove_duplicates (prgm_modules @ acc_modules) in
		let blackboxes = prgm_blackboxes @ acc_blackboxes in
		let ntypes = prgm_ntypes @ acc_ntypes in
		let clauses = (List.map (fun cls -> match cls with
			| Syntax.NotifClause(cls_name, cls_args, cls_body) -> Syntax.HelperClause(process_name prgm_name cls_name, cls_args, cls_body);
			| _ -> cls;) prgm_clauses) @ acc_clauses in
		Syntax.Program(acc_name, modules, blackboxes, ntypes, clauses)) imports main;;

	(* true if str1 begins with str2 *)
	let begins_with (str1 : string) (str2 : string) : bool = 
		if String.length str2 > String.length str1 then false else
		(String.sub str1 0 (String.length str2)) = str2;;

	let plus_name (name : string) : bool = begins_with name "+";;

	let minus_name (name : string) : bool = begins_with name "-";;

	let bb_name (name : string) : bool = begins_with name "bb";;

	let normal_name (name : string) : bool = not (plus_name name || minus_name name || bb_name name);;

	let make_External_BB (name : string) (ip : string) (port : int) : Syntax.blackbox =
		if bb_name name then Syntax.External_BB(name, ip, port) else
		raise (Parse_error ("external blackbox name " ^ name ^ " does not start with BB."));;

	let make_Internal_BB (name : string) : Syntax.blackbox = 
		if name = "forward" then Syntax.Internal_BB(name) else
		raise (Parse_error ("internal blackbox " ^ name ^ " is not supported."));;

	let make_Type (name : string) (fields : string list) : Syntax.notif_type = 
		if normal_name name && List.fold_right (fun str acc -> (normal_name str) && acc) fields true then
		Syntax.Type(name, fields) else
		raise (Parse_error ("type names and type fields cannot start with +, -, or bb. This is violated in type " ^ name));;

	let make_Plus_Minus_Clause (name : string) (args : Syntax.argument list) (body : Syntax.literal list) : Syntax.clause = 
		if plus_name name then Syntax.PlusClause(name, args, body) else
		if minus_name name then Syntax.MinusClause(name, args, body) else
		raise (Parse_error ("if a clause's arguments starts with a notification variable then it must be a plus or minus clause. This is violated by clause " ^ name));;

	let make_HelperClause (name : string) (args : Syntax.argument list) (body : Syntax.literal list) : Syntax.clause = 
		if normal_name name then Syntax.HelperClause(name, args, body) else
		raise (Parse_error ("if a clause has no notification arguments then it must be a helper clause and its name cannot start with +, -, or BB. This is violated in clause " ^ name));;

	let make_NotifClause (name : string) (args : Syntax.argument list) (body : Syntax.literal list) : Syntax.clause = 
		if bb_name name then Syntax.NotifClause(name, args, body) else
		raise (Parse_error ("if a clause has two arguments which are both notification variables then it must be a notification clause and its name must start with BB. This is violated in clause " ^ name));;

	let make_Arg_term (t : Syntax.term) : Syntax.argument =
		match t with
		| Syntax.Variable(_) -> Syntax.Arg_term(t);
		| _ -> raise (Parse_error ("argument terms must be variables."));;

	let make_Variable (str : string) : Syntax.term =
		if normal_name str then Syntax.Variable(str) else
		raise (Parse_error ("variable names cannot begin with +, -, or BB. This is violated by " ^ str));;

	let make_Arg_notif (nv : Syntax.notif_var) : Syntax.argument =
		Syntax.Arg_notif(nv);;

	let make_Notif_var (type_name : string) (var_name : string) : Syntax.notif_var =
		if normal_name var_name then Syntax.Notif_var(type_name, var_name) else
		raise (Parse_error ("notification variable names cannot begin with +, -, or BB. This is violated by " ^ var_name));;

	let make_Apply (rel_name : string) (tl : Syntax.term list) : Syntax.atom = Syntax.Apply(rel_name, tl);;

	let make_Query (bbname : string) (field_name : string) (tl : Syntax.term list) : Syntax.atom =
		if bb_name bbname then Syntax.Query(bbname, field_name, tl) else
		raise (Parse_error ("queries must be to external blackboxes, whose names begin with BB. This is violated by blackbox " ^ bbname));;

	let make_Constant_Variable (str : string) : Syntax.term =
		try let _ = int_of_string str in Syntax.Constant(str) with exn -> Syntax.Variable(str);;

	let make_Field_ref (var_name : string) (field_name : string) : Syntax.term =
		if normal_name var_name then Syntax.Field_ref(var_name, field_name) else
		raise (Parse_error ("notification variable names cannot begin with +, -, or BB. This is violated by " ^ var_name));;

end

module Conversion = struct
	(* Functions to turn a Syntax.program into a Types.program. *)

	(* These functions assume that there are no modules to be imported in prgm. *)
	let notif_type_convert (nt : Syntax.notif_type) : Types.notif_type = 
		match nt with Syntax.Type(name, field_names) -> Types.Type(name, field_names);;

	let notif_var_convert (prgm : Syntax.program) (cls : Syntax.clause) (nv : Syntax.notif_var) : Types.notif_var =
		match prgm with Syntax.Program(_, _, _, ntypes, _) ->
		match nv with Syntax.Notif_var(type_name, var_name) ->
		match List.filter (function Syntax.Type(n, _) -> type_name = n ) ntypes with
		| [] -> raise (Parsing.Parse_error ("notif_var " ^ var_name ^ " in clause " ^ (Syntax.clause_name cls) ^ " has an invalid type " ^ type_name));
		| t :: _ -> Types.Notif_var(notif_type_convert t, var_name);;

	let term_convert (prgm : Syntax.program) (cls : Syntax.clause) (t : Syntax.term) : Types.term = 
		match t with
		| Syntax.Constant(str) -> Types.Constant(str);
		| Syntax.Variable(str) -> Types.Variable(str);
		| Syntax.Field_ref(var_name, field_name) ->
		match List.filter (function 
			| Syntax.Arg_notif(Syntax.Notif_var(_, name)) -> name = var_name;
			| _ -> false;) (Syntax.clause_arguments cls) with
		| Syntax.Arg_notif(nv) :: _ -> Types.Field_ref((notif_var_convert prgm cls nv), field_name);
		| _ -> raise (Parsing.Parse_error ("the notification in " ^ (Syntax.term_to_string t) ^ " in clause " ^ (Syntax.clause_name cls) ^ " is not defined."));;

	let blackbox_convert (bb : Syntax.blackbox) : Types.blackbox =
		match bb with
		| Syntax.Internal_BB(n) -> Types.Internal_BB(n);
		| Syntax.External_BB(n, ip, port) -> Types.External_BB(n, ip, port);;

	let atom_convert (prgm : Syntax.program) (cls : Syntax.clause) (a : Syntax.atom) : Types.atom =
		match a with
		| Syntax.Equals(t1, t2) -> Types.Equals((term_convert prgm cls t1), (term_convert prgm cls t2));
		| Syntax.Apply(rel_name, terms) -> Types.Apply(rel_name , List.map (term_convert prgm cls) terms);
		| Syntax.Bool(b) -> Types.Bool(b);
		| Syntax.Query(bbname, rel_name, terms) ->
			match prgm with Syntax.Program(_, _, blackboxes, _, _) ->
			match List.filter (fun bb -> Syntax.blackbox_name bb = bbname) blackboxes with
			| [] -> raise (Parsing.Parse_error ("black box " ^ bbname ^ " in clause " ^ (Syntax.clause_name cls) ^ " does not exist."));
			| bb :: _ -> Types.Query(blackbox_convert bb, rel_name, List.map (term_convert prgm cls) terms);;

	let literal_convert (prgm : Syntax.program) (cls : Syntax.clause) (l : Syntax.literal) : Types.literal =
		match l with
		| Syntax.Pos(a) -> Types.Pos(atom_convert prgm cls a);
		| Syntax.Neg(a) -> Types.Neg(atom_convert prgm cls a);;

	let argument_convert (prgm : Syntax.program) (cls : Syntax.clause) (arg : Syntax.argument) : Types.argument = 
		match arg with
		| Syntax.Arg_notif(nv) -> Types.Arg_notif(notif_var_convert prgm cls nv);
		| Syntax.Arg_term(t) ->
			match t with
			| Syntax.Constant(_) -> raise (Parsing.Parse_error ("argument " ^ (Syntax.argument_name arg) ^ " in clause " ^ (Syntax.clause_name cls) ^ " is a constant."));
			| Syntax.Variable(str) -> Types.Arg_term(Types.Variable(str));
			| Syntax.Field_ref(_, _) -> raise (Parsing.Parse_error ("argument " ^ (Syntax.argument_name arg) ^ " in clause " ^ (Syntax.clause_name cls) ^ " is a field ref."));;

	let clause_convert (prgm : Syntax.program) (cls : Syntax.clause) : Types.clause = 
		match cls with
		| Syntax.PlusClause(name, args, body) -> Types.PlusClause(name, List.map (argument_convert prgm cls) args, List.map (literal_convert prgm cls) body);
		| Syntax.MinusClause(name, args, body) -> Types.MinusClause(name, List.map (argument_convert prgm cls) args, List.map (literal_convert prgm cls) body);
		| Syntax.HelperClause(name, args, body) -> Types.HelperClause(name, List.map (argument_convert prgm cls) args, List.map (literal_convert prgm cls) body);
		| Syntax.NotifClause(bbname, args, body) ->
			match prgm with Syntax.Program(_, _, blackboxes, _, _) ->
			match List.filter (fun bbox -> Syntax.blackbox_name bbox = bbname) blackboxes with
			| [] -> raise (Parsing.Parse_error ("black box " ^ bbname ^ " in clause " ^ (Syntax.clause_name cls) ^ " does not exist."));
			| bb :: _ -> Types.NotifClause(blackbox_convert bb, List.map (argument_convert prgm cls) args, List.map (literal_convert prgm cls) body);;

	let rec drop (l : 'a list) (n : int) : 'a list = 
		if n <= 0 then l else
		match l with
		| [] -> [];
		| h :: t -> drop t (n - 1);;

	let clause_key (cls : Types.clause) : string =
		match cls with
		| Types.PlusClause(name, args, _) -> "plus " ^ name ^ " " ^ (Type_Helpers.list_to_string Type_Helpers.argument_to_string args);
		| Types.MinusClause(name, args, _) -> "minus " ^ name ^ " " ^ (Type_Helpers.list_to_string Type_Helpers.argument_to_string args);
		| Types.HelperClause(name, args, _) -> "helper " ^ name ^ " " ^ (Type_Helpers.list_to_string Type_Helpers.argument_to_string args);
		| Types.NotifClause(bb, args, _) -> "notif" ^ (Type_Helpers.blackbox_name bb) ^ " " ^ (Type_Helpers.list_to_string Type_Helpers.argument_to_string args);;

	let helper_relation_key (cls : Types.clause) : string =
		match cls with
		| Types.PlusClause(name, args, _) -> clause_key (Types.HelperClause(name, drop args 1, []));
		| Types.MinusClause(name, args, _) -> clause_key (Types.HelperClause(name, drop args 1, []));
		| _ -> raise (Failure "only plus and minus clauses can be passed to implicit_relation_key");;

	let make_relations (clauses : Types.clause list) : Types.relation list =
		let tbl = Hashtbl.create (List.length clauses) in
		let _ = List.iter (fun cls -> match cls with
		| Types.PlusClause(name, args, _) ->
			let key = clause_key cls in
			let _ = try (match Hashtbl.find tbl key with 
				| Types.PlusRelation(name, args, clauses) -> Hashtbl.replace tbl key (Types.PlusRelation(name, args, cls :: clauses));
				| _ -> ();)
			with Not_found -> Hashtbl.add tbl key (Types.PlusRelation(name, args, [cls])) in
			let helper_key = helper_relation_key cls in
			if not (Hashtbl.mem tbl helper_key) then Hashtbl.add tbl helper_key (Types.HelperRelation(name, drop args 1, []));
		| Types.MinusClause(name, args, _) ->
			let key = clause_key cls in
			let _ = try (match Hashtbl.find tbl key with 
				| Types.MinusRelation(name, args, clauses) -> Hashtbl.replace tbl key (Types.MinusRelation(name, args, cls :: clauses));
				| _ -> ();)
			with Not_found -> Hashtbl.add tbl key (Types.PlusRelation(name, args, [cls])) in
			let helper_key = helper_relation_key cls in
			if not (Hashtbl.mem tbl helper_key) then Hashtbl.add tbl helper_key (Types.HelperRelation(name, drop args 1, []));
		| Types.HelperClause(name, args, _) ->
			(let key = clause_key cls in
			try (match Hashtbl.find tbl key with 
				| Types.HelperRelation(name, args, clauses) -> Hashtbl.replace tbl key (Types.HelperRelation(name, args, cls :: clauses));
				| _ -> ();)
			with Not_found -> Hashtbl.add tbl key (Types.HelperRelation(name, args, [cls])););
		| Types.NotifClause(bb, args, _) ->
			let key = clause_key cls in
			try (match Hashtbl.find tbl key with 
				| Types.NotifRelation(bb, args, clauses) -> Hashtbl.replace tbl key (Types.NotifRelation(bb, args, cls :: clauses));
				| _ -> ();)
			with Not_found -> Hashtbl.add tbl key (Types.NotifRelation(bb, args, [cls]));) clauses in
		Hashtbl.fold (fun key_str rel acc -> rel :: acc) tbl [];;
	
	let program_convert (prgm : Syntax.program) : Types.program = 
		match prgm with Syntax.Program(name, _, _, _, clauses) ->
		Types.Program(name, make_relations (List.map (clause_convert prgm) clauses));;

end