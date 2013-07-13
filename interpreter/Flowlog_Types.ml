open Unix;;
open Xsb;;
open Packet;;
open OxPlatform;;
open OpenFlow0x01_Core;;

let debug = true;;

(* Defines the basic syntax types for the Flowlog interpreter. *)
(* File format:
import module1.
import module2.
...
import moduleN.

blackbox name_1 @ ip_1, port_1. (* we're enforcing that black box names start with bb and other names cannot. *)
blackbox name_2 @ ip_2, port_2.
...
blackbox name_L @ ip_L, port_L.

module module_name: (* module names and all other variables are case insensitive *)

type type_name_1 = { name_1,1, name_1,2, ..., name_1,M }.
type type_name_2 = { name_2,1, name_2,2, ..., name_2,M }.
...
type type_name_K = { name_K,1, name_K,2, ..., name_K,M }.

clause_name(arg_1 : type_name_1, ..., arg_n) :-
line_1,
line_2,
...
line_J.

...

During parsing, relation name rel_name in module module_name (which in flowlog is module_name.rel_name) is rewritten module_name/rel_name (all turned to lower case).
*)
module Syntax = struct
	(* type name, field names *)
	type notif_type = Type of string * string list;;
	(* type of black boxes. name, ip, port. *)
	type blackbox = Internal_BB of string | External_BB of string * string * int;;
	(* type name, variable name *)
	type notif_var = Notif_var of string * string;;
	(* constants and variables or a field of a value (like pkt.locPt) *)
	type term = Constant of string | Variable of string | Field_ref of string * string;;
	(* things like A = B or R(A, B, C) *)
	type atom = Equals of term * term | Apply of string * term list | Query of string * string * term list | Bool of bool;;
	(* atoms and negations of atoms *)
	type literal = Pos of atom | Neg of atom;;
	(* argument to a clause is either a notif_var or a term
	This contains the local decl of the var, including its type, in the head *)
	type argument = Arg_notif of notif_var | Arg_term of term;;
	(* name, arguments, body *)
	type clause = 
		(* rest of name, args, body*)
		PlusClause of string * argument list * literal list |
		(* rest of name, args, body *)
		MinusClause of string * argument list * literal list |
		(* name, args, body *)
		HelperClause of string * argument list * literal list |
		(* name, args (only 2 and both are Arg_notif), body *)
		NotifClause of string * argument list * literal list;;

	(* name, module names to be imported, black boxes, notification types, clauses *)	
	type program = Program of string * string list * blackbox list * notif_type list * clause list;;

	(* Some helper and printing functions *)
	let term_to_string (t : term) : string = 
		match t with
		| Constant(c) -> c; 
		| Variable(v) -> v;
		| Field_ref(notif_name, field_name) -> notif_name ^ "." ^ field_name;;

	let clause_name (cls : clause) : string = 
		match cls with
		| PlusClause(name,_, _) -> name;
		| MinusClause(name, _, _) -> name;
		| HelperClause(name, _, _) -> name;
		| NotifClause(name, _, _) -> name;;

	let clause_arguments (cls : clause) : argument list = 
		match cls with
		| PlusClause(_, args, _) -> args;
		| MinusClause(_, args, _) -> args;
		| HelperClause(_, args, _) -> args;
		| NotifClause(_, args, _) -> args;;

	let clause_body (cls : clause) : literal list = 
		match cls with
		| PlusClause(_, _, body) -> body;
		| MinusClause(_, _, body) -> body;
		| HelperClause(_, _, body) -> body;
		| NotifClause(_, _, body) -> body;;

	let blackbox_name (bb : blackbox) : string =
		match bb with
		| Internal_BB(name) -> name;
		| External_BB(name, _, _) -> name;;

	let argument_name (arg : argument) : string =
		match arg with
		| Arg_notif(Notif_var(_, var_name)) -> var_name;
		| Arg_term(t) -> term_to_string t;;

	let program_name (prgm : program) : string =
		match prgm with Program(name, _, _, _) -> name;;

	(* These functions process a newly parsed program and make all relations have the module come before it. *)
	let process_notif_type_name (fn : string -> string) (nt : notif_type) : notif_type =
		match nt with Notif_type(name, fields) -> Notif_type(fn name, fields);;

	let process_notif_var_name (fn : string -> string) (nv : notif_var) : notif_var = 
		match nv with Notif_var(type_name, var_name) -> Notif_var(fn type_name, var_name);;

	let process_atom_name (fn : string -> string) (a : atom) : atom = 
		match a with
		| Apply(name, tl) -> Apply(fn name, tl);
		| _ -> a;;

	let process_literal_name (fn : string -> string) (lit : literal) : literal = 
		match lit with
		| Pos(a) -> Pos(process_atom_name fn a);
		| Neg(a) -> Neg(process_atom_name fn a);;

	let process_argument_name (fn : string -> string) (arg : argument) : argument =
		match arg with
		| Arg_notif(nv) -> Arg_notif(process_notif_var_name fn nv);
		| _ -> arg;;

	(* Notice that NotifClauses don't get their names changed. *)
	let process_clause_name (fn : string -> string) (cls : clause) : clause = 
		match cls with 
		|PlusClause(name, args, body) -> PlusClause(fn name, List.map (process_argument_name fn) args, List.map (process_literal_name fn) body);
		|MinusClause(name, args, body) -> MinusClause(fn name, List.map (process_argument_name fn) args, List.map (process_literal_name fn) body);
		|HelperClause(name, args, body) -> HelperClause(fn name, List.map (process_argument_name fn) args, List.map (process_literal_name fn) body);
		|NotifClause(name, args, body) -> NotifClause(name, List.map (process_argument_name fn) args, List.map (process_literal_name fn) body);;

	let process_name (name : string) (str : string) : string =
		if not (String.contains str '/') then str ^ "/" ^ name else str;;

	let fix_names (prgm : program) : program = 
		match prgm with Program(name, modules, blackboxes, ntypes, clauses) -> 
		Program(name, modules, blackboxes, List.map (process_notif_type_name (process_name name)) ntypes, List.map (process_clause_name (process_name name)) clauses);;

	let rec remove_duplicates (l : 'a list) : 'a list =
		match l with
		[] -> [];
		| h :: t -> if List.mem h t then (remove_duplicates t) else h :: (remove_duplicates t);;

	let import (main : program) (imports : program list) : program =
		List.fold_right (fun prgm acc -> 
		match acc with Program(acc_name, acc_modules, acc_blackboxes, acc_ntypes, acc_clauses) ->
		match prgm with Program(prgm_name, prgm_modules, prgm_blackboxes, prgm_ntypes, prgm_clauses) ->
		let modules = remove_duplicates (prgm_modules @ acc_modules) in
		let blackboxes = prgm_blackboxes @ acc_blackboxes in
		let ntypes = prgm_ntypes @ acc_ntypes in
		let clauses = (List.map (fun cls -> match cls with
			| NotifClause(cls_name, cls_args, cls_body) -> HelperClause(process_name prgm_name cls_name, cls_args, cls_body);
			| _ -> cls;) prgm_clauses) @ acc_clauses in
		Program(acc_name, modules, blackboxes, ntypes, clauses)) imports main;;
	
end

module Types = struct
	(* type name, field names *)
	type notif_type = Type of string * string list;;
	(* type of black boxes. name, ip, port. *)
	type blackbox = Internal_BB of string | External_BB of string * string * int;;
	(* type name, variable name *)
	type notif_var = Notif_var of notif_type * string;;
	(* constants and variables or a field of a value (like pkt.locPt) *)
	type term = Constant of string | Variable of string | Field_ref of notif_var * string;;
	(* type of actual arriving notification. type and values *)
	type notif_val = Notif_val of notif_type * term list;;
	(* things like A = B or R(A, B, C) *)
	type atom = Equals of term * term | Apply of string * term list | Query of blackbox * string * term list | Bool of bool;;
	(* atoms and negations of atoms *)
	type literal = Pos of atom | Neg of atom;;
	(* argument to a clause is either a notif_var or a term
	This contains the local decl of the var, including its type, in the head *)
	type argument = Arg_notif of notif_var | Arg_term of term;;
	(* name, arguments, body *)

	type clause = 
		(* rest of name, args, body*)
		PlusClause of string * argument list * literal list |
		(* rest of name, args, body *)
		MinusClause of string * argument list * literal list |
		(* name, args, body *)
		HelperClause of string * argument list * literal list |
		(* name, args (only 2 and both are Arg_notif), body *)
		NotifClause of blackbox * argument list * literal list;;

	type relation =
		(* rest of name, args, body*)
		PlusRelation of string * argument list * clause list |
		(* rest of name, args, body*)
		MinusRelation of string * argument list * clause list |
		(* name, args, body *)
		HelperRelation of string * argument list * clause list |
		(* name, args (only 2 and both are Arg_notif), body *)
		NotifRelation of blackbox * argument list * clause list;;

	(* name, relations *)	
	type program = Program of string * relation list;;

	(* raised on errors converting from Syntax to Types. *)
	exception Parse_error of string;;

	let packet_type = Type("packet", ["LocSw"; "LocPt"; "DlSrc"; "DlDst"; "DlTyp"; "NwSrc"; "NwDst"; "NwProto"]);;
	let switch_port_type = Type("switch_port_type", ["Switch"; "Port"]);;

	(* Functions to turn a Syntax.program into a Types.program. *)

	(* These functions assume that there are no modules to be imported in prgm. *)
	let notif_type_convert (nt : Syntax.notif_type) : notif_type = 
		match nt with Syntax.Notif_type(name, field_names) -> Notif_type(name, field_names);;

	let notif_var_convert (prgm : Syntax.program) (cls : Syntax.clause) (nv : Syntax.notif_var) : notif_var =
		match prgm with Syntax.Program(_, _, _, ntypes, _) ->
		match nv with Syntax.Notif_var(type_name, var_name) ->
		match List.filter (function Syntax.Type(n, _) -> type_name = n ) ntypes with
		| [] -> raise (Parse_error "notif_var " ^ var_name ^ " in clause " ^ (Syntax.clause_name cls) ^ " has an invalid type " ^ type_name);
		| t :: _ -> Types.Notif_var(t, var_name);;

	let term_convert (prgm : Syntax.program) (cls : Syntax.clause) (t : Syntax.term) : term = 
		match t with
		| Syntax.Constant(str) -> Constant(str);
		| Syntax.Variable(str) -> Variable(str);
		| Syntax.Field_ref(var_name, field_name) ->
		match List.filter (function 
			| Syntax.Arg_notif(Syntax.Notif_var(_, name)) -> name = var_name;
			| _ -> false;) (clause_arguments cls) with
		| [] -> raise (Parse_error "the notification in " ^ (Syntax.term_to_string t) ^ " in clause " ^ (Syntax.clause_name cls) ^ " is not defined.");
		| Syntax.Arg_notif(nv) :: _ -> Field_ref((notif_var_convert prgm cls nv), field_name);;

	let blackbox_convert (bb : Syntax.blackbox) : blackbox =
		match bb with
		| Syntax.Internal_BB(n) -> Internal_BB(n);
		| Syntax.External_BB(n, ip, port) -> External_BB(n, ip, port);;

	let atom_convert (prgm : Syntax.program) (cls : Syntax.clause) (a : Syntax.atom) : atom =
		match a with
		| Syntax.Equals(t1, t2) -> Equals((term_convert prgm cls t1), (term_convert prgm cls t2));
		| Syntax.Apply(rel_name, terms) -> Apply(rel_name , List.map (term_convert prgm cls) terms);
		| Syntax.Bool(b) -> Bool(b);
		| Syntax.Query(bbname, rel_name, terms) ->
			match prgm with Syntax.Program(_, _, blackboxes, _, _) ->
			match List.filter (fun bb -> Syntax.blackbox_name bb = n) blackboxes with
			| [] -> raise (Parse_error "black box " ^ bbname ^ " in clause " ^ (Syntax.clause_name cls) ^ " does not exist.");
			| bb :: _ -> Query(blackbox_convert bb, rel_name, List.map (term_convert prgm cls) terms);;

	let literal_convert (prgm : Syntax.program) (cls : Syntax.clause) (l : Syntax.literal) : literal =
		match l with
		| Syntax.Pos(a) -> Pos(atom_convert prgm cls a);
		| Syntax.Neg(a) -> Neg(atom_convert prgm cls a);;

	let argument_convert (prgm : Syntax.program) (cls : Syntax.clause) (arg : Syntax.argument) : argument = 
		match arg with
		| Syntax.Arg_notif(nv) -> Arg_notif(notif_var_convert prgm nv);
		| Syntax.Arg_term(t) ->
			match t with
			| Syntax.Constant(_) -> raise (Parse_error "argument " ^ (Syntax.argument_to_string arg) ^ " in clause " ^ (Syntax.clause_name cls) ^ " is a constant.");
			| Syntax.Variable(str) -> Arg_term(Variable(str));
			| Syntax.Field_ref(_, _) -> raise (Parse_error "argument " ^ (Syntax.argument_to_string arg) ^ " in clause " ^ (Syntax.clause_name cls) ^ " is a field ref.");;

	let clause_convert (prgm : Syntax.program) (cls : Syntax.clause) : clause = 
		match cls with
		| Syntax.PlusRelation(name, args, body) -> PlusRelation(name, List.map (argument_convert prgm cls) args, List.map (literal_convert prgm cls) body);
		| Syntax.MinusRelation(name, args, body) -> MinusRelation(name, List.map (argument_convert prgm cls) args, List.map (literal_convert prgm cls) body);
		| Syntax.HelperRelation(name, args, body) -> HelperRelation(name, List.map (argument_convert prgm cls) args, List.map (literal_convert prgm cls) body);
		| Syntax.NotifRelation(bb, args, body) ->
			let name = Syntax.blackbox_name bb in
			match prgm with Syntax.Program(_, _, blackboxes, _, _) ->
			match List.filter (fun bbox -> Syntax.blackbox_name bbox = n) blackboxes with
			| [] -> raise (Parse_error "black box " ^ name ^ " in clause " ^ (Syntax.clause_name cls) ^ " does not exist.");
			| bb :: _ -> NotifRelation(blackbox_convert bb, List.map (argument_convert prgm cls) args, List.map (literal_convert prgm cls) body);;

	let rec drop (l : 'a list) (n : int) : 'a list = 
		if n <= 0 then l else
		match l with
		| [] -> [];
		| h :: t -> drop t (n - 1);;

	let blackbox_name (bb : blackbox) =
		match bb with
		| Internal_BB(name) -> name;
		| External_BB(name, _, _) -> name;;

	let clause_key (cls : clause) : string =
		match cls with
		| PlusClause(name, args, _) -> "plus " ^ name ^ " " ^ (args_to_string args);
		| MinusClause(name, args, _) -> "minus " ^ name " " ^ (args_to_string args);
		| HelperClause(name, args, _) -> "helper " ^ name ^ " " ^ (args_to_string args);
		| NotifClause(bb, args, _) -> "notif" ^ (blackbox_name bb) ^ " " ^ (args_to_string args);;

	let helper_relation_key (cls : clause) : string =
		match cls with
		| PlusClause(name, args, _) -> clause_key HelperClause(name, drop args 1, []);
		| MinusClause(name, args, _) -> clause_key HelperClause(name, drop args 1, []);
		| _ -> raise (Failure "only plus and minus clauses can be passed to implicit_relation_key");;

	let make_relations (clauses : clause list) : relation list =
		let tbl = Hashtbl.create (List.length clauses) in
		let _ = List.iter (fun cls -> match cls with
		| PlusClause(_, _, _) ->
			let key = clause_key cls in
			let _ = try (match Hashtbl.find tbl key with PlusRelation(name, args, clauses) ->
				Hashtbl.replace tbl key PlusRelation(name, args, cls :: clauses))
			with Not_found -> Hashtbl.add tbl key PlusRelation(name, args, [cls]) in
			let helper_key = helper_relation_key cls in
			if not Hashtbl.mem tbl helper_key then Hashtbl.add tbl helper_key HelperRelation(name, drop args 1, []);
		| MinusClause(_, _, _) ->
			let key = clause_key cls in
			let _ = try (match Hashtbl.find tbl key with MinusRelation(name, args, clauses) ->
				Hashtbl.replace tbl key MinusRelation(name, args, cls :: clauses))
			with Not_found -> Hashtbl.add tbl key PlusRelation(name, args, [cls]) in
			let helper_key = helper_relation_key cls in
			if not Hashtbl.mem tbl helper_key then Hashtbl.add tbl helper_key HelperRelation(name, drop args 1, []);
		| HelperClause(_, _, _) ->
			let key = clause_key cls in
			try (match Hashtbl.find tbl key with HelperRelation(name, args, clauses) ->
				Hashtbl.replace tbl key HelperRelation(name, args, cls :: clauses))
			with Not_found -> Hashtbl.add tbl key HelperRelation(name, args, [cls]);
		| NotifClause(_, _, _) ->
			let key = clause_key cls in
			try (match Hashtbl.find tbl key with NotifRelation(bb, args, clauses) ->
				Hashtbl.replace tbl key NotifRelation(bb, args, cls :: clauses))
			with Not_found -> Hashtbl.add tbl key PlusRelation(bb, args, [cls]);) clauses in
		Hashtbl.fold (fun key_str rel acc -> rel :: acc) tbl [];;
	
	let program_convert (prgm : Syntax.program) : program = 
		match prgm with Syntax.Program(name, _, _, _, clauses) ->
		Program(name, make_relations (List.map (clause_convert prgm) clauses));;

end

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

	let atom_to_string (a : Types.atom) : string =
		match a with
		| Types.Equals(t1, t2) -> (term_to_string t1) ^ " = " ^ (term_to_string t2);
		| Types.Apply(str, tl) -> str ^ "(" ^ (list_to_string term_to_string tl) ^ ")";
		| Types.Query(bb, name, tl) -> (Types.blackbox_name bb) ^ "_" ^ name ^ "(" ^ (list_to_string term_to_string tl) ^ ")";
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
		| Types.Arg_notif(n) -> (notif_var_to_terms n) @ acc) args [];;

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
		| Types.NotifClause(name, _, _) -> name;;

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
		if body = [] then name ^ "(" ^ (list_to_string argument_to_string args) ^ ")"
		else name ^ "(" ^ (list_to_string argument_to_string args) ^ ") :- " ^
			(list_to_string literal_to_string body);;

	let relation_name (rel : Types.relation) : string = 
		match rel with
		Relation(name, _, _) -> name;;

end
