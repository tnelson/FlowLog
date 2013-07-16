open Unix;;
open Packet;;
open OxPlatform;;
open OpenFlow0x01_Core;;

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
		| PlusClause(name,_, _) -> "+" ^ name;
		| MinusClause(name, _, _) -> "-" ^ name;
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
	type program = Program of string * notif_type list * relation list;;

	let packet_type = Type("packet", ["LOCSW"; "LOCPT"; "DLSRC"; "DLDST"; "DLTYP"; "NWSRC"; "NWDST"; "NWPROTO"]);;
	let switch_port_type = Type("switch_port", ["SWITCH"; "PORT"]);;

end