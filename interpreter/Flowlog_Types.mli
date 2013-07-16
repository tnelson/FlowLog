module Syntax : sig
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
	
	(* some helper functions *)
	val term_to_string : term -> string;;

	val blackbox_name : blackbox -> string;;

	val clause_name : clause -> string;;

	val clause_arguments : clause -> argument list;;

	val argument_name : argument -> string;;

end

module Types : sig
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

	val packet_type : notif_type;;
	val switch_port_type : notif_type;;

end