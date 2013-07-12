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
	
	(* appends name of module to relation names besides black boxes *)
	val fix_names : program -> program;;

	(* imports the second argument into the first argument to make a new program *)
	val import : program -> program list -> program;;

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
		PlusRelation of string * string * argument list * clause list |
		(* rest of name, args, body*)
		MinusRelation of string * string * argument list * clause list |
		(* name, args, body *)
		HelperRelation of string * argument list * clause list |
		(* name, args (only 2 and both are Arg_notif), body *)
		NotifRelation of blackbox * argument list * clause list;;

	(* name, relations *)	
	type program = Program of string * relation list;;

	(* raised on errors converting from Syntax to Types. *)
	exception Parse_error of string;;

	(* turns a Syntax.program into a Types.program *)
	val program_convert : Syntax.program -> program;;
end

module Type_Helpers : sig
	val list_to_string :  ('a -> string) -> ('a list) -> string;;

	val notif_var_name : Syntax.notif_var -> string;;

	val notif_var_to_terms : Syntax.notif_var -> Syntax.term list;;

	val notif_var_to_string : Syntax.notif_var -> string;;

	val notif_type_to_string : Syntax.notif_type -> string;;

	val notif_val_to_string : Syntax.notif_val -> string;;

	val term_to_string : Syntax.term -> string;;

	val atom_to_string : Syntax.atom -> string;;

	val literal_to_string : Syntax.literal -> string

	val get_atom : Syntax.literal -> Syntax.atom;;

	val argument_to_string : Syntax.argument -> string;;

	val arguments_to_terms : Syntax.argument list -> Syntax.term list;;

	val terms_to_notif_val : Syntax.notif_type -> Syntax.term list -> Syntax.notif_val;;

	val clause_to_string : Syntax.clause -> string;;

	val relation_name : Syntax.relation -> string;;

	val relation_trigger_type : Syntax.relation -> Syntax.notif_type option;;

	val print_relation : Syntax.relation -> unit;;

	val find_relation_by_name : Syntax.program -> string -> Syntax.relation option;;

	val is_forward_relation : Syntax.program -> Syntax.relation -> bool;;

	val forward_relation : Syntax.program -> Syntax.relation;;
end