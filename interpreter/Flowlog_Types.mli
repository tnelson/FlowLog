open Packet;;
open OxPlatform;;
open OpenFlow0x01_Core;;

module Syntax : sig
	(* type name, field names *)
	type notif_type = Type of string * string list;;
	(* type, name *)
	type notif_var = Notif_var of notif_type * string;;
	(* constants and variables or a field of a value (like pkt.locPt) *)
	type term = Constant of string | Variable of string | Field_ref of notif_var * string;;
	(* type of actual arriving notification. type and values *)
	type notif_val = Notif_val of notif_type * term list;;
	(* things like A = B or R(A, B, C) *)
	type atom = Equals of term * term | Apply of string * term list | Bool of bool;;
	(* atoms and negations of atoms *)
	type literal = Pos of atom | Neg of atom;;
	(* argument to a clause is either a notif_var or a term *)
	type argument = Arg_notif of notif_var | Arg_term of term;;
	(* name, arguments, body *)
	type clause = Clause of string * argument list * literal list;;
	(* name, arguments, clauses *)
	type relation = Relation of string * argument list * clause list;;
	(* name, relations *)
	type program = Program of string * relation list;;
	
	val packet_type : notif_type;;
	val switch_port_type : notif_type;;

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

module Evaluation : sig
	val start_program : Syntax.program -> out_channel -> in_channel -> (Syntax.term list) list;;

	val respond_to_notification : Syntax.notif_val -> Syntax.program -> out_channel -> in_channel -> Syntax.notif_val list;;
end

(*module Flowlog_Parsing : sig
	val make_relations : clause list -> relation list;;

	val make_program : string -> relation list -> program;;
end*)
