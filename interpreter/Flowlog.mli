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

	val notif_var_name : Flowlog.notif_var -> string;;

	val notif_var_to_terms : Flowlog.notif_var -> Flowlog.term list;;

	val notif_to_string : Flowlog.notif_var -> string;;

	val term_to_string : Flowlog.term -> string;;

	val atom_to_string : Flowlog.atom -> string;;

	val literal_to_string : Flowlog.literal -> string

	val get_atom : Flowlog.literal -> Flowlog.atom;;

	val argument_to_string : Flowlog.argument -> string;;

	val argument_to_terms : Flowlog.argument -> Flowlog.term list;;

	val terms_to_notif_val : Flowlog.term list -> Flowlog.notif_val;;

	val clause_to_string : Flowlog.clause -> string;;

	val relation_name : Flowlog.relation -> string;;

	val relation_trigger_type : Flowlog.relation -> Flowlog.ftype option;;

	val print_relation : Flowlog.relation -> unit;;

	val find_relation_by_name : Flowlog.program -> string -> Flowlog.relation option;;

	val is_forward_relation : Flowlog.program -> Flowlog.relation -> bool;;

	val forward_relation : Flowlog.program -> Flowlog.relation;;
end

module Evaluation : sig
	val start_program : Flowlog.program -> out_channel -> in_channel -> (term list) list;;

	val respond_to_notification : Flowlog.notif_val -> Flowlog.program -> out_channel -> in_channel -> notif_val list;;
end

module Flowlog_Parsing : sig
	val make_relations : Flowlog.clause list -> Flowlog.relation list;;

	val make_program : string -> Flowlog.relation list -> Flowlog.program;;
end