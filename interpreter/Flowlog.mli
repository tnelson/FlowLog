open Packet;;
open OxPlatform;;
open OpenFlow0x01_Core;;

module Flowlog : sig
	(* constants and variables (recall variables are uppercase) *)
	type term = Constant of string | Variable of string;;
	(* Things like A = B or R(A, B, C) *)
	type atom = Equals of term * term | Apply of string * term list | Bool of bool;;
	(* Atoms and negations of atoms *)
	type literal = Pos of atom | Neg of atom;;
	(* name, arguments, body *)
	type clause = Clause of string * term list * literal list;;
	(* name, arguments, clauses *)
	type relation = Relation of string * term list * clause list;;
	(* name, relations, forward *)
	type program = Program of string * relation list * relation;;

	val packet_vars : term list;;
	val packet_vars_2 : term list;;
	val shp_vars : term list;;
	val shp_name : string;;
	val start_program : program -> out_channel -> in_channel -> (term list) list;;
	val update_switch_ports : switchId -> portId list -> out_channel -> in_channel -> unit;;
	val respond_to_packet : program -> switchId -> xid -> packetIn -> out_channel -> in_channel -> unit;;

end

module To_String : sig
	val list_to_string :  ('a -> string) -> ('a list) -> string;;

	val term_to_string : Flowlog.term -> string;;

	val atom_to_string : Flowlog.atom -> string;;

	val literal_to_string : Flowlog.literal -> string

	val get_atom : Flowlog.literal -> Flowlog.atom;;

	val clause_to_string : Flowlog.clause -> string;;

	val relation_name : Flowlog.relation -> string;;

	val print_relation : Flowlog.relation -> unit;;
end

module Flowlog_Parsing : sig
	val make_relations : Flowlog.clause list -> Flowlog.relation list;;

	val make_program : string -> Flowlog.relation list -> Flowlog.program;;
end