open Packet;;
open OxPlatform;;
open OpenFlow0x01_Core;;

module Flowlog : sig
	type term = Constant of string | Variable of string;;
	
	(* Things like A = B or R(A, B, C) *)
	type atom = Equals of term * term | Apply of relation * term list | Bool of bool
	and
	(* the and is for mutually recursive types *)
	(* Atoms and negations of atoms *)
		literal = Pos of atom | Neg of atom
	and
	(* First string is name, second is list of arguments, third is body *)
		 clause = Clause of relation * term list * literal list
	and
	(* name, arguments, clauses, plus, minus *)
		 relation = Relation of string * term list * clause list * relation option * relation option;;
	
	(* list of non-forward relations, forward relation*)
	type program = Program of relation list * relation;;

	val start_program : program -> out_channel -> in_channel -> (term list) list;;
	val respond_to_packet : program -> switchId -> xid -> packetIn -> out_channel -> in_channel -> unit;;

end