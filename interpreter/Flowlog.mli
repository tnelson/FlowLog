open Packet;;
open OxPlatform;;
open OpenFlow0x01_Core;;

module To_String = struct
	val list_to_string :  ('a -> string) -> ('a list) -> string;;

	val term_to_string : term -> string;;

	let atom_to_string (a : atom) : string =
		match a with
		| Equals(t1, t2) -> term_to_string(t1) ^ " = " ^ term_to_string(t2);
		| Apply(str, args) -> str ^ "(" ^ (list_to_string term_to_string args) ^ ")";
		| Bool(b) -> string_of_bool b;;

	let literal_to_string (l : literal) : string = 
		match l with
		| Pos(a) -> atom_to_string a;
		| Neg(a) -> "not(" ^ (atom_to_string a) ^ ")";;
	
	let get_atom (lit : literal) : atom =
		match lit with
		| Pos(a) -> a;
		| Neg(a) -> a;;

	let clause_to_string (cl : clause) : string =
		match cl with
		| Clause(str, args, []) -> str ^ "(" ^ (list_to_string term_to_string args) ^ ")";
		| Clause(str, args, body) -> str ^ "(" ^ (list_to_string term_to_string args) ^ ") :- " ^
			(list_to_string literal_to_string body);;

	let relation_name (rel : relation) : string = 
		match rel with
		Relation(str, _, _) -> str;;
end

module Flowlog : sig
	(* constants and variables (recall variables are uppercase) *)
	type term = Constant of string | Variable of string;;
	(* Things like A = B or R(A, B, C) *)
	type atom = Equals of term * term | Apply of string * term list | Bool of bool;;
	(* Atoms and negations of atoms *)
	type literal = Pos of atom | Neg of atom;;
	(* name, arguments, body *)
	type clause = Clause of string * term list * literal list;;
	(* name, arguments, clauses, plus, minus *)
	type relation = Relation of string * term list * clause list;;
	(* name, relations *)
	type program = Program of string * relation list * relation;;

	val packet_vars : term list;;
	val packet_vars_2 : term list;;
	val start_program : program -> out_channel -> in_channel -> (term list) list;;
	val respond_to_packet : program -> switchId -> xid -> packetIn -> out_channel -> in_channel -> unit;;

end