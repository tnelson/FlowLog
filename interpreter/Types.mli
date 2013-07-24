module Types : sig
	(* either internal or external in which case it has an ip and a port. *)
	type bb_type = Internal | External of string * int;;
	(* actual blackbox. *)
	type blackbox = BlackBox of string * bb_type;;
	(* type name, field names *)
	type term_type = Type of string * string list | Term_defer of string;;
	(* constants, variables, field refs *)
	type term = Constant of string list * term_type | Variable of string * term_type | Field_ref of string * string;;
	(* things like A = B or R(A, B, C) or true. For apply it's sign, blackbox, relation, args *)
	type atom = Equals of bool * term * term | Apply of bool * string * term list | Bool of bool;;
	(* type of clause *)
	type clause_type = Plus | Minus | Helper | Action;;
	(* the full type of the clause *)
	type signature = Signature of clause_type * string * term list;;
	(* clause type, name, arguments, body *)
	type clause = Clause of signature * atom list;;
	(* name, module names to be imported, black boxes, notification types, clauses *)
	type program = Program of string * string list * blackbox list * term_type list * clause list;;

	val raw_type : term_type;;
	val packet_type : term_type;;
	val switch_port_type : term_type;;

end