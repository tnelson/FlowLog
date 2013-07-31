module Types = struct
	(* either internal or external in which case it has an ip and a port. *)
	type bb_type = Internal | External of string * int;;
	(* actual blackbox. *)
	type blackbox = BlackBox of string * bb_type;;
	(* type name, field names *)
	type term_type = Type of string * string list | Term_defer of string;;
	(* constants, variables, field refs *)
	type term = Constant of string list * term_type | Variable of string * term_type | Field_ref of string * string;;
	(* things like A = B or R(A, B, C) or true. For apply it's sign, blackbox/module, relation, args *)
	type atom = Equals of bool * term * term | Apply of bool * string * string * term list | Bool of bool;;
	(* type of clause *)
	type clause_type = Plus | Minus | Helper | Action;;
	(* clause type, module, name, args *)
	type signature = Signature of clause_type * string * string * term list;;
	(* clause type, name, arguments, body *)
	type clause = Clause of signature * atom list;;
	(* name, module names to be imported, black boxes, notification types, clauses *)
	type program = Program of string * string list * blackbox list * term_type list * clause list;;

	let raw_type = Type("raw", ["VALUE"]);;
	let packet_type = Type("packet", ["LOCSW"; "LOCPT"; "DLSRC"; "DLDST"; "DLTYP"; "NWSRC"; "NWDST"; "NWPROTO"]);;
	let switch_port_type = Type("switch_port", ["SWITCH"; "PORT"]);;
	let startup_type = Type("startup", []);;

end