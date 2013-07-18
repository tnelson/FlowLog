module Types = struct
	(* either internal or external in which case it has an ip and a port. *)
	type bb_type = Internal | External of string * int | BB_defer;;
	(* actual blackbox. *)
	type blackbox = BlackBox of string * bb_type;;
	(* type name, field names *)
	type term_type = Type of string * string list | Term_defer of string option;;
	(* constants, variables, field refs *)
	type term = Constant of string list * term_type | Variable of string * term_type | Field_ref of term * string;;
	type sign = Pos | Neg;;
	(* things like A = B or R(A, B, C). For apply its sign, module, relation, args *)
	type atom = Equals of sign * term * term | Apply of sign * blackbox option * string * term list | Bool of bool;;
	(* type of clause *)
	type clause_type = Plus | Minus | Helper | Action;;
	(* name, arguments, body *)
	type clause = Clause of clause_type * string * term list * atom list;;
	(* groupings of clauses by name and type signature.
		type, name (this is the long desugared name e.g. learned_pkt_raw_raw_raw for mac_learning's plus learned clause),
		arguments (all variables), clauses (all the clauses with that signature) *)
	type relation = Relation of clause_type * string * term list * clause list;;
	(* name, module names to be imported, black boxes, notification types, clauses *)	
	type raw_program = Raw_program of string * string list * blackbox list * term_type list * clause list;;
	type program = Program of string * blackbox list * term_type list * relation list;;


	(*let make_notif_val (prgm : program) (type_name : string) (vals : string list) : notif_val =
		match prgm with Program(prgm_name, _, _, types, _) ->
		match List.filter (function Type(name, _) -> name = String.lowercase type_name) types with
		| [] -> raise (Failure ("Program " ^ prgm_name ^ " does not have a type called " ^ type_name "."));
		| Type(name, fields) :: _ -> if fields.length = vals.length then
			Notif_val(Type(name, fields), List.map (fun str -> Constant(str)) vals) else
			raise (Failure "Too many arguments passed into a notif_val of type " ^ name ".");;*)

	let packet_type = Type("packet", ["LOCSW"; "LOCPT"; "DLSRC"; "DLDST"; "DLTYP"; "NWSRC"; "NWDST"; "NWPROTO"]);;
	let switch_port_type = Type("switch_port", ["SWITCH"; "PORT"]);;

end