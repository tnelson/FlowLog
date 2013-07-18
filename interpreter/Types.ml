module Types = struct
	(* either internal or external in which case it has an ip and a port. *)
	type bb_type = Internal | External of string * int;;
	(* actual blackbox. *)
	type blackbox = BlackBox of string * bb_type;;
	(* type name, field names *)
	type notif_type = Type of string * string list;;
	(* constant, variable, notif_var like pkt : packet, field_ref like pkt.locPt *)
	type term = Constant of string | Variable of string | Notif_var of string * string | Field_ref of string * string;;
	(* not a negation, or negation *)
	type sign = Pos | Neg;;
	(* things like A = B or R(A, B, C). For apply its sign, module, relation, args *)
	type atom = Equals of sign * term * term | Apply of sign * string * string * term list | Bool of sign * bool;;
	(* type of clause *)
	type clause_type = Plus | Minus | State | Action;;
	(* name, arguments, body *)
	type clause = Clause of clause_type * string * term list * atom list;;
	(* name, module names to be imported, black boxes, notification types, clauses *)	
	type program = Program of string * string list * blackbox list * notif_type list * clause list;;

	(* actual notification value *)
	type notif_val = Notif_val of notif_type * term list;;

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