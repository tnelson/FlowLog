module Types : sig
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
	(* things like A = B or R(A, B, C) *)
	type atom = Equals of sign * term * term | Apply of sign * string * term list | Bool of sign * bool;;
	(* type of clause *)
	type clause_type = Plus | Minus | State | Action;;
	(* name, arguments, body *)
	type clause = Clause of clause_type * string * term list * atom list;;

	(* name, module names to be imported, black boxes, notification types, clauses *)	
	type program = Program of string * string list * blackbox list * notif_type list * clause list;;

	(*val make_notif_val : program -> string -> string list -> notif_val;;*)
	val packet_type : notif_type;;
	val switch_port_type : notif_type;;

end