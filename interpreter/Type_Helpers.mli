open Types;;

module Type_Helpers : sig
	val ends_with : string -> string -> bool;;

	val list_to_string : ('a -> string) -> 'a list -> string;;

	val blackbox_name : Types.blackbox -> string;;

	val term_type_name : Types.term_type -> string;;

	val type_of_term : Types.term -> Types.term_type;;

	val term_to_string : Types.term -> string;;

	val bool_to_string : bool -> string;;

	val atom_to_string : Types.atom -> string;;

	val clause_type_to_string : Types.clause_type -> string;;

	val clause_signature : Types.clause -> string;;

	val signature_name : Types.signature -> string;;

	val signature_to_string : Types.signature -> string;;

	val clause_to_string : Types.clause -> string;;

	val get_blackbox : Types.program -> string -> Types.blackbox;;

end

module Parse_Helpers : sig
	val process_program_names : Types.program -> Types.program;;

	val import : Types.program -> Types.program list -> Types.program;;

	val process_program_types : Types.program -> Types.program;;

end
