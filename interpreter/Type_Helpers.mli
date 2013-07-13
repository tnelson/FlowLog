open Flowlog_Types;;

module Type_Helpers : sig
	val list_to_string :  ('a -> string) -> ('a list) -> string;;

	val notif_var_name : Types.notif_var -> string;;

	val notif_var_to_terms : Types.notif_var -> Types.term list;;

	val notif_var_to_string : Types.notif_var -> string;;

	val notif_type_to_string : Types.notif_type -> string;;

	val notif_val_to_string : Types.notif_val -> string;;

	val term_to_string : Types.term -> string;;

	val blackbox_name : Types.blackbox -> string;;

	val atom_to_string : Types.atom -> string;;

	val literal_to_string : Types.literal -> string

	val get_atom : Types.literal -> Types.atom;;

	val argument_to_string : Types.argument -> string;;

	val arguments_to_terms : Types.argument list -> Types.term list;;

	val terms_to_notif_val : Types.notif_type -> Types.term list -> Types.notif_val;;

	val clause_name : Types.clause -> string;;

	val clause_arguments : Types.clause -> Types.argument list;;

	val clause_body : Types.clause -> Types.literal list;;

	val clause_to_string : Types.clause -> string;;

	val relation_body : Types.relation -> Types.clause list;;

end

module Conversion : sig
	(* turns a Syntax.program into a Types.program *)
	val program_convert : Syntax.program -> Types.program;;
end