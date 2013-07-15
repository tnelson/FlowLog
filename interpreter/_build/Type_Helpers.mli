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

	val literal_to_string : Types.literal -> string;;

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

module Parsing : sig
	(* raised on errors in parsing or converting from Syntax to Types. *)
	exception Parse_error of string;;

	val make_Program : string -> string list -> Syntax.blackbox list -> Syntax.notif_type list -> Syntax.clause list -> Syntax.program;;

	val import : Syntax.program -> Syntax.program list -> Syntax.program;;

	val make_External_BB : string -> string -> int -> Syntax.blackbox;;

	val make_Internal_BB : string -> Syntax.blackbox;;

	val make_Type : string -> string list -> Syntax.notif_type;;

	val make_Plus_Minus_Clause : string -> Syntax.argument list -> Syntax.literal list -> Syntax.clause;;

	val make_HelperClause : string -> Syntax.argument list -> Syntax.literal list -> Syntax.clause;;

	val make_NotifClause : string -> Syntax.argument list -> Syntax.literal list -> Syntax.clause;;

	val make_Arg_term : Syntax.term -> Syntax.argument;;

	val make_Variable : string -> Syntax.term;;

	val make_Arg_notif : Syntax.notif_var -> Syntax.argument;;

	val make_Notif_var : string -> string -> Syntax.notif_var;;

	val make_Apply : string -> Syntax.term list -> Syntax.atom;;

	val make_Query : string -> string -> Syntax.term list -> Syntax.atom;;

	val make_Constant_Variable : string -> Syntax.term;;

	val make_Field_ref : string -> string -> Syntax.term;;

end

module Conversion : sig
	(* turns a Syntax.program into a Types.program *)
	val program_convert : Syntax.program -> Types.program;;
end