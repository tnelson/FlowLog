open Types;;

module Type_Helpers : sig
    
end
(*
module Parsing : sig
	(* raised on errors in parsing or converting from Syntax to Types. *)
	exception Parse_error of string;;

	val make_Program : string -> string list -> Syntax.blackbox list -> Syntax.notif_type list -> Syntax.clause list -> Syntax.program;;

	val import : Syntax.program -> Syntax.program list -> Syntax.program;;

	val make_import : string -> string;;

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

	val make_Apply_Query : string -> string -> Syntax.term list -> Syntax.atom;;

	val make_Constant_or_Variable : string -> Syntax.term;;

	val make_Field_ref : string -> string -> Syntax.term;;

end

module Conversion : sig
	(* turns a Syntax.program into a Types.program *)
	val program_convert : Syntax.program -> Types.program;;
end*)