open Types;;

module Xsb : sig
	val start_xsb : unit -> out_channel * in_channel * in_channel;;
	val send_assert : string -> string;;
	val send_query : string -> int -> (string list) list;;
	val halt_xsb : unit -> unit;;
	val debug_print_listings : unit -> unit;;
end

module Communication : sig
	val query_signature : Types.program -> Types.signature -> (Types.term list) list;;
	val retract_signature : Types.signature -> unit;;
	val assert_signature : Types.signature -> unit;;
	val start_program : Types.program -> unit;; 
end
