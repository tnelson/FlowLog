open Flowlog_Types;;

module Xsb : sig
	val start_xsb : unit -> out_channel * in_channel;;
	val send_assert : string -> string;;
	val send_query : string -> int -> (string list) list;;
	val halt_xsb : unit -> unit;;
end

module Communication : sig
	val query_relation : Types.relation -> Types.argument list -> (Types.term list) list;;
	val retract_relation : Types.relation -> Types.term list -> unit;;
	val assert_relation : Types.relation -> Types.term list -> unit;;
	val start_program : Types.program -> unit;; 
end
