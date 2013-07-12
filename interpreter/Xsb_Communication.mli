open Evaluation;;

module Xsb : sig
	val start_xsb : unit -> out_channel * in_channel;;
	val send_assert : string -> string;;
	val send_query : string -> int -> (string list) list;;
	val halt_xsb : unit -> unit;;
end

module Communications : sig
	
end