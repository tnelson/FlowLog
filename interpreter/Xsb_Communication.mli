open Unix;;
open Printf;;

module Xsb : sig
	val start_xsb : unit -> out_channel * in_channel;;
	val send_assert : string -> out_channel -> in_channel -> string;;
	val send_query : string -> int -> out_channel -> in_channel -> (string list) list;;
	val halt_xsb : out_channel -> unit;;
end