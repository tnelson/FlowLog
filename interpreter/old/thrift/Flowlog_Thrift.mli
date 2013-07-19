open Arg
open Thrift
open Flowlog_rpc_types
open Thread
open Flowlog_Types.Types
open Type_Helpers

module Flowlog_Thrift_Out : sig
	val doBBnotify : blackbox -> notif_val -> unit;;
        (* returns list of lists of strings ~= set of tuples of values *)
        val doBBquery : blackbox -> atom -> string list list;;
end

module Flowlog_Thrift_In : sig
        val start_listening : program -> notif_type list -> unit;;
end
