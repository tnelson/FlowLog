open Arg
open Thrift
open Flowlog_rpc_types
open Thread
open Type_Helpers
open Types

module Flowlog_Thrift_Out : sig
    (* sends the blackbox a notification. the term must be a notif. constant *)
	val doBBnotify : Types.blackbox -> Types.term -> unit;;
    (* returns list of lists of strings ~= set of tuples of values *)
    val doBBquery : Types.blackbox -> Types.atom -> string list list;;
end

