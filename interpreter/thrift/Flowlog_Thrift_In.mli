open Arg
open Thrift
open Flowlog_rpc_types
open Thread
open Flowlog_Types.Types
open Type_Helpers


module Flowlog_Thrift_In : sig
        val start_listening : program -> notif_type list -> unit;;
end
