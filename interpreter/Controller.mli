open Flowlog_Types;;
open Packet;;
open OxPlatform;;
open OpenFlow0x01_Core;;
open OpenFlow0x01;;

module type PROGRAM : sig
	val program : Types.program;;
end

module Forwarding : sig
	val update_buffer : switchId * packetIn option -> unit;;
	val forward_packets : Types.notif_val list -> unit;;
end

module Make_Controller : functor (Program : PROGRAM) -> sig end;;