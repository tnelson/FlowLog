open Types;;
open Packet;;
open OxPlatform;;
open OpenFlow0x01_Core;;
open OpenFlow0x01;;

module Evaluation : sig
	val respond_to_notification : Types.term -> Types.program -> (switchId * packetIn * Types.term) option -> unit;;
end