open Flowlog_Types;;
open Packet;;
open OxPlatform;;
open OpenFlow0x01_Core;;
open OpenFlow0x01;;

module Evaluation : sig
	val respond_to_notification : Types.notif_val -> Types.program -> unit;;
end