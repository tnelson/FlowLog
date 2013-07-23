open Types;;
open Packet;;
open OxPlatform;;
open OpenFlow0x01_Core;;
open OpenFlow0x01;;


module Controller_Forwarding : sig
	val update_buffer : (switchId * packetIn) option -> unit;;
	val queue_packets : Types.term list -> unit;;
	val flush_packets : unit -> unit;;
    val pkt_to_notif : switchId -> packetIn -> Types.term;;
end

