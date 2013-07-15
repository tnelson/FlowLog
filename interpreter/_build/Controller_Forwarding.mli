open Flowlog_Types;;
open Packet;;
open OxPlatform;;
open OpenFlow0x01_Core;;
open OpenFlow0x01;;


module Controller_Forwarding : sig
	val update_buffer : (switchId * packetIn) option -> unit;;
	val forward_packets : Types.notif_val list -> unit;;
        val pkt_to_notif : switchId -> packetIn -> Types.notif_val;;
        val debug : bool;;
end

