open Flowlog_Types;;

module type PROGRAM = sig
	val program : Types.program;;
end

module Make_OxModule : functor (Program : PROGRAM) -> sig
	val switch_connected : switchId -> OpenFlow0x01.SwitchFeatures.t -> unit;;
  	val switch_disconnected : switchId -> unit;;
  	val packet_in : switchId -> xid -> PacketIn.t -> unit;;
  	val barrier_reply : switchId -> xid -> unit;;
  	val stats_reply : switchId -> xid -> StatsReply.t -> unit;;
  	val cleanup : unit -> unit;;
  	val forward_packets : Types.notif_val list -> unit;;
end

module Make_Controller : functor (Program : PROGRAM) -> sig end;;