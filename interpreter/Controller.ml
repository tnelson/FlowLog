open Flowlog_Types;;
open Type_Helpers;;
open Xsb_Communication;;
open Evaluation;;
open Packet;;
open OxPlatform;;
open OpenFlow0x01_Core;;
open OpenFlow0x01;;
open Controller_Forwarding;;

let debug = true;;

module type PROGRAM = sig
	val program : Types.program;;
end


module Make_OxModule (Program : PROGRAM) = struct
	include OxStart.DefaultTutorialHandlers;;	
	
	Communication.start_program Program.program;;

	let switch_connected (sw : switchId) (feats : OpenFlow0x01.SwitchFeatures.t) : unit =
	    Printf.printf "Switch %Ld connected.\n%!" sw;
	    let port_nums = List.map (fun (x : PortDescription.t)-> x.PortDescription.port_no) feats.SwitchFeatures.ports in
	    let sw_string = Int64.to_string sw in
	    let notifs = List.map (fun portid -> Types.Notif_val(Types.switch_port_type, [Types.Constant(sw_string); Types.Constant(string_of_int portid)])) port_nums in
	    List.iter (fun notif -> Evaluation.respond_to_notification notif Program.program) notifs;
	    if debug then Xsb.debug_print_listings ();;

	let packet_in (sw : switchId) (xid : xid) (pk : packetIn) : unit =
		Printf.printf "%s\n%!" (packetIn_to_string pk);
		Controller_Forwarding.update_buffer (Some (sw, pk));
		Evaluation.respond_to_notification (Controller_Forwarding.pkt_to_notif sw pk) Program.program;
		if debug then Xsb.debug_print_listings ();;
	
	let cleanup () : unit = 
		if debug then print_endline "running cleanup";
		Xsb.halt_xsb ();;

end

module Make_Controller (Program : PROGRAM) = OxStart.Make (Make_OxModule (Program));;
