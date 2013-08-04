open Types;;
open Type_Helpers;;
open Xsb_Communication;;
open Evaluation;;
open Packet;;
open OxPlatform;;
open OpenFlow0x01_Core;;
open OpenFlow0x01;;
open Controller_Forwarding;;
open Flowlog_Thrift_In;;

let debug = true;;

module type PROGRAM = sig
	val program : Types.program;;
end

(* 
need:
Evalutaion.respond_to_notification
*)

module Make_OxModule (Program : PROGRAM) = struct
	include OxStart.DefaultTutorialHandlers;;	

    (* Start up XSB, etc. *)
	Communication.start_program Program.program;;
   
    (* Listen for incoming notifications via RPC *)
    Flowlog_Thrift_In.start_listening Program.program;;
 
    (* Send the "startup" notification. Enables initialization, etc. in programs *)
    let startup = Types.Constant([], Types.startup_type) in
	  Evaluation.respond_to_notification startup Program.program;;

	
	let switch_connected (sw : switchId) (feats : OpenFlow0x01.SwitchFeatures.t) : unit =
	    Printf.printf "Switch %Ld connected.\n%!" sw;
	    let port_nums = List.map (fun (x : PortDescription.t)-> x.PortDescription.port_no) feats.SwitchFeatures.ports in
	    let sw_string = Int64.to_string sw in
	    let notifs = List.map (fun portid -> Types.Constant([sw_string; string_of_int portid], Types.switch_port_type)) port_nums in
	    List.iter (fun notif -> Evaluation.respond_to_notification notif Program.program) notifs;;
	    
	let packet_in (sw : switchId) (xid : xid) (pk : packetIn) : unit =
		Printf.printf "Packet in on switch %Ld.\n%s\n%!" sw (packetIn_to_string pk);
		(* pkt_to_notif parses the packet; don't repeat that work *)
		let notif = (Controller_Forwarding.pkt_to_notif sw pk) in		  
		  Controller_Forwarding.remember_for_forwarding (Some (sw, pk, notif));
		  Evaluation.respond_to_notification notif Program.program;
		  (* for the love of sanity, remember to clear this out even if no packets result! *)
		  Controller_Forwarding.clear_remember_for_forwarding();;		  
	
	let cleanup () : unit = 
		if debug then print_endline "running cleanup";
		Xsb.halt_xsb ();;

end

module Make_Controller (Program : PROGRAM) = OxStart.Make (Make_OxModule (Program));;
