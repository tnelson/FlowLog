open Xsb;;
open Flowlog;;
open Packet;;
open OxPlatform;;
open OpenFlow0x01_Core;;

module type PROGRAM = sig
	val program : Flowlog.program;;
end

module Make_OxModule (Program : PROGRAM) = struct
	include OxStart.DefaultTutorialHandlers;;

	let switch_connected (sw : switchId) : unit =
	    Printf.printf "Switch %Ld connected.\n%!" sw
	
	let ref_out_ch = ref None;;
	let ref_in_ch = ref None;;
	let get_ch = (fun () -> match !ref_out_ch with
		| None -> let out_ch, in_ch = Xsb.start_xsb () in 
			let _ = ref_out_ch := Some(out_ch) in
			let _ = ref_in_ch := Some(in_ch) in
			let _ = Flowlog.start_program Program.program out_ch in_ch in
			let _ = print_endline "started program" in
			(out_ch, in_ch);
		| Some(out_ch) -> match !ref_in_ch with
			|Some(in_ch) -> (out_ch, in_ch);
			| _ -> raise (Failure "ref_out_ch is some but ref_in_ch is none"));;
	
	let _ = get_ch ();;
	
	let packet_in (sw : switchId) (xid : xid) (pk : packetIn) =
		Printf.printf "%s\n%!" (packetIn_to_string pk);
		let out_ch, in_ch = get_ch () in
		Flowlog.respond_to_packet Program.program sw xid pk out_ch in_ch;;

	let cleanup () = let out_ch, in_ch = get_ch () in Xsb.halt_xsb out_ch;;

end

module Make_Controller (Program : PROGRAM) = OxStart.Make (Make_OxModule (Program));;