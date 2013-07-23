open Types;;
open Type_Helpers;;
open Packet;;
open OxPlatform;;
open OpenFlow0x01_Core;;
open OpenFlow0x01;;

let debug = true;;

module Controller_Forwarding = struct

	let pkt_buffer = ref None;;

	(* type of pkt_info is switchId * packetIn option but I can't declare it that for some reason. *)
	let update_buffer pkt_info : unit = pkt_buffer := pkt_info;;

	let begins_with (str1 : string) (str2 : string) : bool = 
		if String.length str2 > String.length str1 then false else
		(String.sub str1 0 (String.length str2)) = str2;;

	let get_field (notif : Types.term) (str : string) : string =
		if debug then print_endline ("starting get_field with field " ^ str);
		match notif with
		| Types.Constant(strings, Types.Type(_, fields)) ->
			let combined = List.combine fields strings in
			if debug then print_endline (Type_Helpers.list_to_string (function (str1, str2) -> str ^ ":" ^ str2) combined);
			if debug then print_endline (List.assoc str combined);
			List.assoc str combined;
		| _ -> raise (Failure "for notification values only");;

	let pkt_to_notif (sw : switchId) (pk : packetIn) : Types.term = 
		if debug then print_endline "starting pkt_to_notif";
		let pkt_payload = parse_payload pk.input_payload in
		let isIp = ((dlTyp pkt_payload) = 0x0800) in
		let strings = [Int64.to_string sw;
		string_of_int pk.port;
		Int64.to_string pkt_payload.Packet.dlSrc;
		Int64.to_string pkt_payload.Packet.dlDst;
		string_of_int (dlTyp pkt_payload);
		Int32.to_string (nwSrc pkt_payload);
		Int32.to_string (nwDst pkt_payload);
		if isIp then (string_of_int (nwProto pkt_payload)) else "arp"] in
		(*let _ = if debug then print_endline ("pkt to term list: " ^ (Type_Helpers.list_to_string Type_Helpers.term_to_string ans)) in
		let _ = if debug then print_endline ("dlTyp: " ^ (string_of_int (dlTyp pkt_payload))) in*)
		if debug then print_endline "finishing pkt_to_notif";
		Types.Constant(strings, Types.packet_type);;

	(* notice that the current implementation is not efficient--if its just a repeater its doing way too much work. *)
	let forward_packets (notifs : Types.term list) : unit =
		match !pkt_buffer with
		| None -> raise (Failure "forward packets called before packet arrived.");
		| Some(sw, pk) ->
		let _ = pkt_buffer := None in
		let actions_list = ref [] in
		let pk_notif = pkt_to_notif sw pk in
		let dlSrc_old = (get_field pk_notif "DLSRC") in
		if debug then print_endline ("dlSrc_old: " ^ dlSrc_old);
		let dlDst_old = (get_field pk_notif "DLDST") in
		let nwSrc_old = (get_field pk_notif "NWSRC") in
		let nwDst_old = (get_field pk_notif "NWDST") in
		if debug then print_endline ("dlSrc_old: " ^ dlSrc_old ^ " dlDst_old: " ^ dlDst_old ^ " nwSrc_old: " ^ nwSrc_old ^ " nwDst_old: " ^ nwDst_old);
		let _ = List.iter (fun notif -> 
			let locPt_string = (get_field notif "LOCPT") in
			(* if (begins_with locPt_string "_h") then actions_list := Output(AllPorts) :: !actions_list else *)
			let _ = actions_list := Output(PhysicalPort(int_of_string locPt_string)) :: !actions_list in
	
			let dlSrc_new = (get_field notif "DLSRC") in
			let _ = actions_list := SetDlSrc(Int64.of_string (if (begins_with dlSrc_new "_h") then dlSrc_old else dlSrc_new)) :: !actions_list in
	
			let dlDst_new = (get_field notif "DLDST") in
			let _ = actions_list := SetDlDst(Int64.of_string (if (begins_with dlDst_new "_h") then dlDst_old else dlDst_new)) :: !actions_list in
	
			let nwSrc_new = (get_field notif "NWSRC") in
			let _ = actions_list := SetNwSrc(Int32.of_string (if (begins_with nwSrc_new "_h") then nwSrc_old else nwSrc_new)) :: !actions_list in
	
			let nwDst_new = (get_field notif "NWDST") in
			let _ = actions_list := SetNwDst(Int32.of_string (if (begins_with nwDst_new "_h") then nwDst_old else nwDst_new)) :: !actions_list in
			()) notifs in
		let _ = if debug then print_endline ("print packet payload: " ^ (Packet.to_string (parse_payload pk.input_payload))) in
		send_packet_out sw 0l {output_payload = pk.input_payload; port_id = None; apply_actions = !actions_list};;

end

