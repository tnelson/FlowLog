open Types;;
open Type_Helpers;;
open Packet;;
open OxPlatform;;
open OpenFlow0x01_Core;;
open OpenFlow0x01;;

let debug = true;;

module Controller_Forwarding = struct

	let in_packet_context = ref None;;
	
	(* *)
	let remember_for_forwarding (pkt_info: (switchId * packetIn * Types.term) option) : unit = 
	  in_packet_context := pkt_info;;

	let begins_with (str1 : string) (str2 : string) : bool = 
		if String.length str2 > String.length str1 then false else
		(String.sub str1 0 (String.length str2)) = str2;;

	let get_field (notif : Types.term) (fieldname : string) : string =
		if debug then print_endline ("starting get_field with field " ^ fieldname);
		match notif with
		| Types.Constant(strings, Types.Type(_, fields)) ->
			let combined = List.combine fields strings in
			if debug then print_endline (Type_Helpers.list_to_string (function (str1, str2) -> str1 ^ ":" ^ str2) combined);
			if debug then print_endline ("Result: "^(List.assoc fieldname combined));
			List.assoc fieldname combined;
		| _ -> raise (Failure "for notification constants only");;

	let pkt_to_notif (sw : switchId) (pk : packetIn) : Types.term = 
		if debug then print_endline "starting pkt_to_notif";
		let pkt_payload = parse_payload pk.input_payload in
		if debug then print_endline "payload parsed";
		let isIp = ((dlTyp pkt_payload) = 0x0800) in
		let isArp = ((dlTyp pkt_payload) = 0x0806) in
		let strings = [Int64.to_string sw;
		string_of_int pk.port;
		Int64.to_string pkt_payload.Packet.dlSrc;
		Int64.to_string pkt_payload.Packet.dlDst;
		string_of_int (dlTyp pkt_payload);
		(* nwSrc/nwDst will throw an exception if you call them on an unsuitable packet *)
		if (isIp || isArp) then Int32.to_string (nwSrc pkt_payload) else "0";
		if (isIp || isArp) then Int32.to_string (nwDst pkt_payload) else "0";

		if isIp then (string_of_int (nwProto pkt_payload)) else "arp"] in
		(*let _ = if debug then print_endline ("pkt to term list: " ^ (Type_Helpers.list_to_string Type_Helpers.term_to_string ans)) in
		let _ = if debug then print_endline ("dlTyp: " ^ (string_of_int (dlTyp pkt_payload))) in*)
		if debug then print_endline "finishing pkt_to_notif";
		Types.Constant(strings, Types.packet_type);;

	let outgoing_packet_queue = ref [];;

	let queue_packets (out_notifs : Types.term list) : unit =
		outgoing_packet_queue := out_notifs @ !outgoing_packet_queue;;

    let of_pport_to_string (pp: pseudoPort) : string =
     match pp with      
      | PhysicalPort(id) -> "PhysicalPort: "^(string_of_int id);
      | AllPorts -> "AllPorts";
      | InPort -> "InPort";
      | Flood -> "Flood";
      | Controller(x) -> "Controller:"^(string_of_int x);;

    let sod = function
        Some v -> v
        | None -> -1;;

    let of_action_to_string (act : action) : string =
    match act with
     | Output(pseudoPort) -> "Output to "^(of_pport_to_string pseudoPort);
     | SetDlVlan(dlVlan) -> "Set dlVlan="^(string_of_int (sod dlVlan));
     | SetDlVlanPcp(dlVlanPcp) -> "Set dlVlanPcp="^(string_of_int dlVlanPcp);
     | SetDlSrc(dlAddr) -> "Set dlSrc="^(Int64.to_string dlAddr);
     | SetDlDst(dlAddr) -> "Set dlDst="^(Int64.to_string dlAddr);
     | SetNwSrc(nwAddr) -> "Set nwsrc="^(Int32.to_string nwAddr);
     | SetNwDst(nwAddr) -> "Set nwdst="^(Int32.to_string nwAddr);
     | SetNwTos(nwTos) -> "Set nwTos= "^(string_of_int nwTos);
     | SetTpSrc(tpPort) -> "Set tpSrc = "^(string_of_int tpPort);
     | SetTpDst(tpPort) -> "Set tpDst = "^(string_of_int tpPort);;


	(* notice that the current implementation is not efficient---
	   if its just a repeater its doing way too much work. *)

    (* the notifs set must be a set of packets with the same switch, but also the same payload.*)
	let send_queued_packets (notifs : Types.term list) 
	                        (sw: switchId) (pkt_payload: payload): unit =
	    if debug then Printf.printf "In send_queued_packets...\n%!";
		let actions_list = ref [] in
				
		(* for each switch, need to group ports to output and field rewrites to do.
		   Ox actions are imperative, so SetDlSrc(5) Output(3) SetDlSrc(4) Output(2) works the way you'd expect. *)
		List.iter (fun notif -> 
			(* no need to get the locSw, since the set of packets given will have a common switch *)
			let locPt_string = (get_field notif "LOCPT") in

			(*let dlSrc_new = (get_field notif "DLSRC") in
			let dlDst_new = (get_field notif "DLDST") in
			let nwSrc_new = (get_field notif "NWSRC") in
			let nwDst_new = (get_field notif "NWDST") in*)

			(* if (begins_with locPt_string "_h") then actions_list := Output(AllPorts) :: !actions_list else *)
			
			actions_list := Output(PhysicalPort(int_of_string locPt_string)) :: !actions_list;

			(*actions_list := SetDlSrc(Int64.of_string (if (begins_with dlSrc_new "_h") then dlSrc_old else dlSrc_new)) :: !actions_list;
	        actions_list := SetDlDst(Int64.of_string (if (begins_with dlDst_new "_h") then dlDst_old else dlDst_new)) :: !actions_list;
			actions_list := SetNwSrc(Int32.of_string (if (begins_with nwSrc_new "_h") then nwSrc_old else nwSrc_new)) :: !actions_list;
			actions_list := SetNwDst(Int32.of_string (if (begins_with nwDst_new "_h") then nwDst_old else nwDst_new)) :: !actions_list;
			*)
			(* TODO: avoid unnecessary Set* actions by remembering the last one uttered or the original.
			         which is a point... how expensive is it to modify, even once? *)

			(* TODO check: should throw an error if we get unrestricted header field except for port, right? *)
			
	        actions_list := SetDlSrc(Int64.of_string (get_field notif "DLSRC")) :: !actions_list;
			actions_list := SetDlDst(Int64.of_string (get_field notif "DLDST")) :: !actions_list;
			actions_list := SetNwSrc(Int32.of_string (get_field notif "NWSRC")) :: !actions_list;
			actions_list := SetNwDst(Int32.of_string (get_field notif "NWDST")) :: !actions_list;
			
			()) notifs;

		if debug then 
		begin 
          (List.iter (fun act -> (Printf.printf "---ACTION: %s\n%!" (of_action_to_string act))) !actions_list);
          if (List.length !actions_list) = 0 then
            Printf.printf "---NO ACTIONS! Packet will be dropped.\n%!";
        end;

        (* Since send_packet_out needs the switch, and we may have multiple switches... *)
        send_packet_out sw 0l 
	      {output_payload = pkt_payload; port_id = None; apply_actions = !actions_list};;


    let manufacture_payload() : OpenFlow0x01_Core.payload = 
      NotBuffered(Packet.marshal(
       {Packet.dlSrc = Int64.of_int 0; Packet.dlDst = Int64.of_int 0;
        Packet.dlVlan = None; Packet.dlVlanPcp = 0;
        nw = Packet.Unparsable(0x801, Cstruct.create(0))
       }));;

    let debufferize (pl : payload): payload =
      match pl with
      | NotBuffered(_) -> pl;
      | Buffered(_, b) -> NotBuffered(b);;

    let split_packets_by_switch (pkts : Types.term list): (switchId, Types.term list) Hashtbl.t =
      let sofar = Hashtbl.create(1) in
    	List.iter (fun pkt  -> let locsw = (Int64.of_string (get_field pkt "LOCSW")) in
    		                     if (Hashtbl.mem sofar locsw) then
                                    Hashtbl.add sofar locsw (pkt :: (Hashtbl.find sofar locsw)) 
                                 else
                                    Hashtbl.add sofar locsw [pkt])
    	                pkts;
    	sofar;;

    (* Queue up all packets to be sent and send at once *)
	let flush_packets () : unit =
	    if debug then Printf.printf "In flush_packets..\n%!";
	    if !outgoing_packet_queue <> [] then
	    begin
          
          if debug then 
          begin
   		    (match !in_packet_context with
		  	  | None -> Printf.printf "Flushing... NO INITIAL PACKET!\n%!";
		              
		      | Some(sw, in_pk, notif) -> 
		        Printf.printf "Flushing... FORWARDING PACKET. Switch=%s, Fields= %s\n%!" 
		          (Int64.to_string sw) 
		          (Packet.to_string (parse_payload in_pk.input_payload)));
		    Printf.printf "There are %d packets in the queue.\n%!" (List.length !outgoing_packet_queue);
		  end;

		(* TODO: error message if attempting to mutate a field that can't be mutated. e.g. dlTyp. 
		   IT can be set in emit, but not changed in forward. *)

          (* Every packet in the queue has a dest switch. Group by those,
             since we can only call send_packet_out ONCE per switch for a buffered pkt. *)
		  let mapSwToPkts = (split_packets_by_switch !outgoing_packet_queue) in 
            Hashtbl.iter (fun swId pkts -> 
               		        Printf.printf "  There are %d packets for switch %Ld\n%!" (List.length pkts) swId;
               		        let pkt_payload = (match !in_packet_context with
		  	                (* If this is a pure emit (no payload; create one) *)
		                    | None -> manufacture_payload();		
		                    (* If there is a payload to copy over, and the switch is the same, BUFFER *)              		
		      				| Some(in_sw, in_pk, _) -> if in_sw = swId then in_pk.input_payload
		      				(* If there is a payload to copy over, but the switch is different, UNBUFFER *)
		      			                            else debufferize in_pk.input_payload) in 

               		        
                            send_queued_packets pkts swId pkt_payload) 
              mapSwToPkts;

		  in_packet_context := None;
		  outgoing_packet_queue := [];
		end;;

end

