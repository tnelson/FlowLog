(****************************************************************)
(* Proxy a switch to interact with other OF controllers.        *)
(****************************************************************)

open Flowlog_Types
open Flowlog_Packets
open Flowlog_Helpers
open Printf
open OpenFlow0x01
open OpenFlow0x01_Core
open OpenFlow0x01_Switch
open Message
open Lwt_unix
open ExtList.List
open NetCore_Pattern
open Partial_Eval

open OpenFlow0x01.SwitchFeatures.Capabilities;;
open OpenFlow0x01.SwitchFeatures.SupportedActions;;
open OpenFlow0x01.SwitchFeatures;;
open OpenFlow0x01.PortDescription;;

module Socket = Socket

type msg = Message.t;;

let nocaps = { flow_stats=false
      ; table_stats=false
      ; port_stats=false
      ; stp=false
      ; ip_reasm=false
      ; queue_stats=false
      ; arp_match_ip=false
      };;

let noacts = { output=false
      ; set_vlan_id=false
      ; set_vlan_pcp=false
      ; strip_vlan=false
      ; set_dl_src=false
      ; set_dl_dst=false
      ; set_nw_src=false
      ; set_nw_dst=false
      ; set_nw_tos=false
      ; set_tp_src=false
      ; set_tp_dst=false
      ; enqueue=false
      ; vendor=false};;

let swid_to_fd: (switchId * Lwt_unix.file_descr) list ref = ref [];;

let string_of_sockaddr (sa: sockaddr): string =
  match sa with
    | ADDR_UNIX str -> str
    | ADDR_INET(addr, pt) -> (Unix.string_of_inet_addr addr)^":"^(string_of_int pt);;

(***********************************************************************************)

let rec terminate_pin_connection_thread (servicefd: Lwt_unix.file_descr): unit Lwt.t =
  printf "Closing connection fd...\n%!";
  Lwt_unix.close servicefd;;

let rec wait_for_features_request (servicefd: Lwt_unix.file_descr): unit Lwt.t =
  match_lwt (OpenFlow0x01_Switch.recv_from_switch_fd servicefd) with
    | Some (_, SwitchFeaturesRequest) ->
      Lwt.return ()
    | Some(_,_) ->
      wait_for_features_request servicefd;
    | None ->
      printf "recv_from_switch_fd returned None.\n%!";
      Lwt.return ();;

let rec wait_for_hello (servicefd: Lwt_unix.file_descr): unit Lwt.t =
  (* Expect hello -> send hello -> send features request -> expect features reply *)
  match_lwt (OpenFlow0x01_Switch.recv_from_switch_fd servicefd) with
    | Some (_, Hello(_)) ->
      Lwt.return();
    | Some(_,_) ->
      wait_for_hello servicefd
    | None ->
      printf "recv_from_switch_fd returned None.\n%!";
      Lwt.return ();;

let rec send_features_reply (servicefd: Lwt_unix.file_descr) (swid: int64): unit Lwt.t =
  let features_message = SwitchFeaturesReply(
      {switch_id=swid; num_buffers= Int32.zero; num_tables=0;
       supported_capabilities=nocaps;
       supported_actions=noacts;
       ports=[];}) in
    match_lwt OpenFlow0x01_Switch.send_to_switch_fd servicefd (Int32.of_int 0) features_message with
      | true -> Lwt.return ()
      | false -> failwith "send_features_reply";;

let rec switch_listener (prgm: flowlog_program) (swid: switchId) (servicefd: Lwt_unix.file_descr): unit Lwt.t =
  match_lwt (OpenFlow0x01_Switch.recv_from_switch_fd servicefd) with
    | Some (xid, msg) ->
      printf "received from descriptor: \n%!";
      printf "  xid: %d\n%!" (Int32.to_int xid);
      printf "  msg: %s\n%!" (Message.to_string msg);
      (match msg with
        | PacketInMsg(pin) ->
          printf "  pkt_in: %s\n%!" (packetIn_to_string pin);
          printf "(ignoring since packet_in unexpected as proxy switch\n%!";
          switch_listener prgm swid servicefd
        | PacketOutMsg(pout) ->
          printf "  pkt_out: %s\n%!" (PacketOut.to_string pout);

(* output_payload : payload
    ; port_id : portId option
    ; apply_actions : action list*)

          let pkt = (match pout.output_payload with
             | NotBuffered(bytes) ->
                Packet.parse bytes
              | Buffered(_, bytes) ->
                printf "~~ Payload was buffered; should not have been. Ignoring buffer...\n%!";
                Packet.parse bytes) in
          let in_pt = (match pout.port_id with | None -> Physical(Int32.zero) | Some p -> Physical((Int32.of_int p))) in

          let notif = (pkt_to_event swid in_pt pkt) in
            respond_to_notification prgm notif;

            (* may be multiple actions = multi-record problem with events *)
            printf "WARNING! ACTIONS IN PACKET_OUT WILL BE IGNORED BY FLOWLOG.\n%!";


          switch_listener prgm swid servicefd
        | _ -> switch_listener prgm swid servicefd)
    | None ->
      printf "recv_from_switch_fd returned None.\n%!";
      Lwt.return ();;

  (*Lwt.async (fun () -> listen_for_packet_ins_thread servicefd prgm >> terminate_pin_connection_thread servicefd);*)

(***********************************************************************************)

let init_connection_as_switch (swid: int64) (dstip: inet_addr) (dstport: int): (int * Lwt_unix.file_descr) =
  printf "Opening socket for proxy switch with id %Ld...\n%!";
  let fd = socket PF_INET SOCK_STREAM 0 in
    setsockopt fd SO_REUSEADDR true;
    bind fd (ADDR_INET (Unix.inet_addr_any, 0));
    let newport = (match getsockname fd with | ADDR_INET(_, p) -> p | _ -> failwith "init_connection: non ADDR_INET") in
      connect fd (ADDR_INET(dstip, dstport));
      printf "connected to controller at %s port %d.\n%!" (Unix.string_of_inet_addr dstip) dstport;

      (* register switch with the appropriate dpid. *)
      (* step 1: send hello message *)
      let hello_message = Hello(Cstruct.create 0) in
        send_to_switch_fd fd (Int32.of_int 0) hello_message;

      (* TODO step 2: hold and wait for HELLO reply, then features request *)
      wait_for_hello fd;
      wait_for_features_request fd;
      (* TODO step 3: send features reply with DPID *)
      send_features_reply fd swid;

      printf "switch registered.\n%!";
      (* return the actual port used *)
      (newport, fd);;

let shutdown_switch_listener (swid: switchId): unit Lwt.t =
  if mem_assoc swid !swid_to_fd then
    Lwt_unix.close (assoc swid !swid_to_fd)
  else
    Lwt.return ();;

let register_proxy_switch (prgm: flowlog_program) (swid: switchId) (ips: string) (pts: string): Lwt_unix.file_descr =
  match mem_assoc swid !swid_to_fd with
    | true ->
      (* do nothing; switch already registered *)
      assoc swid !swid_to_fd
    | false ->
      (* get a fresh port, spin off a LWT to listen on that port. record the FD so we can output on it. *)
      (* Since we (the proxy switch) always initiates the connection to the controller, we don't need a
         connection-listener, just a listener for packet_outs etc. *)

      let ips_dotted = (if String.contains ips '.' then
        ips
      else
        (Packet.string_of_ip (nwaddr_of_int_string ips))) in

      let (newport, fd) = init_connection_as_switch swid (Unix.inet_addr_of_string ips_dotted) (int_of_string pts) in
        swid_to_fd := (swid, fd)::!swid_to_fd;
        Lwt.async (fun () -> (switch_listener prgm swid fd) >> (shutdown_switch_listener swid));
        fd;;

(***********************************************************************************)

let doSendPacketIn (prgm: flowlog_program) (ev: event) (controller_ip: string) (controller_port_s: string) : unit =
  let locsw = (Int64.of_string (get_field ev "locsw")) in
  let fd = register_proxy_switch prgm locsw controller_ip controller_port_s in

  (* Start with direct copy of packet (possible change of switchid) *)
  (* Assume: only triggered by packet arrival from OF. will not be correct even if packet arrives from CP. *)

  printf "sending packet_in from switch id = %Ld\n%!" locsw;
  let incsw, incpt, incpkt, incbuffid = match !last_packet_received_from_dp with
    | Some(a,b,c,d) -> (a, b, c, d)
    | _ -> failwith "no src packet" in

  (* let pktin_msg = !last_packet_arrived in*)
  let pktin_msg = PacketInMsg({input_payload = NotBuffered(Packet.marshal incpkt);
                                       total_len = Packet.len incpkt;
                                       port = (int_of_string (get_field ev "locpt"));
                                       reason = ExplicitSend}) in

    let _ = send_to_switch_fd fd (Int32.of_int 0) pktin_msg in
      ();;

(***********************************************************************************)