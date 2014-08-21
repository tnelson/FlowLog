(****************************************************************)
(* Testing sending packet_in on alternative ports         *)
(****************************************************************)

(*open NetCore_Types*)
open Flowlog_Types
open Flowlog_Helpers
open Printf
open OpenFlow0x01
open OpenFlow0x01_Core
open OpenFlow0x01_Switch
open Message
open Lwt_unix
open ExtList.List
open Packet

open OpenFlow0x01.SwitchFeatures.Capabilities;;
open OpenFlow0x01.SwitchFeatures.SupportedActions;;
open OpenFlow0x01.PortDescription;;

(* For marshalling *)
open Flowlog_Packets

module Socket = Socket

let to_send_packet1  = {Packet.dlSrc = Int64.of_int 1; Packet.dlDst = Int64.of_int 2;
                    Packet.dlVlan = None; Packet.dlVlanPcp = 0;
                    Packet.dlVlanDei = false; nw = Packet.Unparsable(1000, Cstruct.create(0))};;

let to_send_packet2  = {Packet.dlSrc = Int64.of_int 100; Packet.dlDst = Int64.of_int 200;
                    Packet.dlVlan = None; Packet.dlVlanPcp = 0;
                    Packet.dlVlanDei = false; nw = Packet.Unparsable(2000, Cstruct.create(0))};;


let to_send_message1 = PacketInMsg({input_payload = NotBuffered(Packet.marshal to_send_packet1); 
                                       total_len = Packet.len to_send_packet1;
                                       port = 0;
                                       reason = ExplicitSend});;

let to_send_message2 = PacketInMsg({input_payload = NotBuffered(Packet.marshal to_send_packet2); 
                                       total_len = Packet.len to_send_packet2;
                                       port = 0;
                                       reason = ExplicitSend});;


(***************************************************)

(* We need to create a persistent connection, since we may pass through POX
  (etc.) on the way to whatever application we're interfacing with.
   *)

let source_fds : (int * file_descr) list ref = ref [];;

let init_with_fd (pt: int) (fd : file_descr) : unit =
  if mem_assoc pt !source_fds then  
    raise (Invalid_argument "Already initialized for port")
  else  
    source_fds := (pt, fd)::!source_fds;;

let get_fd (pt: int) : file_descr =
  if mem_assoc pt !source_fds then  
    assoc pt !source_fds 
  else  
    raise (Invalid_argument "No file-descriptor stored for port");;

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

  (*type t =
    { switch_id : int64
    ; num_buffers : int32
    ; num_tables : int8
    ; supported_capabilities : Capabilities.t
    ; supported_actions : SupportedActions.t
    ; ports : PortDescription.t list }*)

(*
    type t =
      { flow_stats : bool (** Flow statistics. *)
      ; table_stats : bool (** Table statistics. *)
      ; port_stats : bool (** Port statistics. *)
      ; stp : bool (** 802.1D spanning tree. *)
      ; ip_reasm : bool (** Can reassemble IP fragments. *)
      ; queue_stats : bool (** Queue statistics. *)
      ; arp_match_ip : bool (** Match IP addresses in ARP packets. *)
      }
*)

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


(* Beware conflict with This expression has type NetCore_Types.capabilities
       but an expression was expected of type
         OpenFlow0x01.SwitchFeatures.Capabilities.t
*)


let rec send_features_reply (servicefd: Lwt_unix.file_descr) (swid: int64): unit Lwt.t =  
  let features_message = SwitchFeaturesReply(
      {switch_id=swid; num_buffers= Int32.zero; num_tables=0;
       supported_capabilities=nocaps;
       supported_actions=noacts;
       ports=[];}) in      
    match_lwt OpenFlow0x01_Switch.send_to_switch_fd servicefd (Int32.of_int 0) features_message with
      | true -> Lwt.return ()          
      | false -> failwith "wait_for_features_reply";;

let init_connection (swid: int64) (sourceport: int) (dstip: inet_addr) (dstport: int): int = 
  printf "opening connection...\n%!";
  let fd = socket PF_INET SOCK_STREAM 0 in
    setsockopt fd SO_REUSEADDR true;
    bind fd (ADDR_INET (Unix.inet_addr_any, sourceport));
    let newport = (match getsockname fd with | ADDR_INET(_, p) -> p | _ -> failwith "init_connection: non ADDR_INET") in
      init_with_fd newport fd;
      printf "initialized and bound\n%!";
      connect fd (ADDR_INET(dstip, dstport));
      printf "connected!\n%!";

      (* register switch with the appropriate dpid. *)
      (* step 1: send hello message *)
      let hello_message = Hello(Cstruct.create 0) in      
        send_to_switch_fd fd (Int32.of_int 0) hello_message;
      
      (* TODO step 2: hold and wait for HELLO reply, then features request *)
      wait_for_hello fd;
      wait_for_features_request fd;
      (* TODO step 3: send features reply with DPID *)
      send_features_reply fd swid;

      (* return the actual port used *)
      newport;;
    
(* 
  Location of external controller is cpip:cpport 
  Assume: ev is a packet type
  
  Assume: have a full packet buffered on the controller already, 
  and only have to do some field modifications before sending.
 
  Todo: check types match (e.g. incoming arp packet vs outgoing ip packet)

  Todo: no field modifications performed! (possible switch id change) 
*)

let last_packet_received_from_dp: (int64 * int32 * Packet.packet * int32 option) option ref = ref None;;

let swid_to_port: (int * int) list ref = ref [];;

let set_last_packet_received (sw: switchId) (ncpt: NetCore_Pattern.port) (pkt: Packet.packet) (buf: int32 option): unit =
  match ncpt with
    | NetCore_Pattern.Physical(x) -> last_packet_received_from_dp := Some (sw, x, pkt, buf) 
    | _ -> failwith "set_last_packet_received";;

let doSendPacketIn (ev: event) (cpip: string) (cpport_s: string) : unit =
  let cpport = (int_of_string cpport_s) in
  let locsw = (int_of_string (get_field ev "locsw")) in

  let srcpt = if mem_assoc locsw !swid_to_port then
    assoc locsw !swid_to_port
  else begin    
    let newport = init_connection (Int64.of_int locsw) 0 (Unix.inet_addr_of_string (Packet.string_of_ip (nwaddr_of_int_string cpip))) cpport in     
    swid_to_port := (locsw, newport)::!swid_to_port;    
    newport
  end in

  (* Start with direct copy of packet (possible change of switchid) *)
  (* Assume: only triggered by packet arrival from OF. will not be correct even if packet arrives from CP. *)
  
  printf "DEBUG: sending packet_in from source port = %d\n%!" srcpt;
  let incsw, incpt, incpkt, incbuffid = match !last_packet_received_from_dp with 
    | Some(a,b,c,d) -> (a, b, c, d) 
    | _ -> failwith "no src packet" in

  (* let pktin_msg = !last_packet_arrived in*)
  let pktin_msg = PacketInMsg({input_payload = NotBuffered(Packet.marshal incpkt); 
                                       total_len = Packet.len incpkt;
                                       port = (int_of_string (get_field ev "locpt"));
                                       reason = ExplicitSend}) in      

    let _ = send_to_switch_fd (get_fd srcpt) (Int32.of_int 0) pktin_msg in  
      ();;

let close_all_packet_connections () = 
  Lwt_main.run (Lwt_list.iter_p (fun (spt, sfd) -> Lwt_unix.close sfd) !source_fds);; 


(********************************************************************)
(* Test for Flowlog's CP packet reception. *)
let test_send_packets () =
  last_packet_received_from_dp := Some (Int64.of_int 1, Int32.of_int 1, to_send_packet1, None);  
  doSendPacketIn {typeid="packet"; values=construct_map [("locsw", "1");("locpt", "2")]} 
                 "127.0.0.1" "9999";
  last_packet_received_from_dp := Some (Int64.of_int 1, Int32.of_int 2, to_send_packet2, None);  
  doSendPacketIn {typeid="packet"; values=construct_map [("locsw", "1");("locpt", "2")]} 
                 "127.0.0.1" "9999";
  close_all_packet_connections();;

