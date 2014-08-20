(****************************************************************)
(* Testing sending packet_in on alternative ports         *)
(****************************************************************)

open NetCore_Types
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

let init_connection (sourceport: int) (dstip: inet_addr) (dstport: int): int = 
  printf "opening connection...\n%!";
  let fd = socket PF_INET SOCK_STREAM 0 in
    setsockopt fd SO_REUSEADDR true;
    bind fd (ADDR_INET (Unix.inet_addr_any, sourceport));
    let newport = (match getsockname fd with | ADDR_INET(_, p) -> p) in
      init_with_fd newport fd;
      printf "initialized and bound\n%!";
      connect fd (ADDR_INET(dstip, dstport));
      printf "connected!\n%!";
      newport;;
    

let test() = 
  let srcpt = init_connection 0 Unix.inet_addr_loopback 9999 in 
  (* test multiple messages in one connection *)
  let _ = send_to_switch_fd (get_fd srcpt) (Int32.of_int 0) to_send_message1 in
  let _ = send_to_switch_fd (get_fd srcpt) (Int32.of_int 0) to_send_message2 in
  close (get_fd srcpt);;

(* 
  Location of external controller is cpip:cpport 
  Assume: ev is a packet type
  
  Assume: have a full packet buffered on the controller already, 
  and only have to do some field modifications before sending.
 
  Todo: check types match (e.g. incoming arp packet vs outgoing ip packet)

  Todo: no field modifications performed! (possible switch id change) 
*)

let swid_to_port: (int * int) list ref = ref [];;

let doSendPacketIn (ev: event) (cpip: string) (cpport_s: string) : unit =
  let cpport = (int_of_string cpport_s) in
  let locsw = (int_of_string (get_field ev "locsw")) in

  let srcpt = if mem_assoc locsw !swid_to_port then
    assoc locsw !swid_to_port
  else begin    
    let newport = init_connection 0 (Unix.inet_addr_of_string (Packet.string_of_ip (nwaddr_of_int_string cpip))) cpport in     
    swid_to_port := (locsw, newport)::!swid_to_port;
    newport
  end in

  (* Start with direct copy of packet (possible change of switchid) *)
  (* Assume: only triggered by packet arrival from OF. will not be correct even if packet arrives from CP. *)
  

  (* let pktin_msg = !last_packet_arrived in*)
  let pktin_msg = to_send_message1 in
    let success = send_to_switch_fd (get_fd srcpt) (Int32.of_int 0) pktin_msg in  
      ();;

let close_all_packet_connections () = 
  Lwt_main.run (Lwt_list.iter_p (fun (spt, sfd) -> Lwt_unix.close sfd) !source_fds);; 
