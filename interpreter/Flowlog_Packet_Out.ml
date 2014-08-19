(****************************************************************)
(* Testing sending packet_in on alternative ports         *)
(****************************************************************)

open NetCore_Types
open Printf
open OpenFlow0x01
open OpenFlow0x01_Core
open OpenFlow0x01_Switch
open Message
open Lwt_unix

(* For marshalling *)
open Flowlog_Packets

module Socket = Socket

let otherListenPort = ref 9999;;
let myPort = ref 19999;;

(*
type payload =
  | Buffered of int32 * bytes 
  | NotBuffered of bytes

type packetInReason =
  | NoMatch
  | ExplicitSend
  
type packetIn =
    { input_payload : payload
    ; total_len : int16
    ; port : portId
    ; reason : packetInReason
    }
*)

(* 
buffer id = -1 : not buffered
*)


let to_send_packet  = {Packet.dlSrc = Int64.of_int 1; Packet.dlDst = Int64.of_int 2;
                    Packet.dlVlan = None; Packet.dlVlanPcp = 0;
                    Packet.dlVlanDei = false; nw = Packet.Unparsable(100, Cstruct.create(0))};;

let to_send_message = PacketInMsg({input_payload = NotBuffered(Packet.marshal to_send_packet); 
                                       total_len = Packet.len to_send_packet;
                                       port = 0;
                                       reason = ExplicitSend});;

(***************************************************)

(* We need to create a persistent connection, since we may pass through POX
  (etc.) on the way to whatever application we're interfacing with.

   *)

let switch_fd : file_descr option ref = ref None;;

let init_with_fd (fd : file_descr) : unit =
  match !switch_fd with
  | Some _ ->
    raise (Invalid_argument "Platform already initialized")
  | None ->
    switch_fd := Some fd;;

let get_fd () : file_descr =
  match !switch_fd with
    | Some fd ->
      fd
    | None ->
      raise (Invalid_argument "Platform not initialized");;

let init_connection () = 
  printf "opening connection...\n%!";
  let fd = socket PF_INET SOCK_STREAM 0 in
    setsockopt fd SO_REUSEADDR true;
    bind fd (ADDR_INET (Unix.inet_addr_any, !myPort));
    init_with_fd fd;
    printf "initialized and bound\n%!";
    connect fd (ADDR_INET(Unix.inet_addr_loopback, !otherListenPort));
    printf "connected!\n%!";;
    

(* inet_addr_of_string *)

init_connection();;

(* send_to_switch_fd (sock : file_descr) (xid : xid) (msg : msg) : bool Lwt.t*)

send_to_switch_fd (get_fd()) (Int32.of_int 0) to_send_message;;

close (get_fd());;


(* BUG: can't re-send. listener improperly set up? (never gets back to listening?) *)