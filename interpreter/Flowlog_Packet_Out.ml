(****************************************************************)
(* Testing sending packet_in on alternative ports         *)
(****************************************************************)

open NetCore_Types
open Printf
open OpenFlow0x01
open OpenFlow0x01_Core
open Message
open Lwt_unix

(* For marshalling *)
open Flowlog_Packets

module Socket = Socket

let otherListenPort = ref 9999;;

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

let to_send_packet_in_bits = Message.marshal (Int32.of_int 1) to_send_message;;

(***************************************************)

