open Printf
open OpenFlow0x01Types
open WordInterface
open Packet.Types
open NetCore_Syntax
open Misc

module Controller =
  struct
  
  let (policy, push) = Lwt_stream.create ()
  
  let rec make_policy () = 
      Pol (All, [GetPacket process_packet])

  and process_packet sw pt pk : unit =
      (* stuff here *)
      push (Some (make_policy ()))

  let _ = push (Some (make_policy ()))

end