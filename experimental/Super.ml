open OxPlatform
open OpenFlow0x01_Core
open Int64

module MyApplication = struct

  include OxStart.DefaultTutorialHandlers

  let debufferize (pl : payload): payload =
    match pl with
    | NotBuffered(_) -> pl;
    | Buffered(_, b) -> NotBuffered(b);;

(*type nw =
  | Ip of Ip.t
  | Arp of Arp.t
  | Unparsable of (dlTyp * bytes)

type packet = {
  dlSrc : dlAddr;
  dlDst : dlAddr; 
  dlVlan : dlVlan;
  dlVlanPcp : dlVlanPcp;
  nw : nw
}*)

  (* beware: multiple "payload" types... *)
  (* Produce a NotBuffered containing bits for the specified ethernet packet: 
     note that it's not an IP packet; the bits are unparsable inside the layer2 envelope.
     the first arg to unparsable constructor contains the dlTyp field. *)
  let manufacture_payload_for_packet_out() : OpenFlow0x01_Core.payload = 
    NotBuffered(Packet.marshal(
       {Packet.dlSrc = Int64.of_int 100; Packet.dlDst = Int64.of_int 101;
        Packet.dlVlan = None; Packet.dlVlanPcp = 103;
        nw = Packet.Unparsable(1000, Cstruct.create(0))
       }));;

  (* this will cause a packet storm! *)
  (* what is the meaning of AllPorts if not buffered? *)
  let handle_packet (othsw: switchId) (xid : xid) (pk : packetIn) : unit =
    Printf.printf "Unbuffered... out: %s\n%!" (to_string othsw);
    send_packet_out othsw 0l
      { (*output_payload = debufferize pk.input_payload;*)
        output_payload = manufacture_payload_for_packet_out();
        port_id = None;
        apply_actions = [Output AllPorts] 
      }      

  let packet_in (sw : switchId) (xid : xid) (pk : packetIn) : unit =
    Printf.printf "At switch: %Ld := %s\n%!" sw (packetIn_to_string pk);
    
    (* send out of each switch *)
    List.iter (fun asw -> if sw <> asw then handle_packet asw xid pk) 
              [Int64.of_int 1;Int64.of_int 2;Int64.of_int 3]


end

module Controller = OxStart.Make (MyApplication)

