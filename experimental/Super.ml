open OxPlatform
open OpenFlow0x01_Core
open Int64

module MyApplication = struct

  include OxStart.DefaultTutorialHandlers

  let debufferize (pl : payload): payload =
    match pl with
    | NotBuffered(_) -> pl;
    | Buffered(_, b) -> NotBuffered(b);;

  let handle_packet (othsw: switchId) (xid : xid) (pk : packetIn) : unit =
    Printf.printf "Unbuffered...\n%!";
    send_packet_out othsw 0l
      { output_payload = debufferize pk.input_payload;
        port_id = None;
        apply_actions = [Output AllPorts] 
      }      

  let packet_in (sw : switchId) (xid : xid) (pk : packetIn) : unit =
    Printf.printf "At switch: %Ld := %s\n%!" sw (packetIn_to_string pk);

    (* buffered *)
   (* Printf.printf "Buffered...\n%!";
    send_packet_out sw 0l
      { output_payload = pk.input_payload;
        port_id = None;
        apply_actions = [Output AllPorts] 
      };*)

    (* unbuffered *)
    
    List.iter (fun asw -> if sw <> asw then handle_packet asw xid pk) 
              [(of_string "1");(of_string "2");(of_string "3")]


end

module Controller = OxStart.Make (MyApplication)

