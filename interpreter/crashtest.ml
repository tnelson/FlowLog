open Surface_Parser
open Surface_Lexer
open Flowlog_Types
open Flowlog_Helpers
open Partial_Eval
open Printf
open Arg
open Flowlog_To_Alloy

open NetCore_Types
open Packet
open OpenFlow0x01
open OpenFlow0x01_Platform
open OpenFlow0x01_Core
open Lwt
open NetCore_Pattern
open NetCore_Wildcard
open NetCore_Controller

open Xsb_Communication

open ExtList.List

let listenPort = ref 6633;;

let make_policy () = 

  let (policies, push) = Lwt_stream.create () in

  (* The callback to be invoked when the policy says to send pkt to controller *)
  let rec updateFromPacket (sw: switchId) (pt: port) (pkt: Packet.packet) : NetCore_Types.action =  
    printf "packet in sw %Ld pt %s: %s\n%!" sw (NetCore_Pretty.string_of_port pt) (Packet.to_string pkt);

    Thread.delay 1.00; (* eventually the number of packets waiting for processing gets too big*)

    (*let newpol = Action([allportsatom; ControllerAction(updateFromPacket)]) in 
        push (Some newpol);*)
        printf "pushed. returning now.\n%!";
        [allportsatom] 

      and

  switch_event_handler (swev: switchEvent): unit =
      match swev with
      | SwitchUp(sw, feats) ->                 
        printf "SWITCH %Ld connected. Ports: %s. Buffers: %d. Tables: %d\n%!" 
          sw
          (String.concat ";" (map Int32.to_string feats.ports))
          feats.num_buffers
          feats.num_tables;        
        

      | SwitchDown(swid) -> 
          printf "SWITCH %Ld went down.\n%!" swid;
    in

  let swpol = HandleSwitchEvent(switch_event_handler) in
  (*let pktpol = Action([ControllerAction(updateFromPacket); allportsatom]) in*)
  let pktpol = Action([ControllerAction(updateFromPacket)]) in

    NetCore_Stream.from_stream (Union(swpol, pktpol)) policies;;


  
let run_flowlog (): unit Lwt.t =  

  (* Start the policy stream *)
  (* >> is from Lwt's Pa_lwt. But you MUST have -syntax camlp4o or it won't be recoginized. *)   
  OpenFlow0x01_Platform.init_with_port !listenPort >>
    let (gen_stream, stream) = make_policy () in

    (* streams for incoming/exiting packets *)
    let (pkt_stream, push_pkt) = Lwt_stream.create () in        

      (* pick cancels all threads given if one terminates *)             
      (* DO NOT attempt to copy ox/frenetic's switch connection detection code here. It will clash with 
         Frenetic's. Instead, register a HandleSwitchEvent policy, which gives us a nice clean callback. *)
      Lwt.pick [gen_stream; NetCore_Controller.start_controller pkt_stream stream];;


let main () =
    printf "-----------\n%!";

      (* Intercede when Ctrl-C is pressed to close XSB, etc. *)
      Sys.catch_break true;
      (* If SIGPIPE ("broken pipe") failure (exit code 141), actually give an error. 
         Without this set, the program terminates with no message. *)
      Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
      try      
        Lwt_main.at_exit (fun () -> return (printf "LWT exiting~\n%!") );
        at_exit (fun () -> (printf "Ocaml exiting~\n%!"));        
        Lwt_main.run (run_flowlog ());     
        printf "LWT Terminated!\n%!";
      with
        | Sys.Break ->                  
          printf "\nExiting gracefully due to break signal.\n%!";
          exit 101
        | exn ->        
        Format.printf "\nUnexpected exception: %s\n%s\n%!"
          (Printexc.to_string exn)
          (Printexc.get_backtrace ());        
        exit 100;;
    
 main();;


