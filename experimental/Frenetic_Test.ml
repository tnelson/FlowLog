open NetCore_Types
open Printf
open Packet
open OpenFlow0x01
open Lwt
open Pa_lwt
open NetCore_Pattern
open NetCore_Wildcard
open NetCore_Controller

let make () = 

    printf "Making...\n%!";

    (* stream of policies, with function to push new policies on *)
	let (policies, push) = Lwt_stream.create () in

	let updateFromPacket (sw: switchId) (pt: port) (pkt: Packet.packet) : action =
		(* Update the policy (how?) *)

		(* Do nothing more *) 
		[] in

	(* Policy to send some packets to testhandler *)
	let apred = Hdr({ptrnDlSrc = WildcardAll;
  	ptrnDlDst = WildcardAll;
  	ptrnDlTyp = WildcardExact(1);
  	ptrnDlVlan = WildcardAll;
  	ptrnDlVlanPcp = WildcardAll;
  	ptrnNwSrc = WildcardAll;
  	ptrnNwDst = WildcardAll;
  	ptrnNwProto = WildcardAll;
  	ptrnNwTos = WildcardAll;
  	ptrnTpSrc = WildcardAll;
  	ptrnTpDst = WildcardAll;
  	ptrnInPort = WildcardAll}) in

	let initpol: pol = ITE(apred,
	                Action([ControllerAction(updateFromPacket)]), 
	                Action([])) in

    printf "policy is:\n%s\n%!" (NetCore_Pretty.string_of_pol initpol);

    (* cargo-cult hacking invocation. why call this? *)
    NetCore_Stream.from_stream initpol policies;;
(*    (initpol, policies);;*)


let listenPort = ref 6633;;

let main () = 
    (* >> is anonymous bind from Pa_lwt. So why isn't this working? *)	 
      OpenFlow0x01_Platform.init_with_port !listenPort >>
        let (gen_stream, stream) = make()  in
        (* streams for incoming/exiting packets *)
        let (pkt_stream, push_pkt) = Lwt_stream.create () in
        Lwt.pick [gen_stream; NetCore_Controller.start_controller pkt_stream stream];;

Lwt_main.run (main ())




		
        

