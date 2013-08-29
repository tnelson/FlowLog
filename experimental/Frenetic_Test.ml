open NetCore_Types
open Printf
open Packet
open OpenFlow0x01
(*open OpenFlow0x01_Core*)
open Lwt
open NetCore_Pattern
open NetCore_Wildcard
open NetCore_Controller

let make () = 

    printf "Making policy and stream...\n%!";

    (* stream of policies, with function to push new policies on *)
	let (policies, push) = Lwt_stream.create () in

	let updateFromPacket (sw: switchId) (pt: port) (pkt: Packet.packet) : NetCore_Types.action =
		

    (* Update the policy via the push function *)

    printf "Packet in on switch %Ld.\n%s\n%!" sw (to_string pkt);

  let allnomod = {
    outDlSrc = None;
    outDlDst = None;
    outDlVlan = None;
    outDlVlanPcp = None;
    outNwSrc = None;
    outNwDst = None;
    outNwTos = None;
    outTpSrc = None;
    outTpDst = None;
    outPort = All; } in 

    (*push (Some (Action([])));*)
    let newpol = (Action([SwitchAction(allnomod)])) in
      push (Some newpol);
    
		 printf "policy is:\n%s\n%!" (NetCore_Pretty.string_of_pol newpol);


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
	                Action([ControllerAction(updateFromPacket)])) in

    printf "policy is:\n%s\n%!" (NetCore_Pretty.string_of_pol initpol);

    (* cargo-cult hacking invocation. why call this? *)
    NetCore_Stream.from_stream initpol policies;;
(*    (initpol, policies);;*)


let listenPort = ref 6633;;

let main () = 
    (* >> is from Lwt's Pa_lwt. But you MUST have -syntax camlp4o or it won't be recoginized. *)	 
      OpenFlow0x01_Platform.init_with_port !listenPort >>
        let (gen_stream, stream) = make()  in
        (* streams for incoming/exiting packets *)
        let (pkt_stream, push_pkt) = Lwt_stream.create () in
        Lwt.pick [gen_stream; NetCore_Controller.start_controller pkt_stream stream];;

Lwt_main.run (main ())




		
        

