open Types;;
open Controller_Forwarding;;
open Xsb_Communication;;
open Type_Helpers;;
open Flowlog_Thrift_Out;;
open OpenFlow0x01_Core;;

let debug = true;;

(* Provides functions for running a Flowlog program.*)
module Evaluation = struct

	let send_notifications (bb : Types.blackbox) (out_notifs : Types.term list) : unit =
		if debug then List.iter (fun out_notif -> print_endline ("outgoing notif: " ^ Type_Helpers.term_to_string out_notif)) out_notifs;
		match bb with
		| Types.BlackBox(name, Types.Internal) -> if name = "forward" || name = "emit" then Controller_Forwarding.queue_packets out_notifs 
			else raise (Failure ("internal black box " ^ name ^ " is not currently supported.")) 
		| _ -> List.iter (fun n -> if debug then Printf.printf "SENDING EXT NOTIF: %s\n%!" (Type_Helpers.term_to_string n);
			Flowlog_Thrift_Out.doBBnotify bb n)	out_notifs;;

	let debug1 = true;;
	
	let respond_to_notification_workhorse (notif : Types.term) (prgm : Types.program) : unit =
		if debug1 then 
		  Printf.printf "\n******************\nincoming notif: [%s] of type: %s\n\n%!" 
		    (Type_Helpers.term_to_string notif) (Type_Helpers.term_type_name (Type_Helpers.type_of_term notif));

		let already_seen = ref [] in
		match prgm with Types.Program(_, _, _, _, clauses) ->
		(match notif with Types.Constant(_, ttype) ->
		(* Flowlog issues notifications before making state changes. It then evaluates +/- relations 
		   on the same pre-state, and finally updates the state. So there is no "after minus but before plus". *)	
		
		(* Trigger the action/plus/minus clauses that match the incoming type. *)		
		List.iter (fun cls -> match cls with 
			| Types.Clause(Types.Signature(Types.Action, module_name, cls_name, [Types.Variable(_, type1); Types.Variable(_, _) as v2]), _) ->
				if (not (List.mem (Type_Helpers.clause_signature cls) !already_seen)) && type1 = ttype then
				(already_seen := Type_Helpers.clause_signature cls :: !already_seen;
				let to_send = Communication.query_signature prgm (Types.Signature(Types.Action, module_name, cls_name, [notif; v2])) in				
				List.iter (fun (tl : Types.term list) -> send_notifications (Type_Helpers.get_blackbox prgm cls_name) tl) to_send);
			| _ -> ();) clauses;
		Controller_Forwarding.flush_packets (); 

		let to_assert = ref [] in
		let to_retract = ref [] in

		List.iter (fun cls -> match cls with 
			| Types.Clause(Types.Signature(Types.Minus, module_name, cls_name, Types.Variable(_, type1) :: tail), _) ->
				if (not (List.mem (Type_Helpers.clause_signature cls) !already_seen)) && type1 = ttype then
				already_seen := Type_Helpers.clause_signature cls :: !already_seen;
				to_retract := (List.map (fun tl -> (Types.Signature(Types.Helper, module_name, cls_name, tl)))
					            (Communication.query_signature prgm (Types.Signature(Types.Minus, module_name, cls_name, notif :: tail))))
  				              @ !to_retract;			
			| _ -> ();) clauses;

		List.iter (fun cls -> match cls with 
			| Types.Clause(Types.Signature(Types.Plus, module_name, cls_name, Types.Variable(_, type1) :: tail), _) ->
				if (not (List.mem (Type_Helpers.clause_signature cls) !already_seen)) && type1 = ttype then
				already_seen := Type_Helpers.clause_signature cls :: !already_seen;
				to_assert := (List.map (fun tl -> (Types.Signature(Types.Helper, module_name, cls_name, tl)))
					           (Communication.query_signature prgm (Types.Signature(Types.Plus, module_name, cls_name, notif :: tail))))		
                             @ !to_assert;
			| _ -> ();) clauses;

		(* State changes happen together after evaluation is complete *)
        List.iter (fun (s : Types.signature) -> Communication.retract_signature s) !to_retract;
		List.iter (fun (s : Types.signature) -> Communication.assert_signature s) !to_assert;

		| _ -> raise (Failure "respond_to_notification can only be called with a constant"));
		
		if debug then Xsb.debug_print_listings ();;

(* XSB is shared state. We also have the remember_for_forwarding and packet_queue business *)
let xsbmutex = Mutex.create();;

let respond_to_notification (notif : Types.term) (prgm : Types.program) (packet_context: (switchId * packetIn * Types.term) option): unit =
	  (* Ox is catching our exceptions and continuing. Bad behavior for debugging 
	     a new codebase! So any time handling a notification throws an exception,
	     catch it ourselves, print the stack trace, and quit. *)
	  try
	    Mutex.lock xsbmutex;
	    Controller_Forwarding.remember_for_forwarding packet_context;
		respond_to_notification_workhorse notif prgm;
		(* for the love of sanity, remember to clear this out even if no packets result! *)
		Controller_Forwarding.clear_remember_for_forwarding();
		Mutex.unlock xsbmutex;
	  with exn -> 
	    Format.printf "Unexpected exception: %s\n----------\n%s\n%!"
        (Printexc.to_string exn)
        (Printexc.get_backtrace ());
        
        exit(1);;


end
