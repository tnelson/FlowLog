open Types;;
open Controller_Forwarding;;
open Xsb_Communication;;
open Type_Helpers;;
open Flowlog_Thrift_Out;;

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

	let respond_to_notification (notif : Types.term) (prgm : Types.program) : unit =
		if debug1 then 
		  Printf.printf "incoming notif: [%s] of type: %s\n%!" 
		    (Type_Helpers.term_to_string notif) (Type_Helpers.term_type_name (Type_Helpers.type_of_term notif));
		let already_seen = ref [] in
		match prgm with Types.Program(_, _, _, _, clauses) ->
		(match notif with Types.Constant(_, ttype) ->
		List.iter (fun cls -> match cls with 
			| Types.Clause(Types.Signature(Types.Action, module_name, cls_name, [Types.Variable(_, type1); Types.Variable(_, _) as v2]), _) ->
				if (not (List.mem (Type_Helpers.clause_signature cls) !already_seen)) && type1 = ttype then
				(already_seen := Type_Helpers.clause_signature cls :: !already_seen;
				let to_send = Communication.query_signature prgm (Types.Signature(Types.Action, module_name, cls_name, [notif; v2])) in
				(*Printf.printf "  *** tosend found %d\n%!" (List.length to_send);*)
				List.iter (fun (tl : Types.term list) -> send_notifications (Type_Helpers.get_blackbox prgm cls_name) tl) to_send);
			| _ -> ();) clauses;
		Controller_Forwarding.flush_packets (); 
		List.iter (fun cls -> match cls with 
			| Types.Clause(Types.Signature(Types.Minus, module_name, cls_name, Types.Variable(_, type1) :: tail), _) ->
				if (not (List.mem (Type_Helpers.clause_signature cls) !already_seen)) && type1 = ttype then
				already_seen := Type_Helpers.clause_signature cls :: !already_seen;
				let to_retract = Communication.query_signature prgm (Types.Signature(Types.Minus, module_name, cls_name, notif :: tail)) in
				List.iter (fun (tl : Types.term list) -> Communication.retract_signature (Types.Signature(Types.Helper, module_name, cls_name, tl))) to_retract;
			| _ -> ();) clauses;
		List.iter (fun cls -> match cls with 
			| Types.Clause(Types.Signature(Types.Plus, module_name, cls_name, Types.Variable(_, type1) :: tail), _) ->
				if (not (List.mem (Type_Helpers.clause_signature cls) !already_seen)) && type1 = ttype then
				already_seen := Type_Helpers.clause_signature cls :: !already_seen;
				let to_assert = Communication.query_signature prgm (Types.Signature(Types.Plus, module_name, cls_name, notif :: tail)) in
				List.iter (fun (tl : Types.term list) -> Communication.assert_signature (Types.Signature(Types.Helper, module_name, cls_name, tl))) to_assert;
			| _ -> ();) clauses;
		| _ -> raise (Failure "respond_to_notification can only be called with a constant"));

		if debug then Xsb.debug_print_listings ();;

end
