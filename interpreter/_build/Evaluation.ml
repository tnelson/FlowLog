open Types;;
open Controller_Forwarding;;
open Xsb_Communication;;
(*open Flowlog_Thrift_Out;;*)
open Type_Helpers;;

let debug = true;;

(* Provides functions for running a Flowlog program.*)
module Evaluation = struct

	let send_notifications (bb : Types.blackbox) (out_notifs : Types.term list) : unit =
		if debug then List.iter (fun out_notif -> print_endline ("outgoing notif: " ^ Type_Helpers.term_to_string out_notif)) out_notifs;
		match bb with
		| Types.BlackBox(name, Types.Internal) -> if name = "forward" then Controller_Forwarding.queue_packets out_notifs 
			else raise (Failure ("internal black box " ^ name ^ " is not currently supported.")) 
		| _ -> List.iter (fun n -> if debug then Printf.printf "SENDING EXT NOTIF: %s\n%!" (Type_Helpers.term_to_string n);
			(*Flowlog_Thrift_Out.doBBnotify bb n*))	out_notifs;;

	let debug1 = false;;

(* 
need:
Flowlog_Thrift_Out.doBBnotify bb n
Type_Helpers.get_blackbox
*)


	let respond_to_notification (notif : Types.term) (prgm : Types.program) : unit =
		if debug1 then print_endline ("incoming notif: " ^ Type_Helpers.term_to_string notif);
		let already_seen = ref [] in
		match prgm with Types.Program(_, _, _, _, clauses) ->
		match notif with Types.Constant(_, ttype) ->
		List.iter (fun cls -> match cls with 
			| Types.Clause(Types.Signature(Types.Action, cls_name, [Types.Variable(_, type1); Types.Variable(_, _) as v2]), _) ->
				if (not (List.mem (Type_Helpers.clause_signature cls) !already_seen)) && type1 = ttype then
				(already_seen := Type_Helpers.clause_signature cls :: !already_seen;
				let to_send = Communication.query_signature (Types.Signature(Types.Action, cls_name, [notif; v2])) in
				send_notifications (Type_Helpers.get_blackbox prgm name) to_send);
			| _ -> ();) clauses;
		List.iter (fun cls -> match cls with 
			| Types.Clause(Types.Signature(Types.Minus, cls_name, Types.Variable(_, type1) :: tail), _) ->
				if (not List.mem (Type_Helpers.clause_signature cls) !already_seen) && type1 = ttype then
				already_seen := Type_Helpers.clause_signature cls :: !already_seen;
				let to_retract = Communication.query_signature (Types.Signature(Types.Minus, cls_name, notif @ tail)) in
				List.iter (fun (tl : Types.term list) -> Communication.retract_signature (Types.Signature(Types.Helper, cls_name, tl))) to_retract;
			| _ -> ();) clauses;
		List.iter (fun cls -> match cls with 
			| Types.Clause(Types.Signature(Types.Plus, cls_name, Types.Variable(_, type1) :: tail), _) ->
				if (not List.mem (Type_Helpers.clause_signature cls) !already_seen) && type1 = ttype then
				already_seen := Type_Helpers.clause_signature cls :: !already_seen;
				let to_assert = Communication.query_signature (Types.Signature(Types.Plus, cls_name, notif @ tail)) in
				List.iter (fun (tl : Types.term list) -> Communication.assert_signature (Types.Signature(Types.Helper, cls_name, tl))) to_assert;
			| _ -> ();) clauses;;

end
