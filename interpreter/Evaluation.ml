open Flowlog_Types;;
open Controller_Forwarding;;
open Xsb_Communication;;
open Flowlog_Thrift_Out;;
open Type_Helpers;;

let debug = true;;

(* Provides functions for running a Flowlog program.*)
module Evaluation = struct

	let send_notifications (bb : Types.blackbox) (out_notifs : Types.notif_val list) : unit =
		if debug then List.iter (fun out_notif -> print_endline ("outgoing notif: " ^ Type_Helpers.notif_val_to_string out_notif)) out_notifs;
		match bb with
		| Types.BlackBox(name, Types.Internal) -> if name = "forward" then Controller_Forwarding.queue_packets out_notifs 
                                             else raise (Failure ("internal black box " ^ name ^ " is not currently supported.")) 
		| _ -> List.iter (fun n -> if debug then Printf.printf "SENDING EXT NOTIF: %s\n%!" (Type_Helpers.notif_val_to_string n);
	                               Flowlog_Thrift_Out.doBBnotify bb n)
	                     out_notifs;;

	let debug1 = false;;

(* 
need:
send_notifications bb out_notifs
flush_notifications
Communication.query_clause
Communication.retract_clause
Communication.assert_clause
Type_Helpers.get_type
Type_Helpers.get_blackbox
*)

	let respond_to_notification (notif : Types.notif_val) (prgm : Types.program) : unit =
		if debug1 then print_endline ("incoming notif: " ^ Type_Helpers.notif_val_to_string notif);
		match prgm with Types.Program(name, _, _, _, clauses) ->
		match notif with Types.Notif_val(ntype, terms) ->
		List.iter (fun cls -> match cls with
			| Types.Clause(Types.Action, name, args, body) -> match args with
				| [Types.Notif_var(_, type_name); Types.Notif_var(_, _)] -> 
					let target_type = Type_Helpers.get_type prgm type_name in
					if target_type = ntype then
					let to_send = Communication.query_clause (Types.Clause(Types.Action, name, terms @ tail, body)) in
					send_notifications (Type_Helpers.get_blackbox prgm name) (List.map (fun (tl : Types.term list) -> Types.Notif_val(target_type, tl)) to_send);
				| _ -> raise (Failure "an action clause must have exactly two typed variables.");
			| _ -> ();) clauses;
		flush_notifications ();
		List.iter (fun cls -> match cls with 
			| Types.Clause(Types.Minus, name, args, body) -> match args with
				| Types.Notif_var(_, type_name) :: tail-> ; if Type_Helpers.get_type prgm type_name = ntype then
					let to_retract = Communication.query_clause (Types.Clause(Types.Minus, name, terms @ tail, body)) in
					let state_clause = Types.Clause(Types.State, name, tail, []) in
					List.iter (fun (tl : Types.term list) -> Communication.retract_clause state_clause tl) to_retract;
				| _ -> raise (Failure "the first argument of a minus clause is always a typed variable.");
			| _ -> ();) clauses;
		List.iter (fun cls -> match cls with 
			| Types.Clause(Types.Plus, name, args, body) -> match args with
				| Types.Notif_var(_, type_name) :: tail-> ; if Type_Helpers.get_type prgm type_name = ntype then
					let to_assert = Communication.query_clause (Types.Clause(Types.Plus, name, terms @ tail, body)) in
					let state_clause = Types.Clause(Types.State, name, tail, []) in
					List.iter (fun (tl : Types.term list) -> Communication.assert_clause state_clause tl) to_retract;
				| _ -> raise (Failure "the first argument of a plus clause is always a typed variable.");
			| _ -> ();) clauses;
end
