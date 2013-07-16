open Flowlog_Types;;
open Controller_Forwarding;;
open Xsb_Communication;;
open Flowlog_Thrift_Out;;
open Type_Helpers;;

let debug = true;;

(* Provides functions for running a Flowlog program.
ASSUMPTIONS: We assume that programs passed into functions in this module have
1) all implied relations defined (i.e. if there's +R or -R then there's R)
2) all relations except NotifRelations have names ending in /(name of program).
3) the name of the program is lower case.
*)
module Evaluation = struct

	let send_notifications (bb : Types.blackbox) (out_notifs : Types.notif_val list) : unit =
		if debug then List.iter (fun out_notif -> print_endline ("outgoing notif: " ^ Type_Helpers.notif_val_to_string out_notif)) out_notifs;
		match bb with
		| Types.Internal_BB(name) -> if name = "forward" then Controller_Forwarding.forward_packets out_notifs 
                                             else raise (Failure ("internal black box " ^ name ^ " is not currently supported.")) 
		| _ -> List.iter (fun n -> Flowlog_Thrift_Out.doBBnotify bb n) out_notifs;;

	let fire_relation (prgm : Types.program) (rel : Types.relation) (notif : Types.notif_val)  : unit =
		if debug then print_endline ("firing relation: " ^ (Type_Helpers.relation_name rel));
		match notif with Types.Notif_val(ntype, terms) ->
		let arg_terms = List.map (fun t -> Types.Arg_term(t)) terms in
		match rel with
		| Types.NotifRelation(bb, args, _) ->
			(match args with
			| [] -> raise (Failure "notif relations always have two arguments.");
			| _ :: tail -> 
			let out_notifs = List.map (fun (tl : Types.term list) -> Types.Notif_val(ntype, tl))
				(Communication.query_relation rel (arg_terms @ tail)) in
			send_notifications bb out_notifs;);
		| Types.MinusRelation(_, args, _) ->
			(match args with
			| [] -> raise (Failure "minus relations always have at least one argument.");
			| _ :: tail ->
			let to_retract = Communication.query_relation rel (arg_terms @ tail) in
			let helper_rel = Type_Helpers.helper_relation prgm rel in
			List.iter (fun (tl : Types.term list) -> Communication.retract_relation helper_rel tl) to_retract;);
		| Types.PlusRelation(_, args, _) ->
			(match args with
			| [] -> raise (Failure "plus relations always have at least one argument.");
			| _ :: tail ->
			let to_assert = Communication.query_relation rel (arg_terms @ tail) in
			let helper_rel = Type_Helpers.helper_relation prgm rel in
			List.iter (fun (tl : Types.term list) -> Communication.assert_relation helper_rel tl) to_assert;);
		| Types.HelperRelation(_, _, _) -> raise (Failure "helper relations cannot be fired.");;

	let debug1 = false;;

	let respond_to_notification (notif : Types.notif_val) (prgm : Types.program) : unit =
		if debug1 then print_endline ("incoming notif: " ^ Type_Helpers.notif_val_to_string notif);
		match prgm with Types.Program(name, relations) ->
		match notif with Types.Notif_val(ntype, _) ->
		if debug1 then print_endline "starting NotifRelations.";
		let _ = List.iter (fun rel -> match rel with
			| Types.NotifRelation(bb, args, _) -> (match args with
				| [] -> raise (Failure "NotifRelations always have two arguments");
				| Types.Arg_notif(Types.Notif_var(nt, _)) :: _ -> if debug1 then print_endline ((Type_Helpers.blackbox_name bb) ^ ": " ^ Type_Helpers.notif_type_to_string nt);
					if nt = ntype then fire_relation prgm rel notif;
				| _ -> raise (Failure "NotifRelations always have an Arg_notif as their first argument"););
			| _ -> ();) relations in
		if debug1 then print_endline "starting MinusRelations.";
		let _ = List.iter (fun rel -> match rel with
			| Types.MinusRelation(name, args, _) -> (match args with
				| [] -> raise (Failure "MinusRelations always have at least two arguments");
				| Types.Arg_notif(Types.Notif_var(nt, _)) :: _ -> if debug1 then print_endline ("-" ^ name ^ ": " ^ Type_Helpers.notif_type_to_string nt);
					if nt = ntype then fire_relation prgm rel notif;
				| _ -> raise (Failure "MinusRelations always have an Arg_notif as their first argument"););
			| _ -> ();) relations in
		if debug1 then print_endline "starting PlusRelations.";
		let _ = List.iter (fun rel -> match rel with
			| Types.PlusRelation(name, args, _) -> (match args with
				| [] -> raise (Failure "PlusRelations always have at least two arguments");
				| Types.Arg_notif(Types.Notif_var(nt, _)) :: _ -> if debug1 then print_endline ("+" ^ name ^ ": " ^ Type_Helpers.notif_type_to_string nt);
					if nt = ntype then fire_relation prgm rel notif;
				| _ -> raise (Failure "PlusRelations always have an Arg_notif as their first argument"););
			| _ -> ();) relations in ();;
	

end
