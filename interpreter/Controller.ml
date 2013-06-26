open Xsb;;
open Flowlog;;
open Packet;;
open OxPlatform;;
open OpenFlow0x01_Core;;
open OpenFlow0x01;;

let debug = true;;

module type PROGRAM = sig
	val program : Flowlog.program;;
end

module Make_OxModule (Program : PROGRAM) = struct
	include OxStart.DefaultTutorialHandlers;;
	
	let ref_out_ch = ref None;;
	let ref_in_ch = ref None;;
	let get_ch = (fun () -> match !ref_out_ch with
		| None -> let out_ch, in_ch = Xsb.start_xsb () in 
			let _ = ref_out_ch := Some(out_ch) in
			let _ = ref_in_ch := Some(in_ch) in
			let _ = Flowlog.start_program Program.program out_ch in_ch in
			let _ = if debug then print_endline "started program" in
			(out_ch, in_ch);
		| Some(out_ch) -> match !ref_in_ch with
			|Some(in_ch) -> (out_ch, in_ch);
			| _ -> raise (Failure "ref_out_ch is some but ref_in_ch is none"));;
	
	let _ = get_ch ();;
	
	let switch_connected (sw : switchId) (feats : OpenFlow0x01.SwitchFeatures.t) : unit =
	    Printf.printf "Switch %Ld connected.\n%!" sw;
	    (* the next line does nothing but tests the calling of feats.ports which right now fails *)
	    let port_nums = List.map (fun (x : PortDescription.t)-> x.PortDescription.port_no) feats.SwitchFeatures.ports in
	    let out_ch, in_ch = get_ch () in
	    Flowlog.update_switch_ports sw port_nums out_ch in_ch;;
	    

	let packet_in (sw : switchId) (xid : xid) (pk : packetIn) : unit =
		if debug then Printf.printf "%s\n%!" (packetIn_to_string pk);
		let out_ch, in_ch = get_ch () in
		Flowlog.respond_to_packet Program.program sw xid pk out_ch in_ch;;

	let cleanup () : unit = 
		let _ = print_endline "running cleanup" in
		let out_ch, in_ch = get_ch () in
		Xsb.halt_xsb out_ch;;

end

(* Eventually change to MyOxStart.Make so cleanup is called on exceptions. *)
module Make_Controller (Program : PROGRAM) = OxStart.Make (Make_OxModule (Program));;

let process_atom_name (fn : string -> string) (a : Flowlog.atom) : Flowlog.atom = 
	match a with
	| Flowlog.Apply(name, tl) -> Flowlog.Apply(fn name, tl);
	| _ -> a;;

let process_literal_name (fn : string -> string) (lit : Flowlog.literal) : Flowlog.literal = 
	match lit with
	| Flowlog.Pos(a) -> Flowlog.Pos(process_atom_name fn a);
	| Flowlog.Neg(a) -> Flowlog.Neg(process_atom_name fn a);;

let process_clause_name (fn : string -> string) (cls : Flowlog.clause) : Flowlog.clause = 
	match cls with Flowlog.Clause(name, args, body) ->
	let new_body = List.map (process_literal_name fn) body in
	Flowlog.Clause(fn name, args, new_body);;

let process_relation_name (fn: string -> string) (rel : Flowlog.relation) : Flowlog.relation = 
	match rel with Flowlog.Relation(name, args, clauses) -> 
	let new_clauses = List.map (process_clause_name fn) clauses in
	Flowlog.Relation(fn name, args, new_clauses);;

let append_string (to_append : string) (str : string) : string =
	if (str <> "forward") then str ^ to_append else str;;

module Union (Pg1 : PROGRAM) (Pg2 : PROGRAM) = struct
let program = match Pg1.program with 
	| Flowlog.Program(name_1, rel_list_1, forward_rel_1) -> 
	match Pg2.program with 
	| Flowlog.Program(name_2, rel_list_2, forward_rel_2) ->
	let process_1 = process_relation_name (append_string ("/" ^ name_1)) in
	let process_2 = process_relation_name (append_string ("/" ^ name_2)) in
	Flowlog.Program(name_1 ^ "+" ^ name_2, 
		(List.map process_1 rel_list_1) @ (List.map process_2 rel_list_2), 
		Flowlog.Relation("forward", Flowlog.packet_vars @ Flowlog.packet_vars_2, 
		(match process_1 forward_rel_1 with Flowlog.Relation(_, _, clauses) -> clauses)
		@ (match process_2 forward_rel_2 with Flowlog.Relation(_, _, clauses) -> clauses)));;
end