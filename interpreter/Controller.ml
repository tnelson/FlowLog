open Xsb;;
open Flowlog;;
open Packet;;
open OxPlatform;;
open OpenFlow0x01_Core;;

let debug = true;;

module type PROGRAM = sig
	val program : Flowlog.program;;
end

module Make_OxModule (Program : PROGRAM) = struct
	include OxStart.DefaultTutorialHandlers;;

	let switch_connected (sw : switchId) : unit =
	    Printf.printf "Switch %Ld connected.\n%!" sw
	
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
	
	let packet_in (sw : switchId) (xid : xid) (pk : packetIn) =
		if debug then Printf.printf "%s\n%!" (packetIn_to_string pk);
		let out_ch, in_ch = get_ch () in
		Flowlog.respond_to_packet Program.program sw xid pk out_ch in_ch;;

	let cleanup () = let out_ch, in_ch = get_ch () in Xsb.halt_xsb out_ch;;

end

(* Eventually change to MyOxStart.Make so cleanup is called on exceptions. *)
module Make_Controller (Program : PROGRAM) = OxStart.Make (Make_OxModule (Program));;

let append_relation_name (new_name : string) (rel : Flowlog.relation) : Flowlog.relation = 
	match rel with Flowlog.Relation(old_name, args, clauses, plus, minus) -> Flowlog.Relation(new_name ^ "/" ^ old_name, args, clauses, plus, minus);;

module Union (Pg1 : PROGRAM) (Pg2 : PROGRAM) = struct
let program = match Pg1.program with Flowlog.Program(name_1, rel_list_1, forward_rel_1) -> match Pg2.program with Flowlog.Program(name_2, rel_list_2, forward_rel_2) ->
	Flowlog.Program(name_1 ^ "+" ^ name_2, List.map (append_relation_name name_1) rel_list_1 @ List.map (append_relation_name name_2) rel_list_2, 
		Flowlog.Relation("forward", Flowlog.packet_vars @ Flowlog.packet_vars_2, 
		(match forward_rel_1 with Flowlog.Relation(_, _, clauses, _, _) -> clauses) @ (match forward_rel_2 with Flowlog.Relation(_, _, clauses, _, _) -> clauses), None, None));;
end