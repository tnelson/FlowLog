(*#load "unix.cma";;
#load "xsb.cmo";;*)
(*#load "oUnit.cma";;*)

open Unix;;
open Xsb;;
open Packet;;
open OxPlatform;;
open OpenFlow0x01_Core;;
(*open OUnit;;*)

module Program = struct

(* constants and variables (recall variables are uppercase) *)
type term = Constant of string | Variable of string;;

(* Things like A = B or R(A, B, C) *)
type atom = Equals of term * term | Apply of relation * term list | Bool of bool
and
(* the and is for mutually recursive types *)
(* Atoms and negations of atoms *)
	literal = Pos of atom | Neg of atom
and
(* First string is name, second is list of arguments, third is body *)
	 clause = Clause of relation * term list * literal list
and
(* name, arguments, clauses, plus, minus *)
	 relation = Relation of string * term list * clause list * relation option * relation option;;

(* list of non-emit relations, emit relation*)
type program = Program of relation list * relation;;

let term_to_string (t : term) : string = 
	match t with
	| Constant(c) -> c; 
	| Variable(v) -> v;; (* v must be upper case *)

let print_term_list (l : (term list) list) : unit =
	let mapped = List.map (fun sublist -> List.map term_to_string sublist) l in
	print_endline (Xsb.lol_to_string mapped);;

let relation_name (rel : relation) : string =
	match rel with 
	| Relation(name, _, _, _, _) -> name;;

let list_to_string (l : 'a list) (conversion : 'a -> string) : string = 
	let ans = List.fold_right (fun x acc -> (conversion x) ^ "," ^ acc) l "" in
	(*let _ = print_endline ("list_to_string: " ^ ans) in*)
	if ans = "" then ans else String.sub ans 0 (String.length ans - 1);;

let atom_to_string (a : atom) : string =
	match a with
	| Equals(t1, t2) -> term_to_string(t1) ^ " = " ^ term_to_string(t2);
	| Apply(rel, args) -> (relation_name rel) ^ "(" ^ (list_to_string args term_to_string) ^ ")";
	| Bool(b) -> string_of_bool b;;

let literal_to_string (l : literal) : string = 
	match l with
	| Pos(a) -> atom_to_string a;
	| Neg(a) -> "not(" ^ (atom_to_string a) ^ ")";;

let clause_to_string (cl : clause) : string =
	match cl with
	| Clause(rel, args, []) -> (relation_name rel) ^ "(" ^ (list_to_string args term_to_string) ^ ")";
	| Clause(rel, args, body) -> (relation_name rel) ^ "(" ^ (list_to_string args term_to_string) ^ ") :- " ^
		(list_to_string body literal_to_string);;

let get_atom (lit : literal) : atom =
	match lit with
	| Pos(a) -> a;
	| Neg(a) -> a;;

let add_unique (x : 'a) (l : 'a list) : 'a list = if List.mem x l then l else x :: l;;

let add_unique_var (t : term) (acc : term list) : term list = 
	match t with
	| Variable(_) -> add_unique t acc;
	| Constant(_) -> acc;;

let get_vars (cl : clause) : term list =
	match cl with
	| Clause(_, args, body) -> List.fold_right 
		(fun (lit : literal) (acc : term list) -> 
			match get_atom(lit) with
			| Equals(t1, t2) -> add_unique_var t1 (add_unique_var t2 acc);
			| Apply(_, tl) -> List.fold_right add_unique_var tl acc;
			| Bool(b) -> acc;)
		body
		(List.fold_right add_unique_var args []);;

let send_clause (cl : clause) (assertion : string) (out_ch : out_channel) (in_ch : in_channel) : (term list) list =
	let _ = print_endline assertion in
	let num_vars = List.length (get_vars cl) in
	let answer = (if num_vars > 0 then Xsb.send_query assertion (List.length (get_vars cl)) out_ch in_ch
	else let _ = Xsb.send_assert assertion out_ch in_ch in []) in
	List.map (fun (l : string list) -> List.map (fun (s : string) -> Constant(s)) l) answer;;

let query_clause (cl : clause) (out_ch : out_channel) (in_ch : in_channel): (term list) list =
	send_clause cl (match cl with
		| Clause(rel, args, _) -> (relation_name rel) ^ "(" ^ (list_to_string args term_to_string) ^ ").") out_ch in_ch;;

let retract_clause (cl : clause) (out_ch : out_channel) (in_ch : in_channel): (term list) list =
    send_clause cl ("retract((" ^ (clause_to_string cl) ^ ")).") out_ch in_ch;;

let assert_clause (cl : clause) (out_ch : out_channel) (in_ch : in_channel): (term list) list =
	send_clause cl ("assert((" ^ (clause_to_string cl) ^ ")).") out_ch in_ch;;

let tentative_assert_clause (cl : clause) (out_ch : out_channel) (in_ch : in_channel): (term list) list =
	let _ = retract_clause cl out_ch in_ch in
	assert_clause cl out_ch in_ch;;

let assert_relation (rel : relation) (out_ch : out_channel) (in_ch : in_channel) : (term list) list =
	match rel with
	| Relation(_, args, [], _, _) -> assert_clause (Clause(rel, args, [Pos(Bool(false))])) out_ch in_ch;
	| Relation(_, _, clauses, _, _) -> List.fold_right (fun cls acc -> (assert_clause cls out_ch in_ch) @ acc) clauses [];;

let query_relation (rel : relation) (args : term list) (out_ch : out_channel) (in_ch : in_channel) : (term list) list =
(*	let _ = print_endline ("query relation: " ^ (relation_name rel) ^ (list_to_string args term_to_string)) in *)
	let ans = query_clause (Clause(rel, args, [])) out_ch in_ch in
	let _ = print_endline (list_to_string ans (fun x -> list_to_string x term_to_string)) in
	ans;;

let start_program (prgm : program) (out_ch : out_channel) (in_ch : in_channel) : (term list) list = 
	match prgm with
	| Program(relations, emit) -> let out = List.fold_right (fun rel acc -> (assert_relation rel out_ch in_ch) @ acc) relations [] in
		out @ (assert_relation emit out_ch in_ch);;

let rec drop (l : 'a list) (n : int) : 'a list = 
	if n <= 0 then l else
	match l with
	| [] -> [];
	| h :: t -> drop t (n - 1);;

(* this isn't quite right yet *)
let respond_to_packet_desugared (prgm : program) (pkt : term list) (out_ch : out_channel) (in_ch : in_channel) : (term list) list =
	match prgm with
	| Program(relations, emit) ->
		let _ = List.iter (fun rel ->
			match rel with
			| Relation(_, _, _, option_plus, option_minus) ->
				let to_assert = (match option_plus with
				| None -> [];
				| Some(plus) -> match plus with | Relation(_, args, _, _, _) -> 
					query_relation plus (pkt @ (drop args (List.length pkt))) out_ch in_ch;) in
				let to_retract = (match option_minus with
				| None -> [];
				| Some(minus) -> match minus with | Relation(_, args, _, _, _) -> 
					query_relation minus (pkt @ (drop args (List.length pkt))) out_ch in_ch;) in
				let _ = List.iter (fun args -> let _ = tentative_assert_clause (Clause(rel, args, [])) out_ch in_ch in ()) to_assert in
				List.iter (fun args -> let _ = retract_clause (Clause(rel, args, [])) out_ch in_ch in ()) to_retract;) relations in
		match emit with
		| Relation(_, args, _, _, _) -> query_relation emit (pkt @ (drop args (List.length pkt))) out_ch in_ch;; 

let rec replace (r : string) (w : string) (st : string) : string =
	if (String.length st < String.length r || String.length st = 0) then st else
		if String.sub st 0 (String.length r) = r then w ^ (replace r w (String.sub st (String.length r) (String.length st - String.length r))) else
			(String.sub st 0 1) ^ (replace r w (String.sub st 1 (String.length st - 1)));;

let pkt_to_term_list (sw : switchId) (pk : packetIn) : term list = 
	let pkt_payload = parse_payload pk.input_payload in
	let ans = List.map (function x -> Constant(x)) [Int64.to_string sw;
	string_of_int pk.port;
	Int64.to_string pkt_payload.Packet.dlSrc;
	Int64.to_string pkt_payload.Packet.dlDst;
	string_of_int (dlTyp pkt_payload);
	Int32.to_string (nwSrc pkt_payload);
	Int32.to_string (nwDst pkt_payload);
	string_of_int (nwProto pkt_payload)] in
	let _ = print_endline ("pkt to term list: " ^ (list_to_string ans term_to_string)) in
	ans;;
(*
let term_list_to_pkt (tl : term list) (pk : packetIn) : switchId * packetOut =
(*	let _ = print_endline ("term list to pkt: " ^ (list_to_string tl term_to_string)) in
	let locSw = Int64.of_int (int_of_string (term_to_string (List.nth tl 0))) in
	let locPt = int_of_string (term_to_string (List.nth tl 1)) in
	let _ = print_endline ("before dlsrc: " ^ (term_to_string (List.nth tl 2))) in
	let dlSrc = Int64.of_string (term_to_string (List.nth tl 2)) in
	let _ = print_endline "after dlsrc" in
	let dlDst = Int64.of_string (term_to_string (List.nth tl 3)) in
	(*let dlTyp = int_of_string (term_to_string (List.nth tl 4)) in*)
	let nwSrc = Int32.of_int (int_of_string (term_to_string (List.nth tl 5))) in
	let nwDst = Int32.of_int (int_of_string (term_to_string (List.nth tl 6))) in
	(*let nwProto = int_of_string (term_to_string (List.nth tl 7)) in*)
	let in_packet = parse_payload pk.input_payload in
	let _ = print_endline ("sending out port " ^ (string_of_int locPt)) in
	let actions_list = ref [Output(PhysicalPort(locPt))] in
		(*if dlSrc <> in_packet.Packet.dlSrc then actions_list := SetDlSrc(dlSrc) :: !actions_list;
		if dlDst <> in_packet.Packet.dlDst then actions_list := SetDlDst(dlDst) :: !actions_list;
		if nwSrc <> Packet.nwSrc in_packet then actions_list := SetNwSrc(nwSrc) :: !actions_list;
		if nwDst <> Packet.nwDst in_packet then actions_list := SetNwDst(nwDst) :: !actions_list;*)
(* 	let out_payload = {dlSrc = input_payload.dlSrc;
		dlDst = input_payload.dlDst;
		dlVlan = input_payload.dlVlan;
		dlVlanPcp = input_payload.dlVlanPcp;
		nw = input_payload.nw} in
	let _ = setDlSrc out_payload dlSrc in
	let _ = setDlDst out_payload dlDst in
	let _ = setNwSrc out_payload nwSrc in
	let _ = setNwDst out_payload nwDst in*)*)
	let locSw = Int64.of_int (int_of_string (term_to_string (List.nth tl 0))) in
	let locPt = int_of_string (term_to_string (List.nth tl 1)) in
	(*let _ = print_endline ("sent out port ")*)
	let out_packet = {output_payload = pk.input_payload; port_id = None; apply_actions = [Output(PhysicalPort(locPt))]} in (* change to Output(AllPorts)) *)
	(locSw, out_packet);;*)
	
let emit_packets (tll : (term list) list) (sw : switchId) (pk : packetIn) : unit = 
	let actions_list = ref [] in
	let _ = List.iter (fun tl -> 
		(*let locSw = Int64.of_string (term_to_string (List.nth tl 0)) in*)
		let locPt = int_of_string (term_to_string (List.nth tl 1)) in
		let dlSrc = Int64.of_string (term_to_string (List.nth tl 2)) in
		let dlDst = Int64.of_string (term_to_string (List.nth tl 3)) in
		(*let dlTyp = int_of_string (term_to_string (List.nth tl 4)) in*)
		let nwSrc = Int32.of_int (int_of_string (term_to_string (List.nth tl 5))) in
		let nwDst = Int32.of_int (int_of_string (term_to_string (List.nth tl 6))) in
		(*let nwProto = int_of_string (term_to_string (List.nth tl 7)) in*)
			actions_list := SetDlSrc(dlSrc) :: !actions_list;
			actions_list := SetDlDst(dlDst) :: !actions_list;
			actions_list := SetNwSrc(nwSrc) :: !actions_list;
			actions_list := SetNwDst(nwDst) :: !actions_list;
			actions_list := Output(PhysicalPort(locPt)) :: !actions_list;
	) tll in
	send_packet_out sw 0l {output_payload = pk.input_payload; port_id = None; apply_actions = !actions_list};;

let respond_to_packet (prgm : program) (sw : switchId) (xid : xid) (pk : packetIn) (out_ch : out_channel) (in_ch : in_channel) : unit = 
	let in_tl = pkt_to_term_list sw pk in
	let out_tll = respond_to_packet_desugared prgm in_tl out_ch in_ch in 
	emit_packets out_tll sw pk;;

(*(*let _ = print_endline "before respond to packet desugared" in*)
	let out_tll = respond_to_packet_desugared prgm in_tl out_ch in_ch in
	(*let _ = print_endline "after respond to packet desugared" in*)
	List.fold_right (fun tl acc -> let out_sw, out_pk = (term_list_to_pkt tl pk) in
		((out_sw, xid, out_pk) :: acc)) out_tll [];;*)

end

open Program
module Mac = struct

let packet_vars = List.map (fun (str : string) -> Variable(str)) ["LocSw"; "LocPt"; "DlSrc"; "DlDst"; "DlTyp"; "NwSrc"; "NwDst"; "NwProto"];;
let packet_vars_2 = List.map (fun (str : string) -> Variable(str)) ["LocSw2"; "LocPt2"; "DlSrc2"; "DlDst2"; "DlTyp2"; "NwSrc2"; "NwDst2"; "NwProto2"];;
let learned_vars = [Variable("Sw"); Variable("Pt"); Variable("Mac")];;

let rec plus_learned = Clause(plus_learned_relation, packet_vars @ learned_vars,
	[Pos(Equals(Variable("LocSw"), Variable("Sw")));
	Pos(Equals(Variable("DlSrc"), Variable("Mac")));
	Pos(Equals(Variable("LocPt"), Variable("Pt")))])
	and
	plus_learned_relation = Relation("plus_learned", packet_vars @ learned_vars, [plus_learned], None, None);;

let rec minus_learned = Clause(minus_learned_relation, packet_vars @ learned_vars,
	[Pos(Equals(Variable("LocSw"), Variable("Sw")));
	Pos(Equals(Variable("DlSrc"), Variable("Mac")));
	Neg(Equals(Variable("LocPt"), Variable("Pt")))])
	and
	minus_learned_relation = Relation("minus_learned", packet_vars @ learned_vars, [minus_learned], None, None);;

let learned_relation = Relation("learned", learned_vars, [], Some(plus_learned_relation), Some(minus_learned_relation));;

let rec emit_1 = Clause(emit_relation, packet_vars @ packet_vars_2,
	[Pos(Apply(learned_relation, [Variable("LocSw"); Variable("LocPt2"); Variable("DlDst")]));
	Pos(Equals(Variable("LocSw"), Variable("LocSw2")));
	Pos(Equals(Variable("DlSrc"), Variable("DlSrc2")));
	Pos(Equals(Variable("DlDst"), Variable("DlDst2")));	
	Pos(Equals(Variable("DlTyp"), Variable("DlTyp2")));
	Pos(Equals(Variable("NwSrc"), Variable("NwSrc2")));
	Pos(Equals(Variable("NwDst"), Variable("NwDst2")));
	Pos(Equals(Variable("NwProto"), Variable("NwProto2")))
	])
	and
	emit_2 = Clause(emit_relation, packet_vars @ packet_vars_2,
	[Pos(Equals(Variable("LocSw"), Variable("LocSw2")));
	Pos(Equals(Variable("DlSrc"), Variable("DlSrc2")));
	Pos(Equals(Variable("DlDst"), Variable("DlDst2")));
	Pos(Equals(Variable("DlTyp"), Variable("DlTyp2")));
	Pos(Equals(Variable("NwSrc"), Variable("NwSrc2")));
	Pos(Equals(Variable("NwDst"), Variable("NwDst2")));
	Pos(Equals(Variable("NwProto"), Variable("NwProto2")));
	Pos(Apply(switch_has_ports_relation, [Variable("LocSw2"); Variable("LocPt2")]));
	Neg(Equals(Variable("LocPt"), Variable("LocPt2")));
	Neg(Apply(learned_relation, [Variable("LocSw"); Variable("Any"); Variable("DlDst")]))
	])
	and
	emit_relation = Relation("emit", packet_vars @ packet_vars_2, [emit_1; emit_2], None, None)
	and
	switch_1 = Clause(switch_has_ports_relation, [Constant("1"); Constant("1")], [])
	and
	switch_2 = Clause(switch_has_ports_relation, [Constant("1"); Constant("2")], [])
	and
	switch_3 = Clause(switch_has_ports_relation, [Constant("1"); Constant("3")], [])
	and
	switch_has_ports_relation = Relation("switchHasPorts", [Variable("Sw"); Variable("Pt")], [switch_1; switch_2; switch_3], None, None);;

let mac_learning_program = Program([plus_learned_relation; minus_learned_relation; learned_relation; switch_has_ports_relation], emit_relation);;
end

(* try out the functions *)

(*open Mac;;*)

(*let out_ch, in_ch = Xsb.start_xsb ();;
Program.start_program mac_learning_program out_ch in_ch;;

print_term_list(Program.respond_to_packet_desugared mac_learning_program
	[Constant("1");
	Constant("2");
	Constant("3");
	Constant("4");
	Constant("5");
	Constant("6");
	Constant("7");
	Constant("8")] out_ch in_ch);;

print_term_list(Program.respond_to_packet_desugared mac_learning_program
	[Constant("1");
	Constant("3");
	Constant("4");
	Constant("3");
	Constant("5");
	Constant("6");
	Constant("7");
	Constant("8")] out_ch in_ch);;

print_term_list(Program.query_relation learned_relation [Variable("X"); Variable("Y"); Variable("Z")] out_ch in_ch);;

Xsb.halt_xsb out_ch;;*)


open OpenFlow0x01_Core;;
module OxController = struct
include OxStart.DefaultTutorialHandlers


(*let out_ch, in_ch = Xsb.start_xsb ();;
start_program mac_learning_program out_ch in_ch;;*)

let switch_connected (sw : switchId) : unit =
    Printf.printf "Switch %Ld connected.\n%!" sw

let ref_out_ch = ref None;;
let ref_in_ch = ref None;;
let get_ch = (fun () -> match !ref_out_ch with
	| None -> let out_ch, in_ch = Xsb.start_xsb () in 
		let _ = ref_out_ch := Some(out_ch) in
		let _ = ref_in_ch := Some(in_ch) in
		let _ = Program.start_program Mac.mac_learning_program out_ch in_ch in
		let _ = print_endline "started program" in
		(out_ch, in_ch);
	| Some(out_ch) -> match !ref_in_ch with
		|Some(in_ch) -> (out_ch, in_ch);
		| _ -> raise (Failure "ref_out_ch is some but ref_in_ch is none"));;

let _ = get_ch ();;

let packet_in (sw : switchId) (xid : xid) (pk : packetIn) =
	Printf.printf "%s\n%!" (packetIn_to_string pk);
	let out_ch, in_ch = get_ch () in
	respond_to_packet Mac.mac_learning_program sw xid pk out_ch in_ch;;
(*	let _ = print_endline "respond_to_packet returned" in
	List.iteri (fun index triple -> match triple with (swOut, xOut, pkOut) -> send_packet_out swOut (Int32.of_int index) pkOut) output;
	print_endline ("sent " ^ (string_of_int (List.length output)) ^ "packets");*)

end

module Controller = OxStart.Make (OxController);;

(*open OxPlatform
open OpenFlow0x01_Core*)

module MyApplication = struct

  include OxStart.DefaultTutorialHandlers;;

  let switch_connected (sw : switchId) : unit =
    Printf.printf "Switch %Ld connected.\n%!" sw;;

  (* [FILL] This packet_in function sends all packets out of port 1.
     Modify it to behave like a repeater: send the packet out of all
     ports, except its input port. *)
  let packet_in (sw : switchId) (xid : xid) (pk : packetIn) : unit =
    Printf.printf "%s\n%!" (packetIn_to_string pk);                                                                                                                               
    send_packet_out sw 0l
      { output_payload = pk.input_payload;                                                                                                                                        
        port_id = None;                                                                                                                                                           
        apply_actions = if pk.port = 1 then [Output(PhysicalPort(2))] else [Output(PhysicalPort(1))]; (* <---- this was the edit *)
      };;

end

(*module Controller = OxStart.Make (MyApplication);;*)