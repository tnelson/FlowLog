open Unix;;
open Xsb;;
open Packet;;
open OxPlatform;;
open OpenFlow0x01_Core;;

let debug = true;;

module Syntax = struct
	(* constants and variables (recall variables are uppercase) *)
	type term = Constant of string | Variable of string;;
	(* Things like A = B or R(A, B, C) *)
	type atom = Equals of term * term | Apply of string * term list | Bool of bool;;
	(* Atoms and negations of atoms *)
	type literal = Pos of atom | Neg of atom;;
	(* name, arguments, body *)
	type clause = Clause of string * term list * literal list;;
	(* name, arguments, clauses *)
	type relation = Relation of string * term list * clause list;;
	(* name, relations, forward *)
	type program = Program of string * relation list * relation;;

	let packet_vars = List.map (fun (str : string) -> Variable(str)) ["LocSw"; "LocPt"; "DlSrc"; "DlDst"; "DlTyp"; "NwSrc"; "NwDst"; "NwProto"];;
	let packet_vars_2 = List.map (fun (str : string) -> Variable(str)) ["LocSw2"; "LocPt2"; "DlSrc2"; "DlDst2"; "DlTyp2"; "NwSrc2"; "NwDst2"; "NwProto2"];;
	let shp_vars = List.map (fun (str : string) -> Variable(str)) ["LocSw"; "LocPt2"];;
end

module To_String = struct
	include Syntax;;

	let list_to_string (conversion : 'a -> string) (l : 'a list) : string = 
		let ans = List.fold_right (fun x acc -> (conversion x) ^ "," ^ acc) l "" in
		if ans = "" then ans else String.sub ans 0 (String.length ans - 1);;

	let term_to_string (t : term) : string = 
		match t with
		| Constant(c) -> c; 
		| Variable(v) -> v;;

	let atom_to_string (a : atom) : string =
		match a with
		| Equals(t1, t2) -> term_to_string(t1) ^ " = " ^ term_to_string(t2);
		| Apply(str, args) -> str ^ "(" ^ (list_to_string term_to_string args) ^ ")";
		| Bool(b) -> string_of_bool b;;

	let literal_to_string (l : literal) : string = 
		match l with
		| Pos(a) -> atom_to_string a;
		| Neg(a) -> "not(" ^ (atom_to_string a) ^ ")";;
	
	let get_atom (lit : literal) : atom =
		match lit with
		| Pos(a) -> a;
		| Neg(a) -> a;;

	let clause_to_string (cl : clause) : string =
		match cl with
		| Clause(str, args, []) -> str ^ "(" ^ (list_to_string term_to_string args) ^ ")";
		| Clause(str, args, body) -> str ^ "(" ^ (list_to_string term_to_string args) ^ ") :- " ^
			(list_to_string literal_to_string body);;

	let relation_name (rel : relation) : string = 
		match rel with
		Relation(str, _, _) -> str;;
end 

module Flowlog = struct	
	include To_String;;

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
		let _ = if debug then print_endline assertion in
		let num_vars = List.length (get_vars cl) in
		let answer = (if num_vars > 0 then Xsb.send_query assertion (List.length (get_vars cl)) out_ch in_ch
		else let _ = Xsb.send_assert assertion out_ch in_ch in []) in
		List.map (fun (l : string list) -> List.map (fun (s : string) -> Constant(s)) l) answer;;
	
	let query_clause (cl : clause) (out_ch : out_channel) (in_ch : in_channel): (term list) list =
		send_clause cl (match cl with
			| Clause(str, args, _) -> str ^ "(" ^ (list_to_string term_to_string args) ^ ").") out_ch in_ch;;
	
	let retract_clause (cl : clause) (out_ch : out_channel) (in_ch : in_channel): (term list) list =
	    send_clause cl ("retract((" ^ (clause_to_string cl) ^ ")).") out_ch in_ch;;
	
	let assert_clause (cl : clause) (out_ch : out_channel) (in_ch : in_channel): (term list) list =
		send_clause cl ("assert((" ^ (clause_to_string cl) ^ ")).") out_ch in_ch;;
	
	let tentative_assert_clause (cl : clause) (out_ch : out_channel) (in_ch : in_channel): (term list) list =
		let _ = retract_clause cl out_ch in_ch in
		assert_clause cl out_ch in_ch;;
	
	let assert_relation (rel : relation) (out_ch : out_channel) (in_ch : in_channel) : (term list) list =
		match rel with
		| Relation(name, args, []) -> assert_clause (Clause(name, args, [Pos(Bool(false))])) out_ch in_ch;
		| Relation(_, _, clauses) -> List.fold_right (fun cls acc -> (assert_clause cls out_ch in_ch) @ acc) clauses [];;
	
	let query_relation (rel : relation) (args : term list) (out_ch : out_channel) (in_ch : in_channel) : (term list) list =
		let _ = if debug then print_endline ("query relation: " ^ (relation_name rel) ^ (list_to_string term_to_string args)) in
		let ans = query_clause (Clause((relation_name rel), args, [])) out_ch in_ch in
		let _ = if debug then print_endline (list_to_string (list_to_string term_to_string) ans) in
		ans;;
	
	let rec drop (l : 'a list) (n : int) : 'a list = 
		if n <= 0 then l else
		match l with
		| [] -> [];
		| h :: t -> drop t (n - 1);;

	let find_relation_by_name (prgm : program) (name : string) : relation option = 
		match prgm with Program(_, relations,_) -> List.fold_right (fun r acc -> if name = (relation_name r) then Some(r) else acc) relations None;;

	(* memoize this function? *)
	let constrain_ports (forward : relation) : relation =
		let constraining_literal = Pos(Apply("switch_has_ports", shp_vars)) in
		match forward with Relation(name, args, clauses) -> Relation(name, args, List.map (fun cls ->
			match cls with Clause(name_1, args_1, body) -> Clause(name_1, args_1, constraining_literal :: body)) clauses);;

	let start_program (prgm : program) (out_ch : out_channel) (in_ch : in_channel) : (term list) list = 
		match prgm with
		| Program(_, relations, forward) -> let out = List.fold_right (fun rel acc -> (assert_relation rel out_ch in_ch) @ acc) relations [] in
			out @ (assert_relation (constrain_ports forward) out_ch in_ch) @ (assert_relation (Relation("switch_has_ports", shp_vars, [])) out_ch in_ch);;

	(* memoize this function? *)
	let update_switch_ports (sw : switchId) (port_nums : portId list) (out_ch : out_channel) (in_ch : in_channel) : unit =
		let sw_string = Int64.to_string sw in
		List.iter (fun port -> let _ = tentative_assert_clause (Clause("switch_has_ports", [Constant(sw_string); Constant(port)], [])) out_ch in_ch in ()) 
		(List.map string_of_int port_nums);;

	let respond_to_packet_desugared (prgm : program) (pkt : term list) (out_ch : out_channel) (in_ch : in_channel) : (term list) list =
		match prgm with
		| Program(_, relations, forward) ->
			let answer = match forward with
			| Relation(_, args, _) -> query_relation (constrain_ports forward) (pkt @ (drop args (List.length pkt))) out_ch in_ch in
			let _ = List.iter (fun rel ->
				let option_plus = find_relation_by_name prgm ("+" ^ (relation_name rel)) in
				let option_minus = find_relation_by_name prgm ("-" ^ (relation_name rel)) in
				let to_assert = (match option_plus with
				| None -> [];
				| Some(plus) -> match plus with | Relation(_, args, _) -> 
					query_relation plus (pkt @ (drop args (List.length pkt))) out_ch in_ch;) in
				let to_retract = (match option_minus with
				| None -> [];
				| Some(minus) -> match minus with | Relation(_, args, _) -> 
					query_relation minus (pkt @ (drop args (List.length pkt))) out_ch in_ch;) in
				let _ = List.iter (fun args -> let _ = tentative_assert_clause (Clause((relation_name rel), args, [])) out_ch in_ch in ()) to_assert in
				List.iter (fun args -> let _ = retract_clause (Clause((relation_name rel), args, [])) out_ch in_ch in ()) to_retract;) relations in
			answer;;
	
	let rec replace (r : string) (w : string) (st : string) : string =
		if (String.length st < String.length r || String.length st = 0) then st else
			if String.sub st 0 (String.length r) = r then w ^ (replace r w (String.sub st (String.length r) (String.length st - String.length r))) else
				(String.sub st 0 1) ^ (replace r w (String.sub st 1 (String.length st - 1)));;
	
	let pkt_to_term_list (sw : switchId) (pk : packetIn) : term list = 
		let _ = if debug then print_endline "starting pkt_to_term_list" in
		let pkt_payload = parse_payload pk.input_payload in
		let isIp = ((dlTyp pkt_payload) = 0x0800) in
		let ans = List.map (function x -> Constant(x)) [Int64.to_string sw;
		string_of_int pk.port;
		Int64.to_string pkt_payload.Packet.dlSrc;
		Int64.to_string pkt_payload.Packet.dlDst;
		string_of_int (dlTyp pkt_payload);
		Int32.to_string (nwSrc pkt_payload);
		Int32.to_string (nwDst pkt_payload);
		if isIp then (string_of_int (nwProto pkt_payload)) else "arp"] in
		let _ = if debug then print_endline ("pkt to term list: " ^ (list_to_string term_to_string ans)) in
		let _ = if debug then print_endline ("dlTyp: " ^ (string_of_int (dlTyp pkt_payload))) in
		ans;;
	
	let begins_with (str1 : string) (str2 : string) : bool = 
		if String.length str2 > String.length str1 then false else
		(String.sub str1 0 (String.length str2)) = str2;;
	
	(* notice that the current implementation is not efficient--if its just a repeater its doing way too much work. *)
	let forward_packets (tll : (term list) list) (sw : switchId) (pk : packetIn) : unit = 
		let actions_list = ref [] in
		let pk_list = pkt_to_term_list sw pk in
		let dlSrc_old = term_to_string (List.nth pk_list 2) in
		let dlDst_old = term_to_string (List.nth pk_list 3) in
		let nwSrc_old = term_to_string (List.nth pk_list 5) in
		let nwDst_old = term_to_string (List.nth pk_list 6) in
		let _ = List.iter (fun tl -> 
			let locPt_string = term_to_string (List.nth tl 1) in
			let _ = if (begins_with locPt_string "_h") then actions_list := Output(AllPorts) :: !actions_list else actions_list := Output(PhysicalPort(int_of_string locPt_string)) :: !actions_list in
	
			let dlSrc_new = term_to_string (List.nth tl 2) in
			let _ = actions_list := SetDlSrc(Int64.of_string (if (begins_with dlSrc_new "_h") then dlSrc_old else dlSrc_new)) :: !actions_list in
	
			let dlDst_new = term_to_string (List.nth tl 3) in
			let _ = actions_list := SetDlDst(Int64.of_string (if (begins_with dlDst_new "_h") then dlDst_old else dlDst_new)) :: !actions_list in
	
			let nwSrc_new = term_to_string (List.nth tl 5) in
			let _ = actions_list := SetNwSrc(Int32.of_string (if (begins_with nwSrc_new "_h") then nwSrc_old else nwSrc_new)) :: !actions_list in
	
			let nwDst_new = term_to_string (List.nth tl 6) in
			let _ = actions_list := SetNwDst(Int32.of_string (if (begins_with nwDst_new "_h") then nwDst_old else nwDst_new)) :: !actions_list in
			()) tll in
		let _ = if debug then print_endline ("print packet payload: " ^ (Packet.to_string (parse_payload pk.input_payload))) in
		send_packet_out sw 0l {output_payload = pk.input_payload; port_id = None; apply_actions = !actions_list};;
	
	let respond_to_packet (prgm : program) (sw : switchId) (xid : xid) (pk : packetIn) (out_ch : out_channel) (in_ch : in_channel) : unit = 
		let in_tl = pkt_to_term_list sw pk in
		let out_tll = respond_to_packet_desugared prgm in_tl out_ch in_ch in 
		forward_packets out_tll sw pk;;

end