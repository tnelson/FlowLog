open Unix;;
open Xsb;;
open Packet;;
open OxPlatform;;
open OpenFlow0x01_Core;;

let debug = true;;

(* Defines the basic types for the Flowlog interpreter. *)
module Syntax = struct
	(* type name, field names *)
	type notif_type = Type of string * string list;;
	(* type, name *)
	type notif_var = Notif_var of notif_type * string;;
	(* constants and variables or a field of a value (like pkt.locPt) *)
	type term = Constant of string | Variable of string | Field_ref of notif_var * string;;
	(* type of actual arriving notification. type and values *)
	type notif_val = Notif_val of notif_type * term list;;
	(* things like A = B or R(A, B, C) *)
	type atom = Equals of term * term | Apply of string * term list | Bool of bool;;
	(* atoms and negations of atoms *)
	type literal = Pos of atom | Neg of atom;;
	(* argument to a clause is either a notif_var or a term *)
	type argument = Arg_notif of notif_var | Arg_term of term;;
	(* name, arguments, body *)
	type clause = Clause of string * argument list * literal list;;
	(* name, arguments, clauses *)
	
	type relation = Relation of string * argument list * clause list;;
	(* name, relations *)

	type program = Program of string * relation list;;
	
	let packet_type = Type("packet", ["LocSw"; "LocPt"; "DlSrc"; "DlDst"; "DlTyp"; "NwSrc"; "NwDst"; "NwProto"]);;
	let switch_port_type = Type("switch_port", ["Sw"; "Pt"]);;

(*	let shp_vars = List.map (fun (str : string) -> Variable(str)) ["LocSw"; "LocPt2"];;
	let shp_name = "__switch_has_ports";;*)

end

(* Provides printing functions and conversion functions both for pretty printing and communication with XSB. *)
module Type_Helpers = struct
	include Syntax;;

	let list_to_string (conversion : 'a -> string) (l : 'a list) : string = 
		let ans = List.fold_right (fun x acc -> (conversion x) ^ "," ^ acc) l "" in
		if ans = "" then ans else String.sub ans 0 (String.length ans - 1);;

	let notif_var_name (n : notif_var) : string =
		match n with Notif_var(t, str) -> str;;

	let notif_var_to_terms (n : notif_var) : term list =
		match n with Notif_var(Type(_, fields), name) ->
		List.map (fun field -> Field_ref(n, field)) fields;;

	let term_to_string (t : term) : string = 
		match t with
		| Constant(c) -> c; 
		| Variable(v) -> v;
		| Field_ref(notif, field) -> (notif_var_name notif) ^ "_" ^ field;;

	let notif_var_to_string (n : notif_var) : string = 
		list_to_string term_to_string (notif_var_to_terms n);;

	let notif_type_to_string (ntype : notif_type) : string = 
		match ntype with Type(name, fields) -> name ^ ": {" ^ (list_to_string (fun str -> str) fields) ^ "}";;

	let notif_val_to_string (n : notif_val) : string =
		match n with Notif_val(ntype, terms) -> (notif_type_to_string ntype) ^ " : " ^ (list_to_string term_to_string terms);;

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

	let argument_to_string (arg : argument) : string =
		match arg with
		| Arg_notif(n) -> notif_var_to_string n;
		| Arg_term(t) -> term_to_string t;;

	let arguments_to_terms (args : argument list) : term list =
		List.fold_right (fun arg acc ->
		match arg with
		| Arg_term(t) -> t :: acc;
		| Arg_notif(n) -> (notif_var_to_terms n) @ acc) args [];;

	let rec drop (l : 'a list) (n : int) : 'a list = 
		if n <= 0 then l else
		match l with
		| [] -> [];
		| h :: t -> drop t (n - 1);;

	let rec take (l : 'a list) (n : int) : 'a list =
		if n <= 0 then [] else
		match l with
		| [] -> [];
		| h :: t -> h :: take t (n-1);;

	let terms_to_notif_val (ntype : notif_type) (terms : term list) : notif_val =
		match ntype with Type(_, names) ->
		if (List.length terms = List.length names) 
		then Notif_val(ntype, terms) 
		else raise (Failure "Tried to create notification with wrong number of terms.");;

	let clause_to_string (cl : clause) : string =
		match cl with
		| Clause(str, args, []) -> str ^ "(" ^ (list_to_string argument_to_string args) ^ ")";
		| Clause(str, args, body) -> str ^ "(" ^ (list_to_string argument_to_string args) ^ ") :- " ^
			(list_to_string literal_to_string body);;

	let relation_name (rel : relation) : string = 
		match rel with
		Relation(str, _, _) -> str;;

	let relation_trigger_type (rel : relation) : notif_type option =
		let this_debug = false in
		let _ = if this_debug then print_endline ("relation_trigger_type: " ^ (relation_name rel) ^ ": ") in
		match rel with Relation(_, args, _) -> 
		match args with
		| [] -> None;
		| h :: tail -> match h with
			| Arg_term(_) -> let _ = if this_debug then print_endline "None" in None;
			| Arg_notif(n) -> match n with Notif_var(t, _) -> let _ = if this_debug then print_endline (notif_type_to_string t) in Some t;;

	let print_relation (rel : relation) : unit =
		match rel with
		Relation(name, args, clauses) -> match clauses with
		| [] -> print_endline (clause_to_string (Clause(name, args, [])));
		|_ -> List.iter (fun cls -> print_endline (clause_to_string cls)) clauses;;

	let find_relation_by_name (prgm : program) (name : string) : relation option = 
		match prgm with Program(_, relations) -> List.fold_right (fun r acc -> if name = (relation_name r) then Some(r) else acc) relations None;;

	let is_forward_relation (prgm : program) (rel : relation) : bool =
		match prgm with Program(prgm_name, relations) ->
		match rel with Relation(rel_name, _, _) ->
		List.mem rel relations && rel_name = "forward/" ^ prgm_name;;

	let forward_relation (prgm : program) : relation =
		match prgm with Program(name, _) ->
		match find_relation_by_name prgm ("forward/" ^ name) with
		| None -> raise (Failure "This program does not have a forward relation.");
		| Some rel -> rel;;

end 

(* Provides functions for high level communication with XSB. *)
module Flowlog_Xsb = struct	
	include Type_Helpers;;

	(* Returns x :: l if x not already in l *)
	let add_unique (x : 'a) (l : 'a list) : 'a list = if List.mem x l then l else x :: l;;
	
	(* Same as add_unique but only if x is a Variable *)
	let add_unique_var (t : term) (acc : term list) : term list = 
		match t with
		| Constant(_) -> acc;
		| Variable(_) -> add_unique t acc;
		| Field_ref(_, _) -> add_unique t acc;;
	
	(* Takes a desugared clause (i.e. one whose arguments are all terms and body contains no Field_refs) and
		returns the number of variables in the clause *)
	let get_vars (cl : clause) : term list =
		match cl with
		| Clause(_, args, body) -> List.fold_right 
			(fun (lit : literal) (acc : term list) -> 
				match get_atom(lit) with
				| Equals(t1, t2) -> add_unique_var t1 (add_unique_var t2 acc);
				| Apply(_, tl) -> List.fold_right add_unique_var tl acc;
				| Bool(b) -> acc;)
			body
			(List.fold_right add_unique_var (arguments_to_terms args) []);;

	let send_clause (cl : clause) (assertion : string) (out_ch : out_channel) (in_ch : in_channel) : 'a list =
		let _ = if debug then print_endline assertion in
		let num_vars = List.length (get_vars cl) in
		let answer = (if num_vars > 0 then Xsb.send_query assertion (List.length (get_vars cl)) out_ch in_ch
		else let _ = Xsb.send_assert assertion out_ch in_ch in []) in
		List.map (fun (l : string list) -> List.map (fun (s : string) -> Constant(s)) l) answer;;
	
	let query_clause (cl : clause) (out_ch : out_channel) (in_ch : in_channel): (term list) list =
		send_clause cl (match cl with
			| Clause(str, args, _) -> str ^ "(" ^ (list_to_string argument_to_string args) ^ ").") out_ch in_ch;;
	
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
	
	let query_relation (rel : relation) (args : argument list) (out_ch : out_channel) (in_ch : in_channel) : (term list) list =
		let _ = if debug then print_endline ("query relation: " ^ (relation_name rel) ^ (list_to_string argument_to_string args)) in
		let ans = query_clause (Clause((relation_name rel), args, [])) out_ch in_ch in
		let _ = if debug then print_endline (list_to_string (list_to_string term_to_string) ans) in
		ans;;

end

(* Provides functions for running a Flowlog program.
ASSUMPTIONS: We assume that programs passed into functions in this module have
1) all implied relations defined (i.e. if there's +R or -R then there's R)
2) a forward relation named forward/(name of program). The arguments of this relation
are an incoming and outgoing packet respectively.
3) all relations have names ending in /(name of program).
4) the name of the program is lower case.
*)
module Evaluation = struct
	include Flowlog_Xsb;;

	(* returns a list of chars for s *)
	let explode (str : string) : char list =
  		let rec expl i l =
    	if i < 0 then l else
    	expl (i - 1) (str.[i] :: l) in
  	expl (String.length str - 1) [];;

  	(* takes a list of chars and makes a string *)
	let implode (l : char list) : string =
  		let result = String.create (List.length l) in
  		let rec imp i = function
  		| [] -> result
  		| c :: l -> result.[i] <- c; imp (i + 1) l in
  		imp 0 l;;

	let fire_relation (rel : relation) (notif : notif_val) (prgm : program) (out_ch : out_channel) (in_ch : in_channel) : notif_val list =
		let _ = if debug then print_endline ("fire_relation: " ^ (relation_name rel)) in
		match rel with Relation(name, args, _) -> 
		match args with
		| [] -> raise (Failure "called fire_relation on a relation with empty args.");
		| head :: tail -> match notif with Notif_val(_, terms) ->
		let results = query_relation rel ((List.map (fun t -> Arg_term(t)) terms) @ tail) out_ch in_ch in
		match notif with Notif_val(ntype, _) ->
		if (is_forward_relation prgm rel) then List.map (terms_to_notif_val ntype) results else
		let _ = (match explode name with
		| '+' :: rest -> List.iter (fun terms -> let _ = tentative_assert_clause (Clause(implode rest, (List.map (fun x -> Arg_term(x)) terms), [])) out_ch in_ch in ()) results;
		| '-' :: rest -> List.iter (fun terms -> let _ = retract_clause (Clause(implode rest, (List.map (fun x -> Arg_term(x)) terms), [])) out_ch in_ch in ()) results;
		| _ -> ()) in [];;


	(* takes in a notification, fires all action rules (+..., -..., forward, ...)
	sends the appropriate things to xsb and returns a list of notifications to be sent out (such as packets) *)
	let respond_to_notification (notif : notif_val) (prgm : program) (out_ch : out_channel) (in_ch : in_channel) : notif_val list = 
		let _ = if debug then print_endline (notif_val_to_string notif) in
		match prgm with Program(name, relations) ->
		match notif with Notif_val(ntype, _) ->
		let forward_rel = forward_relation prgm in
		let forward_ans = if ((relation_trigger_type forward_rel) = Some ntype) then fire_relation forward_rel notif prgm out_ch in_ch else [] in
		(List.fold_right (fun rel acc ->
			if rel <> forward_rel then
			(match (relation_trigger_type rel) with
			| None -> acc;
			| Some(t) -> if t = ntype then (fire_relation rel notif prgm out_ch in_ch) @ acc else acc) else acc) relations []) @ forward_ans;;


	let start_program (prgm : program) (out_ch : out_channel) (in_ch : in_channel) : (term list) list = 
		match prgm with
		| Program(name, relations) -> List.fold_right (fun rel acc -> (assert_relation rel out_ch in_ch) @ acc) relations [];;

end


(*module Flowlog_Parsing = struct
	include Evaluation;;

	(* Makes all relations except those defined implicitly by other relations starting with + or - *)
	let rec make_relations_helper_1 (clist : clause list) : relation list =
		match clist with
		| [] -> [];
		| h :: t -> match h with Clause(clause_name, clause_args, clause_body) ->
			let recur = (make_relations_helper_1 t) in
			let answer, same_name = List.fold_right (fun rel acc ->
				match acc with (already_there_acc, same_name_acc) ->
				match rel with Relation(rel_name, rel_args, rel_clauses) ->
				if rel_name = clause_name then (Relation(rel_name, rel_args, h :: rel_clauses) :: already_there_acc, rel :: same_name_acc)
				else (rel :: already_there_acc, same_name_acc)) recur ([],[]) in
			match same_name with
			| [] -> Relation(clause_name, clause_args, [h]) :: answer;
			| _ -> answer;;

	(* takes in the output of make_relations_helper_1 and makes the implicit relations *)
	let make_relations_helper_2 (rlist : relation list) : relation list = 
		let to_add = List.fold_right (fun rel acc ->
			match rel with Relation(name, args, _) ->
			if ((begins_with name "+") || (begins_with name "-")) then 
				let new_name = String.sub name 1 (String.length name -1) in
				let in_list_function = fun rel1 acc1 -> ((relation_name rel1) = new_name) || acc1 in
				let in_old_list = List.fold_right in_list_function rlist false in
				let in_acc = List.fold_right in_list_function acc false in
				if (not in_old_list) && (not in_acc) then Relation(new_name, drop args (List.length packet_vars), []) :: acc else acc;
			else acc) rlist [] in
		to_add @ rlist;;

	let make_relations (clist : clause list) : relation list =
		let ans = make_relations_helper_2 (make_relations_helper_1 clist) in
		let _ = if debug then List.iter print_relation ans in
		let _ = if debug then print_endline (string_of_int (List.length ans)) in
		ans;;


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

	let append_name (name : string) (str : string) : string =
		if not (String.contains str '/') then str ^ "/" ^ name else str;;

	let make_program (name : string) (relations : relation list) : program =
		let ans = Program(name, List.map (process_relation_name (append_name name)) relations) in
		let _ = if debug then print_endline name in
		let _ = if debug then List.iter print_relation (List.map (process_relation_name (append_name name)) relations) in
		ans;;

	let rec remove_duplicates (l : 'a list) : 'a list =
		match l with
		[] -> [];
		| h :: t -> if List.mem h t then (remove_duplicates t) else h :: (remove_duplicates t);;

	let import (pg1 : program) (pg2 : program) : program = 
		match pg1 with Program(name_1, relations_1) ->
		match pg2 with Program(name_2, relations_2) ->
		Program(name_1, remove_duplicates (relations_1 @ relations_2));;

end*)