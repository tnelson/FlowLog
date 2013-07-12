open Unix;;
open Printf;;
open Flowlog_Types;;

module Xsb = struct
	
	(* creates a pair channels for talking to xsb, starts xsb, and returns the channels *)
	let start_xsb () : out_channel * in_channel =
		let xin_channel, xout_channel, error_channel = Unix.open_process_full "xsb" (Unix.environment ()) in
		(xout_channel, xin_channel);;

	let ref_out_ch = ref None;;
	let ref_in_ch = ref None;;
	
	let get_ch () : out_channel * in_channel = 
		match !ref_out_ch with
		| None -> let out_ch, in_ch = start_xsb () in 
			let _ = ref_out_ch := Some(out_ch) in
			let _ = ref_in_ch := Some(in_ch) in
			(out_ch, in_ch);
		| Some(out_ch) -> match !ref_in_ch with
			|Some(in_ch) -> (out_ch, in_ch);
			| _ -> raise (Failure "ref_out_ch is some but ref_in_ch is none"));;
		
	let halt_xsb (out_ch : out_channel) : unit = 
		output_string out_ch "halt.\n";
		flush out_ch;


	(* True if string str1 ends with string str2 *)
	let ends_with (str1 : string) (str2 : string) : bool = 
		if String.length str2 > String.length str1
		then false
		else (String.sub str1 ((String.length str1) - (String.length str2)) (String.length str2)) = str2;;

	(* This takes in a string command (not query, this doesn't deal with the semicolons).
	It writes the command to xsb and returns the resulting text. *)
	let send_assert (str : string) : string =
		let out_ch, in_ch = get_ch () in
		output_string out_ch (str ^ "\n");
		flush out_ch;
		let answer = ref "" in
		let next_str = ref "" in
		while (not (ends_with (String.trim !next_str) "yes") && not (ends_with (String.trim !next_str) "no")) do
			next_str := input_line in_ch;
            (*print_endline ("DEBUG: send_assert "^ str ^" getting response. Line was: "^(!next_str));*)
			answer := (!answer ^ "\n" ^ String.trim !next_str);
		done;
		String.trim !answer;;


	(* Removes str2 from the end of str1 if its there, otherwise returns str1 *)
	let remove_from_end (str1 : string) (str2 : string) : string = 
		if ends_with str1 str2
		then String.sub str1 0 ((String.length str1) - (String.length str2))
		else str1;; 

	(* groups elements of alist into lists of size num except possibly the first one *)
	let rec group (alist : 'a list) (num : int) : ('a list) list = 
		match alist with
		| [] -> [];
		| f :: r -> match group r num with
			| [] -> [[f]];
			| f1 :: r1 -> if List.length f1 < num
						then (f :: f1) :: r1
						else [f] :: (f1 :: r1);;

	let after_equals (str : string) : string =
		let equals_index = try String.index str '=' with Not_found -> -1 in
		String.trim (String.sub str (equals_index + 1) (String.length str - equals_index - 1));;

	(* Takes a string query (thing with semicolon answers), the number of variables involved.
	 It writes the query to xsb and returns a list of lists with all of the results. *)
	let send_query (str : string) (num_vars : int) : (string list) list =
		let out_ch, in_ch = get_ch () in
		output_string out_ch (str ^ "\n");
		flush out_ch;
		(*let first_line = input_line in_ch in
		if ((ends_with (String.trim first_line) "no") || (ends_with (String.trim first_line) "yes")) then [] else*)
		let answer = ref [] in
		let next_str = ref (input_line in_ch) in
		(*let _ = print_endline (string_of_bool (ends_with (String.trim !next_str) "no")) in*)
		let counter = ref 0 in
		while not (ends_with (String.trim !next_str) "no") do
			if (!counter mod num_vars = 0) then
			(output_string out_ch ";\n";
			flush out_ch);
			counter := !counter + 1;
			next_str := input_line in_ch;
			answer := (remove_from_end (String.trim !next_str) "no") :: !answer;
		done;
		List.map (fun (l : string list) -> List.map after_equals l) (group (List.rev !answer) num_vars);;

end


(* Provides functions for high level communication with XSB. *)
(* Right now ignoring queries. *)
module Communication = struct

	(* assertion, number of answers to expect (number of variables in clause) *)
	type message = Message of string * int;;

	let send_message (mes : message) : (Types.term list) list =
		match mes with Message(assertion, num_vars) ->
		let answer = (if num_vars > 0 then Xsb.send_query assertion num_vars else let _ = Xsb.send_assert assertion in []) in
		List.map (fun (l : string list) -> List.map (fun str -> Types.Constant(str)) l) answer;;

	(* need: 
	val query_relation : Types.relation -> Types.argument list -> (Types.term list) list;;
	val retract_relation : Types.relation -> Types.term list -> unit;;
	val assert_relation : Types.relation -> Types.term list -> unit;; 
	*)

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

	let send_clause (cl : clause) (assertion : string) (out_ch : out_channel) (in_ch : in_channel) : (term list) list =
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

let start_program (prgm : Types.program) (out_ch : out_channel) (in_ch : in_channel) : unit = 
		match prgm with
		| Types.Program(name, relations) -> List.iter (fun rel -> Communication.assert_relation rel out_ch in_ch) relations;;

end