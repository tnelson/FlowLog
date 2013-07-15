open Unix;;
open Printf;;
open Flowlog_Types;;
open Type_Helpers;;

let debug = true;;

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
		| Some(out_ch) -> (match !ref_in_ch with
			|Some(in_ch) -> (out_ch, in_ch);
			| _ -> raise (Failure "ref_out_ch is some but ref_in_ch is none"););;
		
	let halt_xsb () : unit = 
		let out_ch, _ = get_ch () in
		output_string out_ch "halt.\n";
		flush out_ch;;


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
	let send_message (message : string) (num_ans : int) : (Types.term list) list =
		let answer = (if num_ans > 0 then Xsb.send_query message num_ans else let _ = Xsb.send_assert message in []) in
		List.map (fun (l : string list) -> List.map (fun str -> Types.Constant(str)) l) answer;;

	let send_relation (rel : Types.relation) (args : Types.term list) (process : string -> string -> string) : (Types.term list) list =
		let vars = List.filter (function 
			| Types.Variable(_) -> true;
			| Types.Field_ref(_, _) -> true;
			| Types.Constant(_) -> false;) args in
		let args_string = (Type_Helpers.list_to_string Type_Helpers.term_to_string args)  in
		let str = (match rel with
		| Types.PlusRelation(name, _, _) -> process ("+" ^ name) args_string;
		| Types.MinusRelation(name, _, _) -> process ("-" ^ name) args_string;
		| Types.HelperRelation(name, _, _) -> process name args_string;
		| Types.NotifRelation(bb, _, _) -> process (Type_Helpers.blackbox_name bb) args_string;) in
		send_message str (List.length vars);;

	let query_relation (rel : Types.relation) (args : Types.argument list) : (Types.term list) list =
		send_relation rel (Type_Helpers.arguments_to_terms args) (fun name args_string -> name ^ "(" ^ args_string ^ ").");;

	let retract_relation (rel : Types.relation) (args : Types.term list) : unit =
		let _ = send_relation rel args (fun name args_string -> 
			"retract((" ^ name ^ "(" ^ args_string ^ "))).") in ();;

	let assert_relation (rel : Types.relation) (args : Types.term list) : unit =
		let _ = retract_relation rel args in
		let _ = send_relation rel args (fun name args_string -> 
			"assert((" ^ name ^ "(" ^ args_string ^ "))).") in ();;

	(* Returns x :: l if x not already in l *)
	let add_unique (x : 'a) (l : 'a list) : 'a list = if List.mem x l then l else x :: l;;
	
	(* Same as add_unique but only if x is a Variable *)
	let add_unique_var (t : Types.term) (acc : Types.term list) : Types.term list = 
		match t with
		| Types.Constant(_) -> acc;
		| Types.Variable(_) -> add_unique t acc;
		| Types.Field_ref(_, _) -> add_unique t acc;;
	
	(* Takes a desugared clause (i.e. one whose arguments are all terms and body contains no Field_refs) and
		returns the number of variables in the clause *)
	let get_vars (cls : Types.clause) : Types.term list =
		let args, body = (Type_Helpers.clause_arguments cls, Type_Helpers.clause_body cls) in
		List.fold_right (fun (lit : Types.literal) (acc : Types.term list) -> 
				match Type_Helpers.get_atom(lit) with
				| Types.Equals(t1, t2) -> add_unique_var t1 (add_unique_var t2 acc);
				| Types.Apply(_, tl) -> List.fold_right add_unique_var tl acc;
				| Types.Query(_, _, tl) -> List.fold_right add_unique_var tl acc; 
				| Types.Bool(_) -> acc;) body (List.fold_right add_unique_var (Type_Helpers.arguments_to_terms args) []);;

	let start_clause (cls : Types.clause) : unit =
		if debug then print_endline ("assert((" ^ (Type_Helpers.clause_to_string cls) ^ ")).");
		let _ = send_message ("assert((" ^ (Type_Helpers.clause_to_string cls) ^ ")).") (List.length (get_vars cls)) in ();;
		

	let start_relation (rel : Types.relation) : unit =
		List.iter start_clause (Type_Helpers.relation_body rel);;

	let start_program (prgm : Types.program) : unit =
		print_endline "starting program.";
		match prgm with Types.Program(_, relations) ->
		List.iter start_relation relations;;

end