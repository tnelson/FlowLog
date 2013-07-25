open Unix;;
open Printf;;
open Types;;
open Type_Helpers;;
(*open Flowlog_Thrift_Out;;*)

let debug = true;;

module Xsb = struct
	
    (* creates a pair channels for talking to xsb, starts xsb, and returns the channels *)
	let start_xsb () : out_channel * in_channel * in_channel =
		let xin_channel, xout_channel, error_channel = Unix.open_process_full "xsb" (Unix.environment ()) in
		(xout_channel, xin_channel, error_channel);;

	let ref_out_ch = ref None;;
	let ref_in_ch = ref None;;
	let ref_err_ch = ref None;;

	let get_ch () : out_channel * in_channel = 
		match !ref_out_ch with
		| None -> let out_ch, in_ch, err_ch = start_xsb () in 
			let _ = ref_out_ch := Some(out_ch) in
			let _ = ref_in_ch := Some(in_ch) in
			let _ = ref_err_ch := Some(err_ch) in
			(out_ch, in_ch);
		| Some(out_ch) -> (match !ref_in_ch with
			|Some(in_ch) -> (out_ch, in_ch);
			| _ -> raise (Failure "ref_out_ch is some but ref_in_ch is none"););;

	let halt_xsb () : unit = 
		let out_ch, _ = get_ch () in
		output_string out_ch "halt.\n";
		flush out_ch;;

	(* because Tim can't find a non-blocking read similar to read-bytes-avail in Racket,
	    this halts XSB, then terminates  *)
	let debug_print_errors_and_exit () : unit =
	  halt_xsb();	    	    
	  let errstr = ref "" in
	  try
	    while true do
            errstr := !errstr ^ (String.make 1 (input_char (match !ref_err_ch with 
                                               | Some(ch) -> ch;
                                               | _ -> raise (End_of_file))));                      
	      done
	  with End_of_file -> Printf.printf "%s\n%!" !errstr; exit(1);;

	(* Prints the XSB listings currently asserted to stdout.
	   This function is useful for confirming that XSB knows what we think it knows. *)
	let debug_print_listings () : unit =
	    Printf.printf "---------------- PRINTING LISTINGS ----------------\n%!";
		let out_ch, in_ch = get_ch () in
		output_string out_ch ("listing.\n"); flush out_ch;

		let next_str = ref (input_line in_ch) in
		  Printf.printf "%s\n%!" !next_str;
		  while not (Type_Helpers.ends_with (String.trim !next_str) "yes") do
			next_str := input_line in_ch;
			Printf.printf "%s\n%!" !next_str;
		  done;
		  Printf.printf "-------------------------------------------------\n%!";;


	(* This takes in a string command (not query, this doesn't deal with the semicolons).
	It writes the command to xsb and returns the resulting text. *)
	let send_assert (str : string) : string =
	    if debug then Printf.printf "send_assert: %s\n%!" str;
		let out_ch, in_ch = get_ch () in
		output_string out_ch (str ^ "\n");
		flush out_ch;		
		let answer = ref "" in
		let next_str = ref "" in		
		while (not (Type_Helpers.ends_with (String.trim !next_str) "yes") && not (Type_Helpers.ends_with (String.trim !next_str) "no")) do		
			next_str := input_line in_ch;

			(* Do not use this: it won't work. But it is useful for debugging situations with weird XSB output. *)
			(*next_str := (!next_str) ^ (String.make 1 (input_char in_ch));*)

            if debug then Printf.printf "DEBUG: send_assert %s getting response. Line was: %s\n%!" str (!next_str);
			answer := (!answer ^ "\n" ^ String.trim !next_str);
		done;
		(* if debug then Printf.printf "send_assert answer: %s\n%!" (String.trim !answer); *)
		String.trim !answer;;


	(* Removes str2 from the end of str1 if its there, otherwise returns str1 *)
	let remove_from_end (str1 : string) (str2 : string) : string = 
		if Type_Helpers.ends_with str1 str2
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
	    if debug then Printf.printf "send_query: %s (#vars: %d)\n%!" str num_vars;
		let out_ch, in_ch = get_ch () in
		output_string out_ch (str ^ "\n");
		flush out_ch;
		(*let first_line = input_line in_ch in
		if ((ends_with (String.trim first_line) "no") || (ends_with (String.trim first_line) "yes")) then [] else*)
		let answer = ref [] in

		let next_str = ref (input_line in_ch) in

		(* Do not use this: it won't work. But it is useful for debugging situations with weird XSB output. 
           Note the debug_print_errors_and_exit() call---catches error case (which has no endline at end of input) *)
        (*let next_str = ref "" in
		while not (Type_Helpers.ends_with !next_str "\n") do
		  next_str := (!next_str) ^ (String.make 1 (input_char in_ch));
		  Printf.printf "next_str=%s\n%!" !next_str;
		  if (Type_Helpers.ends_with !next_str "| ?- | ?-") then debug_print_errors_and_exit();
		done;*)
		
		let counter = ref 0 in
		while not (Type_Helpers.ends_with (String.trim !next_str) "no") do
			if debug then Printf.printf "DEBUG: send_query %s getting response. Line was: %s\n%!" str (!next_str);
			if (!counter mod num_vars = 0) then
			(output_string out_ch ";\n";
			flush out_ch);
			counter := !counter + 1;

			next_str := input_line in_ch;
		
        (*next_str := "";
        while not (Type_Helpers.ends_with !next_str "\n") do
		  next_str := (!next_str) ^ (String.make 1 (input_char in_ch));
		  Printf.printf "next_str=%s\n%!" !next_str;
		  if (Type_Helpers.ends_with !next_str "| ?- | ?-") then debug_print_errors_and_exit();
		done;*)

			answer := (remove_from_end (String.trim !next_str) "no") :: !answer;
			(* TODO If num_vars is wrong, this will freeze. Can we improve? *)
		done;
		if debug then Printf.printf "send_query finished. answers: \n%s\n%!" (String.concat ", " !answer);
		List.map (fun (l : string list) -> List.map after_equals l) (group (List.rev !answer) num_vars);;

end


(* Provides functions for high level communication with XSB. *)
(* Right now ignoring queries. *)
module Communication = struct

	(* assertion, number of answers to expect (number of variables in clause) *)
	(* if this is a query with 0 variables, will call send_assert and thus need to provide [] vs [[]] *)
	let send_message (message : string) (num_ans : int) : (string list) list =
		if num_ans > 0 then
		                Xsb.send_query message num_ans 
		              else let yn = Xsb.send_assert message in
		                if (Type_Helpers.ends_with yn "yes") then [[]]
		                else []

	(* Returns x :: l if x not already in l *)
	let add_unique (x : 'a) (l : 'a list) : 'a list = if List.mem x l then l else x :: l;;
	
	(* Same as add_unique but only if x is a Variable *)
	let add_unique_var (t : Types.term) (acc : Types.term list) : Types.term list = 
		match t with
		| Types.Constant(_, _) -> acc;
		| Types.Variable(name, Types.Type(_, fields)) -> List.fold_right (fun field acc1 -> add_unique (Types.Field_ref(name, field)) acc1) fields acc;
		| Types.Field_ref(_, _) -> add_unique t acc;
		| _ -> acc;;
	
	let get_vars (cls : Types.clause) : Types.term list =
		match cls with Types.Clause(Types.Signature(_, _, _, args), body) ->
		List.fold_right (fun a acc -> match a with
				| Types.Equals(_, t1, t2) -> add_unique_var t1 (add_unique_var t2 acc);
				| Types.Apply(_, _, _, tl) -> List.fold_right add_unique_var tl acc;
				| Types.Bool(_) -> acc;) body (List.fold_right add_unique_var args []);;


	(* ignoring blackbox queries for the moment *)
	let retract_signature (s : Types.signature) : unit =
		match s with Types.Signature(_, _, _, args) ->
		let num_vars = List.length (List.fold_right (fun t acc -> add_unique_var t acc) args []) in
		let _ = send_message ("retract((" ^ (Type_Helpers.signature_to_string s) ^ ")).") num_vars in ();;

	let assert_signature (s : Types.signature) : unit =
		retract_signature s;
		match s with Types.Signature(_, _, _, args) ->
		let num_vars = List.length (List.fold_right (fun t acc -> add_unique_var t acc) args []) in
		let _ = send_message ("assert((" ^ (Type_Helpers.signature_to_string s) ^ ")).") num_vars in ();;	

	let rec split_list (num : int) (l : 'a list) : 'a list * 'a list =
		if num < 0 then raise (Failure "num should be nonnegative") else
		if num = 0 then ([], l) else
		match l with
		| [] -> raise (Failure "num is bigger than the length of the list");
		| h :: t -> let first_recur, rest_recur = split_list (num - 1) t in (h :: first_recur, rest_recur);;

	let rec group_into_constants (sl : string list) (types : Types.term_type list) : Types.term list =
		match types with
		| [] -> if sl = [] then [] else raise (Failure "More strings than fit into the types");
		| Types.Type(_, fields) as t :: tail ->
			let (first_bunch, rest) = split_list (List.length fields) sl in
			Types.Constant(first_bunch, t) :: group_into_constants rest tail;
		| _ -> raise (Failure "deferd type");;

	let get_queries (prgm : Types.program) (cls : Types.clause) : (Types.atom * Types.blackbox) list =
		match prgm with Types.Program(_, _, blackboxes, _, _) ->
		match cls with Types.Clause(_, body) -> List.fold_right (fun a acc -> match a with
			| Types.Apply(b, bb_name, rel_name, tl) -> (match List.filter (fun bb -> match bb with Types.BlackBox(name, _) -> bb_name = name) blackboxes with
				| [] -> acc;
				| h :: _ -> (a, h) :: acc;);
			| _ -> acc;) body [];;

	let query_signature (prgm : Types.program) (s : Types.signature) : (Types.term list) list =
		match s with Types.Signature(_, _, _, args) ->
		let num_vars = List.length (List.fold_right (fun t acc -> add_unique_var t acc) args []) in

		(*match prgm with Types.Program(_, _, _, _, prgm_clauses) ->
		let clauses = List.filter (fun cls -> Type_Helpers.clause_signature cls = Type_Helpers.signature_name s) prgm_clauses in
		let queries = List.fold_right (fun cls acc -> (get_queries prgm cls) @ acc) clauses [] in*)

		(* CONTINUE QUERY STUFF HERE USING ABOVE COMMENTED OUT CODE AND PREVIOUS FUNCTION *)

		(* need to populate BB query helpers before here, and de-populate when done *)
		let strings = send_message ((Type_Helpers.signature_to_string s) ^ ".") num_vars in

		let types = List.map Type_Helpers.type_of_term (List.filter (function Types.Constant(_,_) -> false; | _ -> true;) args) in
		List.map (fun sl -> group_into_constants sl types) strings;;




	(*let send_relation (rel : Types.relation) (args : Types.term list) (process : string -> string -> string) : (Types.term list) list =
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
		if debug then print_endline ("sending: " ^ str);
		send_message str (List.length vars);;

	let assert_queries (qs : (atom * string list list) list) : unit =
		List.iter (function (q, ans) -> match q with
			| Types.Query(bb, str, tl) -> List.iter (fun sl -> send_message ("assert((" ^ str ^ "/" ^ (Types.blackbox_name bb) ^ "(" ^ (Type_Helpers.list_to_string (fun x -> x) sl) ^ ")))." )) ans;
			| _ -> raise (Failure "only queries allowed here");) qs;;

	let retract_queries (qs : (atom * string list list) list) : unit =
		List.iter (function (q, ans) -> match q with
			| Types.Query(bb, str, tl) -> List.iter (fun sl -> send_message ("retract((" ^ str ^ "/" ^ (Types.blackbox_name bb) ^ "(" ^ (Type_Helpers.list_to_string (fun x -> x) sl) ^ ")))." )) ans;
			| _ -> raise (Failure "only queries allowed here");) qs;;


let get_queries (cls : Types.clause) : Types.atom list =
		List.fold (fun lit acc -> 
			let a = Type_Helpers.get_atom lit in
			match a with
			| Types.Query(_, _, _) -> a :: acc;
			| _ -> acc;) (Type_Helpers.clause_body cls) [];;


	let query_relation (rel : Types.relation) (args : Types.argument list) : (Types.term list) list = 
		let queries = List.fold(fun cls acc -> (get_queries cls) @ acc) (Type_Helpers.relation_body rel) in
		let query_answers = List.map (fun q -> match q with 
			| Types.Query(bb, str, tl) -> (q, Flowlog_Thrift_Out.doBBquery bb q);
			| _ -> raise (Failure "this is only for queries");) queries in
		assert_queries query_answers;
		let ans = send_relation rel (Type_Helpers.arguments_to_terms args) (fun name args_string -> name ^ "(" ^ args_string ^ ").") in
		retract_queries query_answers;
		ans;;*)

(*
	let query_relation (rel : Types.relation) (args : Types.argument list) : (Types.term list) list =
		if debug then print_endline ("query relation: " ^ (Type_Helpers.relation_name rel) ^ "(" ^ 
			(Type_Helpers.list_to_string Type_Helpers.argument_to_string args) ^ ")");
		let ans = send_relation rel (Type_Helpers.arguments_to_terms args) (fun name args_string -> name ^ "(" ^ args_string ^ ").") in
		if debug then List.iter (fun tl -> Printf.printf "query answer: %s\n%!" (Type_Helpers.list_to_string Type_Helpers.term_to_string tl)) ans;
		ans;;
*)

	(*let retract_relation (rel : Types.relation) (args : Types.term list) : unit =
		let _ = send_relation rel args (fun name args_string -> 
			"retract((" ^ name ^ "(" ^ args_string ^ "))).") in ();;

	let assert_relation (rel : Types.relation) (args : Types.term list) : unit =
		retract_relation rel args;
		let _ = send_relation rel args (fun name args_string -> 
			"assert((" ^ name ^ "(" ^ args_string ^ "))).") in ();;*)


	let start_clause (cls : Types.clause) : unit =
		if debug then print_endline ("start_clause: assert((" ^ (Type_Helpers.clause_to_string cls) ^ ")).");
		if debug then (List.iter (fun t -> (Printf.printf "var: %s\n%!" (Type_Helpers.term_to_string t))) (get_vars cls));
		let _ = send_message ("assert((" ^ (Type_Helpers.clause_to_string cls) ^ ")).") (List.length (get_vars cls)) in ();;
		

(*	let start_relation (rel : Types.relation) : unit =
		match Type_Helpers.relation_body rel with
		| [] -> start_clause (Types.HelperClause(Type_Helpers.relation_name rel, Type_Helpers.relation_arguments rel, []));
		| body -> List.iter start_clause body;;*)

	(* assuming all implicitly defined clauses have been added to list of clauses *)
	let start_program (prgm : Types.program) : unit =
		print_endline "starting program.";
		match prgm with Types.Program(_, _, _, _, clauses) ->
		List.iter start_clause clauses;;

end