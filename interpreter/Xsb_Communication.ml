(*****************************************************************)
(* Interface with XSB. Invoked by Partial_Eval etc.              *)
(* We access XSB textually via pipes. Note the special functions *)
(* to properly handle XSB's output.                              *)
(*****************************************************************)

open Unix
open Printf
open Flowlog_Types
open Flowlog_Helpers
open Flowlog_Thrift_Out
open Str
open ExtList.List

(*let debug = true;;*)

let debug = false;;

module Xsb = struct
	
    (* creates a pair channels for talking to xsb, starts xsb, and returns the channels *)
	let start_xsb () : out_channel * in_channel * in_channel =
		let xin_channel, xout_channel, error_channel = Unix.open_process_full "xsb" (Unix.environment ()) in
		(* to prevent errors from accumulating *)
		Unix.set_nonblock (Unix.descr_of_in_channel error_channel);
		printf "XSB started.\n%!";
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

	
	let print_or_flush_errors (print_too: bool) : unit =
	  let errstr = ref "" in
	  try
	    while true do
            errstr := !errstr ^ (String.make 1 (input_char
               (match !ref_err_ch with 
                   | Some(ch) -> ch;
                   | _ -> raise (End_of_file))));   
	    done
	  with | End_of_file -> if print_too then Printf.printf "%s\n%!" !errstr;
	       | Sys_blocked_io -> if print_too then Printf.printf "%s\n%!" !errstr;
	  if print_too then Printf.printf "-------------------------------------------------\n%!";;

    (* we cannot read a line at a time, since XSB will not give a newline on errors *)
	let get_line_gingerly (in_ch: in_channel) (out_ch: out_channel) (orig: string): string =
		let next_str = ref "" in	    	    
        while not (ends_with !next_str "\n") do
  		    next_str := (!next_str) ^ (String.make 1 (input_char in_ch));		 
  		  		 
  		    (*Printf.printf "glg: %s\n%!" !next_str;*)
		    while (ends_with !next_str "| ?- | ?-") do		    
		    	print_or_flush_errors (if debug then true else false);
		    	(* we may have asked an extra semicolon and caused a syntax error. *)
		    	if debug then Printf.printf "XSB Error. Asking again.\n%!";
		  		output_string out_ch (orig ^ "\n");
		    	flush out_ch;
		    	next_str := "";
		  	done
		done;
		!next_str;;

	let flush_xsb_with_dummy out_ch in_ch: unit = 
		output_string out_ch "false.\n";
		flush out_ch;
		if debug then printf "Flushing with dummy false... expect a no response\n%!";
		let l = ref "" in
			while not (!l = "no") do
				l := get_line_gingerly in_ch out_ch "false.\n" ;
				l := String.trim (Str.global_replace (Str.regexp "| \\?-") "" !l);
			done;;


	(* Prints the XSB listings currently asserted to stdout.
	   This function is useful for confirming that XSB knows what we think it knows. *)
	let debug_print_listings () : unit =
	    Printf.printf "---------------- PRINTING LISTINGS ----------------\n%!";
		let out_ch, in_ch = get_ch () in
		output_string out_ch ("listing.\n"); flush out_ch;

		let next_str = ref "" in
		  while not (ends_with (String.trim !next_str) "yes") do		  	
			next_str := get_line_gingerly in_ch out_ch "listing.\n";
			next_str := String.trim (Str.global_replace (Str.regexp "| \\?-") "" !next_str);
			next_str := String.trim (Str.global_replace (Str.regexp "\n\n") "\n" !next_str);
			Printf.printf "%s\n%!" !next_str;
		  done;
		  Printf.printf "-------------------------------------------------\n%!";
		  (* to turn on error printing, pass true. 
		     we flush the error stream every notification to prevent it from filling up,
		     which would cause XSB to block. *)
		  print_or_flush_errors false;;		  


	(* This takes in a string command (not query, this doesn't deal with the semicolons).
	It writes the command to xsb and returns the resulting text. *)
	let send_assert (str : string) : string =
	    if debug then Printf.printf "send_assert: %s\n%!" str;
		let out_ch, in_ch = get_ch () in

		(* Begin by clearing out any error state due to semicolons *)	
		flush_xsb_with_dummy out_ch in_ch;

		output_string out_ch (str ^ "\n");
		flush out_ch;		
		let answer = ref "" in
		let next_str = ref "" in		

		(* For debugging *)
        (*let next_str = ref "" in
		while not (ends_with !next_str "\n") do
		  next_str := (!next_str) ^ (String.make 1 (input_char in_ch));
		  Printf.printf "next_str=%s\n%!" !next_str;
		  if (ends_with !next_str "| ?- | ?-") then debug_print_errors_and_exit();
		done;*)


		while (not (ends_with (String.trim !next_str) "yes") && not (ends_with (String.trim !next_str) "no")) do		
        	(*next_str := input_line in_ch;*)
          (* get a char at a time, because errors won't send a newline *)
          next_str := get_line_gingerly in_ch out_ch str;		  
		  next_str := String.trim (Str.global_replace (Str.regexp "| \\?-") "" !next_str);

          if debug then Printf.printf "DEBUG: send_assert %s getting response. Line was: %s\n%!" str (!next_str);
   	      answer := (!answer ^ "\n" ^ String.trim !next_str);
		done;
		if debug then Printf.printf "send_assert answer: %s\n%!" (String.trim !answer); 
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

	(* Takes a string query (thing with semicolon answers), 
	 and the number of variables involved.
	 It writes the query to xsb and returns the results as a list of tuples. *)
	let send_query (str : string) (num_vars : int) : (string list) list =
	    if debug then Printf.printf "send_query: %s (#vars: %d)\n%!" str num_vars;
		let out_ch, in_ch = get_ch () in

		(* Begin by clearing out any error state due to semicolons *)
		flush_xsb_with_dummy out_ch in_ch;

		(* Send the query *)
		output_string out_ch (str ^ "\n");
		flush out_ch;

		if num_vars = 1 then
		begin
			if debug then Printf.printf "forcing initial semicolon.\n%!";
			output_string out_ch ";\n"; flush out_ch
		end;
		
		let answer = ref [] in
		let next_str = ref "" in        		
		let counter = ref 0 in
		while not (ends_with !next_str "no") do			

            next_str := get_line_gingerly in_ch out_ch str;		  		
			next_str := String.trim (Str.global_replace (Str.regexp "| \\?-") "" !next_str);

			if debug then Printf.printf "%d > '%s'\n%!" !counter !next_str;
			(* This function is tricky:
			    The last line won't be followed by a newline until we give it a ';'. 
				Since we don't know how many blocks total, we may send an extra ';'.
				This is dealt with by the get_line_gingerly function.
				
				We need to send a semicolon before asking for a line if num_vars = 1.
	  	    *)
			
			(* need to account for "X=3no" as well as "no" *)

			if (String.length !next_str > 0) then 
			begin					(* Time to be a coder, not a mathematician: =, not \cong *)
				if (!counter mod num_vars = ((num_vars - 2)) mod num_vars) 
				   && not (ends_with !next_str "no") 
				   && not (ends_with !next_str "yes") then
				begin
			  		if debug then Printf.printf "time for semicolon. at %d, of %d\n%!" !counter num_vars;
			  		output_string out_ch ";\n"; flush out_ch;
				end;			
				counter := !counter + 1;			

				(* If we have a value to save *)
				if !next_str <> "no" then 
					answer := (remove_from_end !next_str "no") :: !answer;			        	
			end;
			(* TODO If num_vars is wrong, this will freeze. Can we improve? *)
		done;
		if debug then Printf.printf "send_query finished. answers: [%s]\n%!" (String.concat ", " !answer);		
		List.map (fun (l : string list) -> List.map after_equals l) (group (List.rev !answer) num_vars);;

end


(* Provides functions for high level communication with XSB. *)
(* Right now ignoring queries. *)
module Communication = struct
	(* assertion, number of answers to expect (number of variables in clause) *)
	(* if this is a query with 0 variables, will call send_assert and thus need to provide [] vs [[]] *)
	(* VITAL: The CALLER must add the terminating period to message. *)
	let send_message (message : string) (num_ans : int) : (string list) list =
		if debug then (printf "send_message: %s (expected: %d)\n%!" message num_ans);
		if num_ans > 0 then
		    Xsb.send_query message num_ans 
	    else 
	        let yn = Xsb.send_assert message in
		        if (ends_with yn "yes") then [[]]
		        else []

	(* Perfectly valid to ask "r(X, 2)" here. *)
	let get_state (f: formula): (string list) list =
		send_message ((string_of_formula f)^".") (length (get_vars_and_fieldvars f));;

	let clause_to_xsb (cls: clause): string =
		(string_of_formula cls.head)^" :- "^(string_of_formula cls.body);;

	(* Substitute notif vars for their fields and produce a string for XSB to consume *)	
	let rec subs_xsb_formula (prgm: flowlog_program) (f: formula): formula = 
		match f with
		| FTrue -> FTrue
		| FFalse -> FFalse
		| FNot(innerf) -> FNot(subs_xsb_formula prgm innerf)
		| FAnd(f1, f2) -> FAnd(subs_xsb_formula prgm f1, subs_xsb_formula prgm f2)
		| FOr(f1, f2) -> FOr(subs_xsb_formula prgm f1, subs_xsb_formula prgm f2)
		| FEquals(_, _) -> f
		| FAtom(modname, relname, tlargs) -> 			
			let subsarglists = mapi (decls_expand_fields prgm modname relname) tlargs in
			let subargs = fold_left (fun acc lst -> acc @ lst) [] subsarglists in
			FAtom(modname, relname, subargs);;

		(* engine will sometimes be told to retract _, need to retract ALL matches, not just first match *)
	let retract_formula (tup: formula): unit = 
		ignore (send_message ("retractall("^(string_of_formula tup)^").") 0);;

	let assert_formula (tup: formula): unit = 
		(* avoid storing multiples of same tuple *)
		retract_formula tup; 
		ignore (send_message ("assert("^(string_of_formula tup)^").") 0);;

	let assert_event (p: flowlog_program) (notif: event): unit =	
			iter assert_formula (inc_event_to_formulas p notif);;

	let retract_event (p: flowlog_program) (notif: event): unit = 
			iter retract_formula (inc_event_to_formulas p notif);;

	let start_clause (prgm: flowlog_program) (cls : clause) : unit =
		(*if debug then print_endline ("start_clause: assert((" ^ (Type_Helpers.clause_to_string cls) ^ ")).");
		if debug then (List.iter (fun t -> (Printf.printf "var: %s\n%!" (Type_Helpers.term_to_string t))) (get_vars cls));*)
		let new_head = subs_xsb_formula prgm cls.head in
		let new_body = subs_xsb_formula prgm cls.body in
		printf "start_clause substituted: %s :- %s" (string_of_formula new_head)(string_of_formula new_body);
		let subs_cls = {head = new_head; 
		                body = new_body;
						orig_rule = cls.orig_rule}  in
			printf "subs cls: %s\n%!" (string_of_clause cls);
			ignore (send_message ("assert((" ^ (clause_to_xsb subs_cls) ^ ")).") 
				                 (length (get_all_clause_vars subs_cls)));
		();;
		

	(* assuming all implicitly defined clauses have been added to list of clauses *)
	let start_program (prgm : flowlog_program) (notables: bool): unit =
		printf "-------------------\nStarting Flowlog Program...\n%!";		
		(* prevent XSB from locking up if unknown relation seen. will assume false if unknown now.*)
		ignore (send_message "set_prolog_flag(unknown, fail)." 0);
		(* Add a clause if it's not fully compiled, OR we're in no-compilation mode *)
		List.iter (fun cl -> 
					if notables || (not (mem cl prgm.can_fully_compile_to_fwd_clauses)) then
					 (start_clause prgm cl)) prgm.clauses;
		Xsb.debug_print_listings();;

end