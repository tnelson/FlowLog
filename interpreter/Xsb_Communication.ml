(*****************************************************************)
(* Interface with XSB. Invoked by Partial_Eval etc.              *)
(* We access XSB textually via pipes. Note the special functions *)
(* to properly handle XSB's output.                              *)
(*****************************************************************)

open Unix
open Printf
open Flowlog_Types
open Flowlog_Helpers
open Flowlog_Packets
open Flowlog_Thrift_Out
open Str
open ExtList.List

(* Enable insanely verbose debugging info *)
let debug = false;;

let count_assert_formula = ref 0;;
let count_retract_formula = ref 0;;
let count_send_assert = ref 0;;
let count_send_query = ref 0;;

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
		flush out_ch;
		(* to enable restart *)
	    ref_out_ch := None;
	    ref_in_ch := None;
	    ref_err_ch := None;;

	
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
		if !global_verbose >= 1 then count_send_assert := !count_send_assert + 1;
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
    
	(* For variables not proceeded with underscore, XSB prints binding sets and requires a semicolon between them.
	   This is problematic since we have no way of knowing how many semicolons to enter. Instead, tell XSB
	   exactly how to print the results, with no semicolons between binding sets. *)
	let build_xsb_query_string (f: formula) : string = 
		let varstrs = map (string_of_term ~verbose:Xsb) (get_vars_and_fieldvars f) in
		let varoutfrags = map (fun varstr -> sprintf "write('%s='), writeln(_%s)" varstr varstr) varstrs in 
		let fstr = (string_of_formula ~verbose:XsbAddUnderscoreVars f) in		
		    (* printf "%s\n(%s, %s, fail).\n%!" (string_of_formula f) fstr (String.concat "," varoutfrags);*)
			sprintf "(%s, %s, fail)." fstr (String.concat "," varoutfrags);;

	(* Takes a string query (thing with semicolon answers), 
	 and the number of variables involved.
	 It writes the query to xsb and returns the results as a list of tuples. *)
	let send_query (str : string) (num_vars : int) : (string list) list =
	    if debug then Printf.printf "send_query: %s (#vars: %d)\n%!" str num_vars;
		let out_ch, in_ch = get_ch () in

		(* Send the query *)
		output_string out_ch (str ^ "\n");
		flush out_ch;
		
		let answer = ref [] in
		let next_str = ref "" in        				
		while not (ends_with !next_str "no") && not (ends_with !next_str "yes") do			
            next_str := get_line_gingerly in_ch out_ch str;		  		
			next_str := String.trim (Str.global_replace (Str.regexp "| \\?-") "" !next_str);
						
			(* may get a blank line. if so, ignore it. terminate only on "no" *)
			if (String.length !next_str > 0) && !next_str <> "no" then 			
				answer := (remove_from_end !next_str "no") :: !answer				
		done;

		if debug then Printf.printf "send_query finished. answers: [%s]\n%!" (String.concat ", " !answer);	
		if !global_verbose >= 1 then count_send_query := !count_send_query + 1;	
		(* separate the list into a list of lists *)
		List.map (fun (l : string list) -> List.map after_equals l) (group (List.rev !answer) num_vars);;

end

(* Provides functions for high level communication with XSB. *)
(* Right now ignoring queries. *)
module Communication = struct
  (* ASSUMED: We're dealing with one event at a time, and so each relation we populate gets only one tuple. *)
  (* raises Not_found if nothing to do for this event *)
  let inc_event_to_formulas (p: flowlog_program) (notif: event): formula list =
    (* event contains k=v mappings and a type. convert to a formula via defns in program*)
    (*printf "Converting event to formula: %s\n%!" (string_of_event notif);*)
    filter_map (function       
        | ReactInc(typename, relname) when mem typename (built_in_supertypes notif.typeid) ->
          Some(FAtom("", relname,                      
                     map (fun fld -> try TConst(StringMap.find fld notif.values) with | Not_found -> failwith ("inc_event_to_formulas: "^fld))
                     (get_fields_for_type p typename)))
        | _ -> None ) p.reacts;;


  (* in this IO relation, at index idx, there should be something of type T. What are T's fields, in order? *)
  let get_io_fields_for_index (prgm: flowlog_program) (relname: string) (idx: int): (string list) option =
    let decl = find (function       
        | DeclInc(rname, argtype) when rname = relname -> true 
        | DeclOut(rname, argtypelst) when rname = relname -> true
        | _ -> false) prgm.decls in 
      match decl with 
        | DeclInc(rname, argtype) when rname = relname -> 
          Some (get_fields_for_type prgm argtype)
        | DeclOut(rname, argtypelst) when rname = relname ->
          (* treat condensed output rels (forward, emit, ...) differently *)
        	if mem relname built_in_condensed_outrels then
        		Some(get_fields_for_type prgm (nth argtypelst idx))
        	else
        		None
          (*get_fields_for_type prgm (nth argtypelst idx)*)
        | _ -> failwith "get_io_fields_for_index";;

   (* in modname.relname, the ith element has which fields? *)
  let decls_expand_fields (prgm: flowlog_program) (modname: string) (relname: string) (i: int) (t: term): term list =  	
    match t with 
      | TVar(vname) when is_io_rel prgm modname relname -> 
      	(match (get_io_fields_for_index prgm relname i) with
      		| Some fieldlist -> map (fun fldname -> TField(vname, fldname)) fieldlist
      		| None -> [t])
      | _ -> [t];;

	(* assertion, number of answers to expect (number of variables in clause) *)
	(* if this is a query with 0 variables, will call send_assert and thus need to provide [] vs [[]] *)
	(* VITAL: The CALLER must add the terminating period to message. *)
	let send_message (message : string) (num_ans : int) : (term list) list =
		if debug then (printf "send_message: %s (expected: %d)\n%!" message num_ans);
		if num_ans > 0 then
		    let strresults = Xsb.send_query message num_ans in
		      map (fun tuplestr -> map reassemble_xsb_term tuplestr) strresults
	    else 
	        let yn = Xsb.send_assert message in
		        if (ends_with yn "yes") then [[]]
		        else []

	(* Perfectly valid to ask "r(X, 2)" here. *)	
	let get_state (f: formula): (term list) list =
		(*send_message ((string_of_formula ~verbose:Xsb f)^".") (length (get_vars_and_fieldvars f));;		*)
		send_message (Xsb.build_xsb_query_string f) (length (get_vars_and_fieldvars f));;		

	(* Extract the entire local controller state from XSB. This lets us do pretty-printing, rather
	   than just using XSB's "listing." command, among other things.
	   Table declaration ---> formulas in that table (use flatten find_all to extract all of them) *)
	let get_full_state_for_program (p: flowlog_program): (sdecl, formula list) Hashtbl.t = 
	  let get_state_helper (tname: string) (nargs: int) = 
        get_state (FAtom("", tname, init nargs (fun i -> TVar("X"^(string_of_int i))))) in
      
      let statehash = (Hashtbl.create 5) in
      let add_to_hash_for_table (tdecl: sdecl) : unit = 
        match tdecl with
          | DeclTable(tname, ttypes) -> 
            (* this produces a list of formulas. which get added on top of prior lists. hence flatten find_all *)
            Hashtbl.add statehash tdecl (map (fun args -> FAtom("", tname, args)) (get_state_helper tname (length ttypes))) 
          | _ -> failwith "add_to_hash_for_table" in
		
		iter add_to_hash_for_table (get_local_tables p);
		statehash;;

  (**************)
  (* improve this when we have more than strings running around *)
    let pretty_print_constant (typename: string) (c: term): string =    
      let strval = (match c with | TConst(s) -> s | _ -> failwith ("pretty_print_constant: non constant")) in
      match typename with
      | "ipaddr" -> Packet.string_of_ip (Int32.of_string strval)
      | "macaddr" -> Packet.string_of_mac (Int64.of_string strval) 
      | "portid" -> strval
      | "switchid" -> strval
      | _ -> strval;;

  let pretty_print_fact (tdecl: sdecl) (f: formula): string =
    match tdecl, f with
      | DeclTable(tname, ttypes), FAtom(_, rname, rargs) when (length rargs) = (length ttypes) -> 
        sprintf "%s(%s)." rname (String.concat ", " (map2 pretty_print_constant ttypes rargs))
      | _ -> failwith "pretty_print_fact";;
  let get_and_print_xsb_state (p: flowlog_program): unit = 
    let currstate = get_full_state_for_program p in       
    let get_tblstrs (tbl: sdecl) : string = 
      match tbl with 
      | DeclTable(tname, ttypes) -> 
        let fmlasfortbl = flatten (Hashtbl.find_all currstate tbl) in
          sprintf "%s:\n%s" tname (String.concat "\n" (map (pretty_print_fact tbl) fmlasfortbl))
      | _ -> failwith "get_and_print_state" in 
    printf "-------\n|STATE|\n-------\n%s\n%!" (String.concat "\n" (map get_tblstrs (get_local_tables p)));;
    (**************)

	let clause_to_xsb ?(forcepositive = false) (cls: clause): string =	  
	  let fpflag = if forcepositive then XsbForcePositive else XsbAddUnderscoreVars in
		(string_of_formula ~verbose:fpflag cls.head)^" :- "^(string_of_formula ~verbose:fpflag cls.body);;

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
	    if !global_verbose >= 1 then count_retract_formula := !count_retract_formula + 1;
		ignore (send_message ("retractall("^(string_of_formula ~verbose:Xsb tup)^").") 0);;

	let assert_formula (tup: formula): unit = 
		(* with trie indexing, we no longer need to worry about storing multiples of the same tuple *)
		if !global_verbose >= 1 then count_assert_formula := !count_assert_formula + 1;
		ignore (send_message ("assert("^(string_of_formula ~verbose:Xsb tup)^").") 0);;

	let assert_event_and_subevents (p: flowlog_program) (notif: event): unit =	
			iter assert_formula (inc_event_to_formulas p notif);;

	let retract_event_and_subevents (p: flowlog_program) (notif: event): unit = 
			iter retract_formula (inc_event_to_formulas p notif);;

	let start_clause (prgm: flowlog_program) ?(forcepositive = false) (cls : clause) : unit =
		(*if debug then print_endline ("start_clause: assert((" ^ (Type_Helpers.clause_to_string cls) ^ ")).");
		if debug then (List.iter (fun t -> (Printf.printf "var: %s\n%!" (Type_Helpers.term_to_string t))) (get_vars cls));*)
		let new_head = subs_xsb_formula prgm cls.head in
		let new_body = subs_xsb_formula prgm cls.body in		
		printf "start_clause substituted: %s :- %s" (string_of_formula ~verbose:Xsb new_head) (string_of_formula ~verbose:Xsb new_body);
		let subs_cls = {head = new_head; 
		                body = new_body;
						orig_rule = cls.orig_rule}  in
			printf "subs cls: %s\n%!" (string_of_clause cls);
			ignore (send_message ("assert((" ^ (clause_to_xsb ~forcepositive:forcepositive subs_cls) ^ ")).") 
				                 (length (get_all_clause_vars subs_cls)));;
	
	(* assuming all implicitly defined clauses have been added to list of clauses *)
	let start_program (prgm : flowlog_program) ?(forcepositive = false) (notables: bool): unit =
		printf "-------------------\nStarting Flowlog Program...\n%!";		
		(* prevent XSB from locking up if unknown relation seen. will assume false if unknown now.*)
		ignore (send_message "set_prolog_flag(unknown, fail)." 0);
	
		(* Require =SET= semantics, not bag semantics. Without these commands, we'd need to
		   retract before every assert to prevent fact bloat over time. *)
		iter (fun (tname, tarity) -> ignore (send_message (sprintf "index(%s/%d,trie)." tname tarity) 0)) (get_all_tables_name_and_arity prgm);

		(* Add a clause if it's not fully compiled, OR we're in no-compilation mode *)				
		if notables then 
		  List.iter (start_clause prgm ~forcepositive:forcepositive) prgm.clauses
	    else 
		  List.iter (start_clause prgm ~forcepositive:forcepositive) prgm.not_fully_compiled_clauses;

		Xsb.debug_print_listings();;

end