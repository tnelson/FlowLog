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

(* XSB is shared state. We also have the remember_for_forwarding and packet_queue business *)
let xsbmutex = Mutex.create();;

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
	    ref_err_ch := None;
	    printf "HALTED XSB.\n%!";;

	(* Flush out the error buffer. *)
	let print_or_flush_errors (print_too: bool) : unit =
	  let errstr = ref "" in
	  try
	    while true do
            errstr := !errstr ^ (String.make 1 (input_char
               (match !ref_err_ch with
                   | Some(ch) -> ch;
                   | _ -> raise (End_of_file))));
	    done;
	  with | End_of_file -> if print_too && (String.length (String.trim !errstr)) > 0 then Printf.printf "[ERROR] EoF: '%s'%!" !errstr;
	       | Sys_blocked_io -> if print_too && (String.length (String.trim !errstr)) > 0 then Printf.printf "[ERROR] Blocked: '%s'\n%!" !errstr;;

    (* If the line ends with "error", print the stderr stream, terminate XSB, and halt execution. *)
	let rec get_line_gingerly (in_ch: in_channel) ~(printerror: bool): string =
		let next_str = input_line in_ch in

		if (ends_with next_str "error") then
		begin
		    if printerror then (* avoid infinite loop if debug_print_listings fails *)
		    begin
				printf "------------ XSB ERROR ------------\n%!";
				sleep(1); (* Give XSB time to print the error to stderr *)
				print_or_flush_errors true;
				debug_print_listings(); (* also flushes errors *)
				printf "-----------------------------------\n%!";
			end;
			halt_xsb();

			failwith "XSB returned an error."
		end;


            (*Printf.printf "glg: %s\n%!" !next_str;*)
		    (*while (ends_with !next_str "| ?- | ?-") do
		    	print_or_flush_errors (if debug then true else false);
		    	(* we may have asked an extra semicolon and caused a syntax error. *)
		    	if debug then Printf.printf "XSB Error. Asking again.\n%!";
		  		output_string out_ch (orig ^ "\n");
		    	flush out_ch;
		    	next_str := "";
		  	done*)

		next_str

	(* Prints the XSB listings currently asserted to stdout.
	   This function is useful for confirming that XSB knows what we think it knows. *)
	and debug_print_listings () : unit =
	    Printf.printf "---------------- PRINTING LISTINGS ----------------\n%!";
		let out_ch, in_ch = get_ch () in
		output_string out_ch ("listing.\n"); flush out_ch;

		let next_str = ref "" in
		  while not (ends_with (String.trim !next_str) "yes") do
			next_str := get_line_gingerly in_ch ~printerror:false;
			next_str := String.trim (Str.global_replace (Str.regexp "| \\?-") "" !next_str);
			next_str := String.trim (Str.global_replace (Str.regexp "\n\n") "\n" !next_str);
			Printf.printf "%s\n%!" !next_str;
		  done;
		  Printf.printf "-------------------------------------------------\n%!";
		  (* to turn on error printing, pass true.
		     we flush the error stream every notification to prevent it from filling up,
		     which would cause XSB to block. *)
		  print_or_flush_errors false;;

	let print_statistics(): unit =
	    Printf.printf "---------------- STATISTICS ----------------\n%!";
		let out_ch, in_ch = get_ch () in
		output_string out_ch ("statistics.\n"); flush out_ch;
		let next_str = ref "" in
		  while not (ends_with (String.trim !next_str) "yes") do
			next_str := get_line_gingerly in_ch ~printerror:false;
			Printf.printf "%s\n%!" !next_str;
		  done;;

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
          next_str := get_line_gingerly in_ch ~printerror:true;
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

	let group (alist : 'a list) (num : int) : ('a list) list =
		let rec group_helper (sublist: 'a list): ('a list) list =
		(*printf "group helper: on length %d\n%!" (length sublist);*)
			match sublist with
				| [] -> []
				| _ -> let (firstn, restlst) = split_nth num sublist in
					firstn :: (group_helper restlst) in
		let offset = (length alist) mod num in
		let header = (take offset alist) in
		let remainder = (drop offset alist) in
		let grouped_remainder = group_helper remainder in
			if offset > 0 then header :: grouped_remainder
			else grouped_remainder;;



(*printf "%s\n%!" (string_of_list_list "; " "," string_of_int (group [] 2));;
printf "%s\n%!" (string_of_list_list "; " "," string_of_int (group [1] 2));;
printf "%s\n%!" (string_of_list_list "; " "," string_of_int (group [1;2;3;4;5;6;7;8;9] 2));;
printf "%s\n%!" (string_of_list_list "; " "," string_of_int (group [1;2;3;4;5;6;7;8;9;10] 2));;
exit(1);;
*)

	(* groups elements of alist into lists of size num except possibly the first one *)
(*	let rec group (alist : 'a list) (num : int) : ('a list) list =
		match alist with
		| [] -> [];
		| f :: r -> match group r num with
			| [] -> [[f]];
			| f1 :: r1 -> if length f1 < num
						then (f :: f1) :: r1
						else [f] :: (f1 :: r1);;
*)

  let rec xsb_of_formula ?(mode:xsbmode = Xsb) ?(compiler: bool = false) (f: formula): string =
    match f with
      | FTrue -> "true"
      | FFalse -> "false"
      | FEquals(t1, t2) -> (xsb_of_term ~mode:mode t1) ^ " = "^ (xsb_of_term ~mode:mode t2)
      | FIn(t, addr, mask) ->
          sprintf "in_ipv4_range(%s,%s,%s)" (xsb_of_term ~mode:mode t) (xsb_of_term ~mode:mode addr) (xsb_of_term ~mode:mode  mask)
      | FNot(f) ->
        (match mode with
          | XsbForcePositive -> "not_"^(xsb_of_formula ~mode:mode f)
          | _ -> "(not "^(xsb_of_formula ~mode:mode f)^")")

      | FAtom("", relname, tlargs) when Flowlog_Builtins.is_built_in relname ->
        Flowlog_Builtins.xsb_for_built_in mode relname tlargs

      | FAtom("", relname, tlargs) ->
          relname^"("^(String.concat "," (map (xsb_of_term ~mode:mode) tlargs))^")"
      | FAtom(modname, relname, tlargs) ->
          modname^"/"^relname^"("^(String.concat "," (map (xsb_of_term ~mode:mode) tlargs))^")"
      | FAnd(f1, f2) -> (xsb_of_formula ~mode:mode f1) ^ ", "^ (xsb_of_formula ~mode:mode f2)
      | FOr(f1, f2) -> failwith "xsb_of_formula: unsupported formula type for XSB conversion:"^(string_of_formula f);;

	(* For variables not proceeded with underscore, XSB prints binding sets and requires a semicolon between them.
	   This is problematic since we have no way of knowing how many semicolons to enter. Instead, tell XSB
	   exactly how to print the results, with no semicolons between binding sets. *)
	let build_xsb_query_string ?(compiler: bool = false) (f: formula) : (int * string) =
		let allvars = (get_vars_and_fieldvars f) in
		(* anys need _ added here; the others get their _ added in xsb_of_formula*)
		let anystrs = map (xsb_of_term ~mode:XsbAddUnderscoreVars) (filter is_ANY_term allvars) in
		let varstrs = map xsb_of_term (filter (fun v -> not (is_ANY_term v)) allvars) in
		let varoutfrags = map (fun varstr -> sprintf "write('%s='), writeln(_%s)" varstr varstr) varstrs in
		let anystr = if (length anystrs) > 0 then (String.concat "^" anystrs)^"^" else "" in
		let fstr = (xsb_of_formula ~mode:XsbAddUnderscoreVars ~compiler:compiler f) in
		    (* printf "%s\n(%s, %s, fail).\n%!" (string_of_formula f) fstr (String.concat "," varoutfrags);*)
		    (* The setof construct wrapped around the formula prevents duplicate results *)
		    (* We wrap the fmla in parens in case it has more than one conjunct in it*)
		    (* any variables should be aggregated; hence the anystr with ^. See setof docs. *)
			let resultstr = (if (length varoutfrags) > 0 then
              sprintf "(setof(t, %s(%s), _), %s, fail)." anystr fstr (String.concat "," varoutfrags)
            else
              sprintf "%s." fstr) in
			(length varstrs, resultstr);;

	(* Takes a string query (thing with semicolon answers),
	 and the number of variables involved.
	 It writes the query to xsb and returns the results as a list of tuples. *)
	let send_query (str : string) (num_vars : int) : (string list) list =
	    if debug then Printf.printf "send_query: %s (#vars: %d)\n%!" str num_vars;

	    let answer = ref [] in
		let next_str = ref "" in
		(try
			let out_ch, in_ch = get_ch () in
			(* Send the query *)
			output_string out_ch (str ^ "\n");
			flush out_ch;

			while not (ends_with !next_str "no") && not (ends_with !next_str "yes") do
	            next_str := get_line_gingerly in_ch ~printerror:true;
				next_str := String.trim (Str.global_replace (Str.regexp "| \\?-") "" !next_str);

				(* may get a blank line. if so, ignore it. terminate only on "no" *)
				if (String.length !next_str > 0) && (!next_str <> "no") && (!next_str <> "yes") then
					answer := (remove_from_end (remove_from_end !next_str "no") "yes") :: !answer;
			done
		with exn ->	printf "send_query encountered an error. Terminating.\n%!";
		           	printf "%s\n%!" (Printexc.to_string exn);
		           	exit(100));

		if debug then Printf.printf "send_query finished. answers: [%s]\n\n%!" (String.concat ", " !answer);
		if !global_verbose >= 1 then count_send_query := !count_send_query + 1;
		(* separate the list into a list of lists *)
		if (length !answer) > 200000 then
		begin
			printf "\nXSB responded with %d conjunctions. Something has possibly gone wrong in query construction (or the database is too large).\nXSB string was: %s\n%!" (length !answer) str;
			exit(1);
		end;

		let grouped = (group (rev !answer) num_vars) in
			map (fun (l : string list) -> map after_equals l) grouped;;

end

(* Provides functions for high level communication with XSB. *)
module Communication = struct

  (* ASSUMED: We're dealing with one event at a time, and so each relation we populate gets only one tuple. *)
  (* raises Not_found if nothing to do for this event *)
  let inc_event_to_formulas (p: flowlog_program) (notif: event) (from: eventsource): formula list =
    (* event contains k=v mappings and a type. convert to a formula via defns in program*)
    (*printf "Converting event to formula: %s. Supertypes: %s\n%!" (string_of_event notif) (String.concat "%s\n%!" (built_in_supertypes notif.typeid));*)
    map (fun typename ->
    	  let relname = inc_event_to_relname p typename from in
          FAtom("", relname, map
          	         (fun fld -> try TConst(StringMap.find fld notif.values) with | Not_found -> failwith ("inc_event_to_formulas: "^fld))
                     (get_fields_for_type p typename)))
        (built_in_supertypes notif.typeid);;


  (* in this IO relation, at index idx, there should be something of type T. What are T's fields, in order? *)
  let get_io_fields_for_index (prgm: flowlog_program) (relname: string) (idx: int) (context_on: event_def option): (string list) option =
  	try
  		Some (get_fields_for_type prgm (input_rel_to_eventname prgm relname))
  	with
  	| Not_found
  	| UndeclaredIncomingRelation(_) ->
  	  	let out = get_outgoing prgm relname in
        		(match out.outarity with
        			| SameAsOnFields -> (match context_on with
        				| Some(e) -> Some (map (fun (n,_) -> n) e.evfields)
        				| None -> failwith "get_io_fields_for_index: no On context given")
        			| FixedEvent(evname) -> Some(get_fields_for_type prgm evname) (* (nth flds idx)*)
        		    | AnyFields -> failwith "get_io_fields_for_index: unsupported AnyFields")
        | _ -> failwith ("get_io_fields_for_index: neither input nor output relation: "^relname);;

   (* in modname.relname, the ith element has which fields? *)
  let decls_expand_fields (prgm: flowlog_program) (modname: string) (relname: string) (context_on: event_def option) (i: int) (t: term): term list =
    match t with
      | TVar(vname) when is_io_rel prgm relname ->
      	(match (get_io_fields_for_index prgm relname i context_on) with
      		| Some fieldlist -> map (fun fldname -> TField(vname, fldname)) fieldlist
      		| None -> [t])
      | _ -> [t];;

	(* assertion, number of answers to expect (number of variables in clause) *)
	(* if this is a query with 0 variables, will call send_assert and thus need to provide [] vs [[]] *)
	(* VITAL: The CALLER must add the terminating period to message. *)
	let send_message (message : string) (num_ans : int) : (term list) list =
	    (* Begin by flushing the error buffer.
	       XSB will send newlines in stderr for (seemingly) no reason...*)
		Xsb.print_or_flush_errors true;
		if !global_verbose >= 8 then (printf "send_message: %s (expected: %d)\n%!" message num_ans);
		if num_ans > 0 then
		    let strresults = Xsb.send_query message num_ans in
		    let tupresults = map (fun tuplestr -> map reassemble_xsb_term tuplestr) strresults in
		      if !global_verbose >= 10 then printf "term results: %s\n%!" (String.concat " " (map (fun l -> "["^(String.concat "," (map string_of_term l))^"]") tupresults));
		      tupresults
	    else
	        let yn = Xsb.send_assert message in
		        if (ends_with yn "yes") then [[]]
		        else []

	(* Perfectly valid to ask "r(X, 2)" here.
	   Note that ANYs will be auto-removed! *)
	let get_state ?(compiler: bool = false) (f: formula): (term list) list =
		(*send_message ((string_of_formula ~verbose:Xsb f)^".") (length (get_vars_and_fieldvars f));;		*)
		let num_non_any_vars, qstr = Xsb.build_xsb_query_string ~compiler:compiler f in
		send_message qstr num_non_any_vars ;;

	(* Extract the entire local controller state from XSB. This lets us do pretty-printing, rather
	   than just using XSB's "listing." command, among other things.
	   Table declaration ---> formulas in that table (use flatten find_all to extract all of them) *)
	let get_full_state_for_program (p: flowlog_program): (table_def, formula list) Hashtbl.t =
	  let get_state_helper (tname: string) (nargs: int) =
        get_state (FAtom("", tname, init nargs (fun i -> TVar("X"^(string_of_int i))))) in

      let statehash = (Hashtbl.create 5) in
      let add_to_hash_for_table (tdef: table_def) : unit =
            (* this produces a list of formulas. which get added on top of prior lists. hence flatten find_all *)
            Hashtbl.add statehash tdef (map (fun args -> FAtom("", tdef.tablename, args))
                                            (get_state_helper tdef.tablename (length tdef.tablearity))) in

		iter add_to_hash_for_table p.tables;
		statehash;;

  (**************)

  let pretty_print_fact (tdecl: table_def) (f: formula): string =
    (*printf "pretty print fact: %s %s\n%!" (string_of_formula f) (string_of_list "," identity tdecl.tablearity);*)
    match f with
      | FAtom(_, rname, rargs) when (length rargs) = (length tdecl.tablearity) ->
        sprintf "%s(%s)." rname (String.concat ", " (map2 pretty_print_term tdecl.tablearity rargs))
      | _ -> failwith ("pretty_print_fact: "^(string_of_formula f));;

  let pretty_print_formula (p: flowlog_program) (f: formula): string =
    (*printf "pretty print formula: %s\n%!" (string_of_formula f);*)
    try
   	(match f with
   	  | FAtom(_, rname, rargs) when is_table p rname ->
     	let tbl = get_table p rname in
     	  let result = pretty_print_fact tbl f in
     	    result
      | _ -> failwith ("pretty_print_formula: "^(string_of_formula f)))
    with | _ -> failwith "pretty_print_formula failed";;

  let get_and_print_xsb_state (p: flowlog_program): unit =
    let currstate = get_full_state_for_program p in
    let get_tblstrs (tbl: table_def) : string =
        let fmlasfortbl = flatten (Hashtbl.find_all currstate tbl) in
          sprintf "%s (%d tuples):\n%s" tbl.tablename (length fmlasfortbl) (String.concat "\n" (map (pretty_print_fact tbl) fmlasfortbl)) in

    printf "-------\n|STATE|\n-------\n%s\n%!" (String.concat "\n" (map get_tblstrs p.tables));;
    (**************)

	let clause_to_xsb ?(forcepositive = false) (cls: clause): string =
	  let fpflag = if forcepositive then XsbForcePositive else XsbAddUnderscoreVars in
		(Xsb.xsb_of_formula ~mode:fpflag cls.head)^" :- "^(Xsb.xsb_of_formula ~mode:fpflag cls.body);;

	(* Substitute notif vars for their fields and produce a string for XSB to consume *)
	(* We need to have a (possible) ON context for dealing with the forward relation *)
	let rec subs_xsb_formula (prgm: flowlog_program) ?(context_on: event_def option = None) (f: formula): formula =
		match f with
		| FTrue -> FTrue
		| FFalse -> FFalse
		| FNot(innerf) -> FNot(subs_xsb_formula prgm ~context_on:context_on innerf)
		| FAnd(f1, f2) -> FAnd(subs_xsb_formula prgm ~context_on:context_on f1, subs_xsb_formula prgm ~context_on:context_on f2)
		| FOr(f1, f2) -> FOr(subs_xsb_formula prgm ~context_on:context_on f1, subs_xsb_formula prgm ~context_on:context_on f2)
		| FEquals(_, _) -> f
		| FIn(_,_,_) -> f
		| FAtom(modname, relname, tlargs) ->
			let subsarglists = mapi (decls_expand_fields prgm modname relname context_on) tlargs in
			let subargs = fold_left (fun acc lst -> acc @ lst) [] subsarglists in
			FAtom(modname, relname, subargs);;

		(* engine will sometimes be told to retract _, need to retract ALL matches, not just first match *)
	let retract_formula (tup: formula): unit =
	    if !global_verbose >= 1 then count_retract_formula := !count_retract_formula + 1;
		ignore (send_message ("retractall("^(Xsb.xsb_of_formula tup)^").") 0);;

	let assert_formula (tup: formula): unit =
		(* with trie indexing, we no longer need to worry about storing multiples of the same tuple *)
		if !global_verbose >= 1 then count_assert_formula := !count_assert_formula + 1;
		ignore (send_message ("assert("^(Xsb.xsb_of_formula tup)^").") 0);;

	let assert_event_and_subevents (p: flowlog_program) (notif: event) (from: eventsource): unit =
			iter assert_formula (inc_event_to_formulas p notif from);;

	let retract_event_and_subevents (p: flowlog_program) (notif: event) (from: eventsource): unit =
			iter retract_formula (inc_event_to_formulas p notif from);;

	let get_on_context (p: flowlog_program) (c: clause): event_def option =
	  let trigger_relname = c.orig_rule.onrel in
	  	Some (get_event p (input_rel_to_eventname p trigger_relname));;

	let start_clause (prgm: flowlog_program) ?(forcepositive = false) (cls : clause) : unit =
		(*if debug then print_endline ("start_clause: assert((" ^ (Type_Helpers.clause_to_string cls) ^ ")).");
		if debug then (iter (fun t -> (Printf.printf "var: %s\n%!" (Type_Helpers.term_to_string t))) (get_vars cls));*)
		let on_context = get_on_context prgm cls in
		let new_head = subs_xsb_formula prgm ~context_on:on_context cls.head in
		let subs_body = subs_xsb_formula prgm ~context_on:on_context cls.body in

		(* Every forward clause needs XSB to enforce port domain.
		   This is done here, rather than in desugaring, so that the compiler
		   can just convert oldport != newport to an ALL action. *)
		let new_body = if is_forward_clause cls then
					   begin
					   	 let (_, old, _) = trim_packet_from_body cls.body in
					   	 let newp = string_of_term (hd (get_head_vars cls)) in
					   	 (* positive, binding atom needs to come BEFORE potential negated atom *)
		                 FAnd(FAtom("", switch_has_port_relname, [TVar((String.uppercase old)^"__LOCSW");
		                 	                                      TVar((String.uppercase newp)^"__LOCPT")]),
		                      subs_body)
		               end
		               else
		                 subs_body in
		if !global_verbose > 4 then
			printf "start_clause substituted: %s :- %s" (Xsb.xsb_of_formula new_head) (Xsb.xsb_of_formula new_body);

		let subs_cls = {head = new_head;
		                body = new_body;
						orig_rule = cls.orig_rule}  in
			if !global_verbose > 4 then
				printf "subs cls: %s\n%!" (string_of_clause cls);
			ignore (send_message ("assert((" ^ (clause_to_xsb ~forcepositive:forcepositive subs_cls) ^ ")).")
				                 (length (get_all_clause_vars subs_cls)));;

	(* assuming all implicitly defined clauses have been added to list of clauses *)
	let start_program (prgm : flowlog_program) ?(forcepositive = false) (notables: bool) (additional: string list): unit =
		printf "-------------------\nStarting Flowlog Program...\n%!";

		(* prevent XSB from locking up if unknown relation seen. will assume false if unknown now.*)
		ignore (send_message "set_prolog_flag(unknown, fail)." 0);

		(* Require =SET= semantics, not bag semantics. Without these commands, we'd need to
		   retract before every assert to prevent fact bloat over time. *)
		iter (fun tdef -> ignore (send_message (sprintf "index(%s/%d,trie)." tdef.tablename (length tdef.tablearity)) 0))
		  ((get_local_tables prgm) @ (get_remote_tables prgm));

		(* Import function that lets us write to the error stream separately. *)
		ignore (send_message "import error_writeln/1 from standard." 0);

		(* Any uncaught XSB error will now force "error\n" to be printed to stdout, then send the error to stderr.
	       This allows us to catch the "error" line when reading from XSB.
	       Note the use of _X instead of X, to prevent XSB from printing a "value" for X here. *)
	    ignore (send_message "assert((default_user_error_handler(_X) :- writeln('error'), error_writeln(_X)))." 0);

	    (*ignore (send_message "jdnfsjdnf(,)." 0);*)

        (* FOR IP ADDRESS MASKING: Is Target in the range Addr/Mask? (Lshift+and to mask out host address portion)
           Remember to flip the mask (i.e., shift by 32-Mask). The mask covers bits we want to _keep_.
           Since XSB may run in either 64 or 32 bit mode, bitwise-and the addr and target with 32-ones *)
	    ignore (send_message "assert((in_ipv4_range(_Target, _Addr, _Mask) :- _X is ((2^32-1) << (32-_Mask)) /\\ (_Target /\\ (2^32-1)), _X is ((2^32-1) << (32-_Mask)) /\\ (_Addr /\\ (2^32-1))))." 0);


	    (* For every built-in predicate, there may be some helper rules to send: *)
	    iter (fun (str) -> ignore (send_message str 0))
	      additional;

		(* remember that if the string contains >1 ., the error may come in the next read *)

		(* If unsafe and compiling to tables, add only non-fully compiled clauses to prevent overlap *)
		if notables || not !global_unsafe then
		  iter (start_clause prgm ~forcepositive:forcepositive) prgm.clauses
	    else
		  iter (start_clause prgm ~forcepositive:forcepositive) prgm.not_fully_compiled_clauses;

		Xsb.debug_print_listings();;

end
