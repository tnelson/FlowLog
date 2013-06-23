(*#load "unix.cma";;*)
open Unix;;
open Printf;;

module Xsb = struct
	
	(* creates a pair channels for talking to xsb, starts xsb, and returns the channels *)
	let start_xsb () : out_channel * in_channel =
		let from_xsb, xsb_out = Unix.pipe() in
		let xsb_in, to_xsb = Unix.pipe() in
		(* Convert the file descriptors created to channels *)
		let xin_channel = in_channel_of_descr from_xsb in
		let xout_channel = out_channel_of_descr to_xsb in
		(* in text mode *)
		let _ = set_binary_mode_out xout_channel false in
		let _ = set_binary_mode_in xin_channel false in
		(* start xsb *)
		let error_1, error_2 = Unix.pipe() in
		let _ = Unix.create_process "xsb" [|"xsb"|] xsb_in xsb_out error_2 in
		(xout_channel, xin_channel);;

	(* error_2 needs to be periodically flushed! *)
		

(* True if string str1 ends with string str2 *)
	let ends_with (str1 : string) (str2 : string) : bool = 
		if String.length str2 > String.length str1
		then false
		else (String.sub str1 ((String.length str1) - (String.length str2)) (String.length str2)) = str2;;

	(* This takes in a string command (not query, this doesn't deal with the semicolons) and two channels (to and from xsb).
	It writes the command to xsb and returns the resulting text.*)
	let send_assert (str : string) (out_ch : out_channel) (in_ch : in_channel) : string =
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

	(* Takes a string query (thing with semicolon answers), the number of variables involved, and the in and out chanels.
	 It writes the query to xsb and returns a list of lists with all of the results. *)
	let send_query (str : string) (num_vars : int) (out_ch : out_channel) (in_ch : in_channel) : (string list) list =
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

	let halt_xsb (out_ch : out_channel) : unit = 
		output_string out_ch "halt.\n";
		flush out_ch;;

	let list_to_string (l : 'a list) (converter : 'a -> string) : string =
		match l with 
		| [] -> "[]";
		| _ -> let ans = List.fold_right (fun x st -> (converter x) ^ ", " ^ st) l "" in
		"[" ^ String.sub ans 0 (String.length ans - 2) ^ "]";;

	(* list of list of string to string conversion *)
	let rec lol_to_string (l : (string list) list) : string = 
		list_to_string l (fun li -> list_to_string li (fun x -> x))

end