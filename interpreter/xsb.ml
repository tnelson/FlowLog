#load "unix.cma";;
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
		let _ = Unix.create_process "/Applications/XSB/bin/xsb" [|"xsb"|] xsb_in xsb_out error_2 in
		(xout_channel, xin_channel);;
		

	(* This takes in a string command (not query, this doesn't deal with the semicolons) and two channels (to and from xsb).
	It writes the command to xsb and returns the resulting text.*)
	let send_assert (str : string) (out_ch : out_channel) (in_ch : in_channel) : string =
		output_string out_ch (str ^ "\n");
		flush out_ch;
		let answer = ref "" in
		let next_str = ref "" in
		while (String.trim !next_str <> "yes" && String.trim !next_str <> "no") do
			next_str := input_line in_ch;
			answer := (!answer ^ "\n" ^ String.trim !next_str);
		done;
		String.trim !answer;;


	(* True if string str1 ends with string str2 *)
	let ends_with (str1 : string) (str2 : string) : bool = 
		if String.length str2 > String.length str1
		then false
		else String.sub str1 ((String.length str1) - (String.length str2)) (String.length str2) = str2;;

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

	(* Takes a string query (thing with semicolon answers), the number of variables involved, and the in and out chanels.
	 It writes the query to xsb and returns a list of lists with all of the results. *)
	let send_query (str : string) (num_vars : int) (out_ch : out_channel) (in_ch : in_channel) : (string list) list =
		output_string out_ch (str ^ "\n");
		flush out_ch;
		let _ = input_line in_ch in
		let answer = ref [] in
		let next_str = ref "" in
		let counter = ref 0 in
		while not (ends_with (String.trim !next_str) "no") do
			if (!counter mod num_vars = 0) then
			(output_string out_ch ";\n";
			flush out_ch);		
			counter := !counter + 1;
			next_str := input_line in_ch;
			answer := (remove_from_end (String.trim!next_str) "no") :: !answer;
		done;
		group (List.rev !answer) num_vars;;

	let halt_xsb (out_ch : out_channel) : unit = 
		output_string out_ch "halt.\n";
		flush out_ch;;

end

(* examples *)
open Xsb;;
let xout_channel, xin_channel = start_xsb ();;

send_assert "assert(p(1))." xout_channel xin_channel;;

send_assert "assert(p(2))." xout_channel xin_channel;;

List.iter (List.iter (printf "%s ")) (send_query "p(X)." 1 xout_channel xin_channel);;
flush Pervasives.stdout;;

send_assert "assert(q(1,2))." xout_channel xin_channel;;

List.iter (List.iter (printf "%s ")) (send_query "q(X, Y)." 2 xout_channel xin_channel);;
flush Pervasives.stdout;;

send_assert "[mac_learning]." xout_channel xin_channel;;

send_assert "assert(learned(1,5,4))." xout_channel xin_channel;;

List.iter (List.iter (printf "%s ")) (send_query "emit(1,2,3,4,5,6,7,8, LocSw2, LocPt2, DlSrc2, DlDst2, DlTyp2, NwSrc2, NwDst2, NwProto2)." 8 xout_channel xin_channel);;
flush Pervasives.stdout;;


(* always close the channel at the end *)
halt_xsb xout_channel
