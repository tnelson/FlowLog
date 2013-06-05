#load "unix.cma";;
open Unix;;
open Printf;;


let from_xsb, xsb_out = Unix.pipe();;
let xsb_in, to_xsb = Unix.pipe();; 

(* Create a pipe for the child 
"The first component of the result is opened for reading, that's the exit to the pipe. 
The second component is opened for writing, that's the entrance to the pipe."
*)

(* Convert the file descriptors created to channels *)
let xin_channel = in_channel_of_descr from_xsb;;
(*print_endline "Child's stdout channel created. Becomes an in channel.";;*)
let xout_channel = out_channel_of_descr to_xsb;;  
(*print_endline "Child's stdin channel created. Becomes an out channel.";;*)

(* in text mode *)
set_binary_mode_out xout_channel false;;
set_binary_mode_in xin_channel false;;

Unix.create_process "/Applications/XSB/bin/xsb" [|"xsb"|] xsb_in xsb_out Unix.stderr;;

(*print_endline "Process created.";;*)

(* This takes in a string command (not query, this doesn't deal with the semicolons) and two channels (to and from xsb). It writes the command to xsb and returns the resulting text.*)
let send_assert str out_ch in_ch =
	output_string out_ch (str ^ "\n");
	flush out_ch;
	let answer = ref "" in
	let next_str = ref "" in
	while (!next_str <> "yes" && !next_str <> "no") do
		next_str := input_line in_ch;
		answer := (!answer ^ "\n" ^ !next_str);
	done;
	String.trim !answer;;

send_assert "assert(p(1))." xout_channel xin_channel;;

send_assert "assert(p(2))." xout_channel xin_channel;;

(* True if string str1 ends with string str2 *)
let ends_with str1 str2 = 
	if String.length str2 > String.length str1
	then false
	else String.sub str1 ((String.length str1) - (String.length str2)) (String.length str2) = str2;;

(* Removes str2 from the end of str1 if its there, otherwise returns str1 *)
let remove_from_end str1 str2 = 
	if ends_with str1 str2
	then String.sub str1 0 ((String.length str1) - (String.length str2))
	else str1;; 

let rec group alist num = 
	match alist with
	| [] -> [];
	| f :: r -> match group r num with
		| [] -> [[f]];
		| f1 :: r1 -> if List.length f1 < num
					then (f :: f1) :: r1
					else [f] :: (f1 :: r1);;

(* Takes a string query (thing with semicolon answers), the number of variables involved, and the in and out chanels.
 It writes the query to xsb and returns a list of lists with all of the results. *)
let send_query str num_vars out_ch in_ch =
	output_string out_ch (str ^ "\n");
	flush out_ch;
	let _ = input_line in_ch in
	let answer = ref [] in
	let next_str = ref "" in
	let counter = ref 0 in
	while not (ends_with !next_str "no") do
		if (!counter mod num_vars = 0) then
		(output_string out_ch ";\n";
		flush out_ch);		
		counter := !counter + 1;
		next_str := input_line in_ch;
		answer := (remove_from_end !next_str "no") :: !answer;
	done;
	group (List.rev !answer) num_vars;;

List.iter (List.iter (printf "%s ")) (send_query "p(X)." 1 xout_channel xin_channel);;
flush Pervasives.stdout;;

send_assert "assert(q(1,2))." xout_channel xin_channel;;

List.iter (List.iter (printf "%s ")) (send_query "q(X, Y)." 2 xout_channel xin_channel);;
flush Pervasives.stdout;;

send_assert "[mac_learning]." xout_channel xin_channel;;

send_assert "assert(learned(1,5,4))." xout_channel xin_channel;;


(*print_string "ready for the big query";;
flush Pervasives.stdout;;*)

List.iter (List.iter (printf "%s ")) (send_query "emit(1,2,3,4,5,6,7,8, LocSw2, LocPt2, DlSrc2, DlDst2, DlTyp2, NwSrc2, NwDst2, NwProto2)." 8 xout_channel xin_channel);;
flush Pervasives.stdout;;



(*

			if !next_str == "" then
			output_string out_ch ";\n";
			flush out_ch;
output_string xout_channel "assert(p(123)).\n";;
flush xout_channel;;

(* prompt? *)
print_endline (input_line xin_channel);;
flush Pervasives.stdout;
(* yes *)
print_endline (input_line xin_channel);;
flush Pervasives.stdout;



(* can we do it again? *)
output_string xout_channel "assert(p(456)).\n";;
flush xout_channel;;

(* prompt? *)
print_endline (input_line xin_channel);;
flush Pervasives.stdout;
(* yes *)
print_endline (input_line xin_channel);;
flush Pervasives.stdout;

(* can we do it again? *)
output_string xout_channel "writeln(hi).\n";;
flush xout_channel;;

(* prompt? *)
(*print_endline (input_line xin_channel);;
flush Pervasives.stdout;
(* yes *)
print_endline (input_line xin_channel);;
flush Pervasives.stdout;

(*output_string xout_channel "p(X), writeln(X).\n";;*)
output_string xout_channel "p(X).\n";;
flush xout_channel;;

(* need to do something different in the end. it's waiting for a semicolon
   because the bindings hold at the end of X=...  
   input_line will block forever in this case. *)

(*try print_endline (input_line xin_channel) with End_of_file -> ();;*)

print_endline (input_line xin_channel);;
flush Pervasives.stdout;

output_string xout_channel ";\n";;
flush xout_channel;;

print_endline (input_line xin_channel);;
flush Pervasives.stdout;

output_string xout_channel ";\n";;
flush xout_channel;;

print_endline (input_line xin_channel);;
flush Pervasives.stdout;

print_endline (input_line xin_channel);;
flush Pervasives.stdout;



(* prompt? *)
(*print_endline (input_line xin_channel);;
flush Pervasives.stdout;
(* yes *)
print_endline (input_line xin_channel);;
flush Pervasives.stdout;
print_endline (input_line xin_channel);;
flush Pervasives.stdout;
print_endline (input_line xin_channel);;
flush Pervasives.stdout;*)
*)
*)
output_string xout_channel "halt.\n";;
flush xout_channel;;
