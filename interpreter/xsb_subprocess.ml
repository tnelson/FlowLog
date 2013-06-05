#load "unix.cma";;
open Unix;;


let from_xsb, xsb_out = Unix.pipe();;
let xsb_in, to_xsb = Unix.pipe();; 

(* Create a pipe for the child 
"The first component of the result is opened for reading, that's the exit to the pipe. 
The second component is opened for writing, that's the entrance to the pipe."
*)

(* Convert the file descriptors created to channels *)
let xin_channel = in_channel_of_descr from_xsb;;
print_endline "Child's stdout channel created. Becomes an in channel.";;
let xout_channel = out_channel_of_descr to_xsb;;  
print_endline "Child's stdin channel created. Becomes an out channel.";;

(* in text mode *)
set_binary_mode_out xout_channel false;;
set_binary_mode_in xin_channel false;;

(* Use the pipe. Can keep stderr the same so XSB errors print in interpreter.
First: new stdin (OUT TO X)
Second: new stdout (IN FROM X)
 *)

Unix.create_process "/Applications/XSB/bin/xsb" [|"xsb"|] xsb_in xsb_out Unix.stderr;;
Unix.sleep 1;;

print_endline "Process created.";;

(*let send_query str out_ch in_ch =
	output_string out_ch (str ^ "\n");
	flush out_ch;
	let answer = ref "" in
	let next_str = ref "" in
	while (!next_str <> "yes" && !next_str <> "no") do
		(*print_endline ("next_str " ^ !next_str);*)
		next_str := input_line in_ch;
		answer := (!answer ^ "\n" ^ !next_str);
	done;
	String.trim !answer;;*)


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

print_endline (send_assert "assert(p(1))." xout_channel xin_channel);;
flush Pervasives.stdout;;

print_endline (send_assert "assert(p(2))." xout_channel xin_channel);;
flush Pervasives.stdout;;

let ends_with str1 str2 = 
	if String.length str2 > String.length str1
	then false
	else String.sub str1 ((String.length str1) - (String.length str2)) (String.length str2) = str2;;

let send_query str out_ch in_ch =
	output_string out_ch (str ^ "\n");
	flush out_ch;
	let answer = ref (input_line in_ch) in
	let next_str = ref "" in
	while not (ends_with !next_str "yes") && not (ends_with !next_str "no") do
		output_string out_ch ";\n";
		flush out_ch;
		next_str := input_line in_ch;
		answer := (!answer ^ "\n" ^ !next_str);
	done;
	String.trim !answer;;

print_endline (send_query "p(X)." xout_channel xin_channel);;
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
