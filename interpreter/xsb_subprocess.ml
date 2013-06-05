#load "unix.cma";;
open Unix;;


let from_xsb, xsb_out = Unix.pipe();;
let xsb_in, to_xsb = Unix.pipe();; 

(* Create a pipe for the child 
"The first component of the result is opened for reading, that's the exit to the pipe. 
The second component is opened for writing, that's the entrance to the pipe."
*)
(*let infromx_descr, child_stdout = Unix.pipe();;
let child_stdin, outtox_descr = Unix.pipe();;*)

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

(* trying to use streams for ease of use *)

(* From http://ocaml.org/tutorials/streams.html *)

let line_stream_of_channel channel =
  Stream.from
    (fun _ ->
       try Some (input_line channel) with End_of_file -> None);;

(* Stream for input from xsb! *)
let xin_stream = line_stream_of_channel xin_channel;;


print_char (input_char xin_channel);;
print_char (input_char xin_channel);;
print_char (input_char xin_channel);;
print_char (input_char xin_channel);;
print_char (input_char xin_channel);;

output_string xout_channel "assert(p(1)). p(X).\\n";;
flush xout_channel;;

(*without this next command the program runs fine but with it it holds. this means xsb isn't writing to the stream in response to writeln(hi).*)
print_char (input_char xin_channel);;
print_char (input_char xin_channel);;
print_char (input_char xin_channel);;
print_char (input_char xin_channel);;
print_char (input_char xin_channel);;
print_char (input_char xin_channel);;
print_char (input_char xin_channel);;
print_char (input_char xin_channel);;
print_char (input_char xin_channel);;
print_char (input_char xin_channel);;


output_string xout_channel "halt.\\n";;
flush xout_channel;;