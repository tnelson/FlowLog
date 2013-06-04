#load "unix.cma";;
open Unix;;

(*match fork () with
| 0 -> execv "/Applications/XSB/bin/xsb" [|"xsb"|];
| pid -> exit 0;;*)



(* create_process prog args new_stdin new_stdout new_stderr *)
(* this line will make the parent's stdin = xsb's stdin, etc.
   that is: we'd have to TYPE assert(p(1)). to get the "yes" back.
Unix.create_process "xsb" [||] Unix.stdin Unix.stdout Unix.stderr;;
   we want something different, right?
*)

(* none of this works below... can run the above and just type xsb commands though. *)

(* Create a pipe for the child 
"The first component of the result is opened for reading, that's the exit to the pipe. 
The second component is opened for writing, that's the entrance to the pipe."
*)
let infromx_descr, outtox_descr = Unix.pipe();;

(* Convert the file descriptors created to channels *)
let xin_channel = in_channel_of_descr infromx_descr;;
print_endline "Child's stdout channel created. Becomes an in channel.";;
let xout_channel = out_channel_of_descr outtox_descr;;  
print_endline "Child's stdin channel created. Becomes an out channel.";;

(* in text mode *)
set_binary_mode_out xout_channel false;;
set_binary_mode_in xin_channel false;;

Unix.sleep 1;;


(* Use the pipe. Can keep stderr the same so XSB errors print in interpreter.
First: new stdin (OUT TO X)
Second: new stdout (IN FROM X)
 *)
(*
but this doesnt work. it should be the other way around! confusing

Unix.create_process "xsb" [||] outtox_descr infromx_descr Unix.stderr;;
*)
Unix.create_process "/Applications/XSB/bin/xsb" [|"xsb"|] outtox_descr infromx_descr Unix.stderr;;
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

(*print_endline (Stream.next xin_stream);;*)

output_string xout_channel "halt.\\n";;
flush xout_channel;;

print_char (input_char xin_channel);;
print_char (input_char xin_channel);;


Unix.sleep 5;;

(*
No need to do this: the prompt will have the streams etc. defined.

while true
do
print_string ""
done;;
*)
