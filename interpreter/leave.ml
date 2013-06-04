#load "unix.cma";;
open Unix;;
let leave () =
    let hh = int_of_string (String.sub Sys.argv.(1) 0 2)
    and mm = int_of_string (String.sub Sys.argv.(1) 2 2) in
    let now = localtime(time ()) in
    let delay = (hh - now.tm_hour) * 3600 + (mm - now.tm_min) * 60 in
    if delay <= 0 then begin
       print_endline "Hey! That time has already passed!";
       exit 0
    end;
    if fork () <> 0 then exit 0;
    sleep delay;
    print_endline "\007\007\007Time to leave!";
    exit 0;;

handle_unix_error leave ();;