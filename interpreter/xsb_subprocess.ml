#load "unix.cma";;
open Unix;;

match fork () with
| 0 -> execv "/Applications/XSB/bin/xsb" [|"xsb"|];
| pid -> exit 0;;


(*Unix.create_process "/Applications/XSB/bin/xsb" [|"xsb"|] Unix.stdout Unix.stdout Unix.stderr;;*)

