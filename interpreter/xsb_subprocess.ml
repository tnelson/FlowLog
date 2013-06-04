#load "unix.cma";;
open Unix;;

(*match fork () with
| 0 -> execv "/Applications/XSB/bin/xsb" [|"xsb"|];
| pid -> exit 0;;*)


Unix.create_process "/Applications/XSB/bin/xsb" [|"xsb"|] Unix.stdout Unix.stdout Unix.stderr;;
Unix.sleep 1;;
print_endline "assert(p(1)).";;

while true
do
print_string ""
done;;

