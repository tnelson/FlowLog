#load "unix.cma";;

let my_entry_byname = Unix.gethostbyname (Unix.gethostname ());;
let my_addr = my_entry_byname.Unix.h_addr_list.(0) ;;

let s_descr = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 ;;
Unix.bind s_descr (Unix.ADDR_INET(my_addr, 1234));;
Unix.listen s_descr 1;;
let (new_descr, new_addr) = Unix.accept s_descr;;
let str = "i'm writing this string";;
Unix.write new_descr str 0 String.length str;;



