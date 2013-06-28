open Flowlog;;
open Controller;;
open Parser;;
open Lexer;;

let lexbuf = Lexing.from_channel Sys.argv.(0) in
let prgm = Parser.main (Lexer.token lexbuf);;

module Run = Controller.Make_Controller(struct let program = prgm end);;

