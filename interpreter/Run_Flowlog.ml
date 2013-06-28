open Flowlog;;
open Controller;;
open Parser;;
open Lexer;;

module Parsed_Program : PROGRAM = struct
	let lexbuf = Lexing.from_channel Sys.argv.(0);;
	let relations = Parser.main (Lexer.token lexbuf);;
	let program = Flowlog.Program(Sys.argv.(0), relations)
end

module Run = Controller.Make_Controller (Parsed_Program);;

