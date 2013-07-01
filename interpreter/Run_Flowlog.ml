open Flowlog;;
open Controller;;
open Parser;;
open Lexer;;

module Parsed_Program : PROGRAM = struct
	let filename = try Sys.argv.(1) with exn -> raise (Failure "Input a .flg filepath.");;
	let lexbuf = Lexing.from_channel (open_in Sys.argv.(1));;
	let relations = try Parser.main Lexer.token lexbuf
		with exn -> 
		let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        let _ = print_endline (string_of_int line) in
        let _ = print_endline (string_of_int cnum) in
        let _ = print_endline tok in
        raise exn;;
     let program = Flowlog_Parsing.make_program Sys.argv.(1) relations;;
end

module Run = Controller.Make_Controller (Parsed_Program);;