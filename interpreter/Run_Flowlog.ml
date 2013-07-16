open Type_Helpers;;
open Controller;;
open Parser;;
open Lexer;;

let read_program (filename : string) : Syntax.program = 
    let lexbuf = Lexing.from_channel (open_in filename) in
    try Parser.main Lexer.token lexbuf
        with exn -> 
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        print_endline ("file " ^ filename ^ " has an error on line " ^ (string_of_int line) ^ " column " ^ (string_of_int cnum) ^
            " in token " ^ tok);
        raise exn;;

let rec parse_imports_helper (filenames : string list) (already_parsed : Syntax.program list) (already_seen : string list) : Syntax.program list =
    match filenames with
    | [] -> already_parsed;
    | h :: t -> if List.mem h already_seen then raise (Type_Helpers.Parse_error ("circular imports: " ^ (Type_Helpers.list_to_string (fun x -> x) (h :: already_seen)))) else
    let first = read_program h in
        match first with Syntax.Program(_, imports, _, _, _) ->
        parse_imports_helper(List.map (fun str -> str ^ ".flg") imports) @ t, first :: already_parsed, h :: already_seen);;

let build_finished_program (filename : string) : Types.program = 
    let syntax_program = read_program filename in
    match syntax_program with Syntax.Program(_, imports, _, _, _) ->
    Conversion.program_convert (Parsing.import syntax_program (parse_imports_helper imports [] [filename]));;


module Parsed_Program : PROGRAM = struct
	let filename = try Sys.argv.(1) with exn -> raise (Failure "Input a .flg filename in the current directory.");;
    let program = build_finished_program filename;;
end

module Run = Controller.Make_Controller (Parsed_Program);;