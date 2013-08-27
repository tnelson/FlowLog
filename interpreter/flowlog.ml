open Surface_Parser;;
open Surface_Lexer;;
open Flowlog_Types;;
open Printf;;

(* Use ExtList.List instead -- provides filter_map, but also tail-recursive combine *)
(*open List;;*)
open ExtList.List;;

(* Thanks to Jon Harrop on caml-list *)
let from_case_insensitive_channel ic =
  let aux buf n =
    let i = input ic buf 0 n in
    for i=0 to i-1 do
      buf.[i] <- Char.lowercase buf.[i]
    done;
    i in
  Lexing.from_function aux

let read_ast (filename : string) : flowlog_ast = 
    printf "Trying to open %s\n%!" filename;
    let lexbuf = from_case_insensitive_channel (open_in filename) in
    try 
      let result = Surface_Parser.main Surface_Lexer.token lexbuf in 
        printf "Done parsing.\n%!";
        pretty_print_program result;
        result
    with exn -> 
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
      print_endline ("File " ^ filename ^ " has an error on line " ^ 
                    (string_of_int line) ^ " column " ^ (string_of_int cnum) ^
                    " at the token " ^ tok);
      raise exn;;

(*let rec parse_imports_helper (filenames : string list) (already_parsed : Types.program list) (already_seen : string list) : Types.program list =
    match filenames with
    | [] -> already_parsed;
    | h :: t -> if List.mem h already_seen then raise (Failure ("circular imports: " ^ (Type_Helpers.list_to_string (fun x -> x) (h :: already_seen)))) else
    let first = read_program h in
        match first with Types.Program(_, imports, _, _, _) ->
        parse_imports_helper ((List.map (fun str -> str ^ ".flg") imports) @ t) (first :: already_parsed) (h :: already_seen);;

let build_finished_program (filename : string) : Types.program = 
    let prgm = read_program filename in
    match prgm with Types.Program(_, imports, _, _, _) ->
    Parse_Helpers.process_program_types (Parse_Helpers.import prgm (parse_imports_helper (List.map (fun str -> str ^ ".flg") imports) [] [filename]));;
*)

(**************************************************************************)
(* TEMPORARY: Grafting in new parser. All this will change due to swap to Frenetic from Ox. *)

(*module Parsed_Program : PROGRAM = struct
	let filename = try Sys.argv.(1) with exn -> raise (Failure "Input a .flg filename in the current directory.");;
    let program = build_finished_program filename;;
end*)

(*module Run = Controller.Make_Controller (Parsed_Program);;*)

let build_clause (r: srule) (in_atom: formula) (relname: string) (terms: term list) (prefix: string) (conj: formula): clause =
    let head = FAtom("", prefix^"_"^relname, terms) in
    let body = FAnd(in_atom, conj) in
    {orig_rule = r; head = head; body = body};;

let clauses_of_rule (r: srule): clause list =
    match r with Rule(increlname, incterm, act) ->    
    let atom_for_on = FAtom("", increlname, [TVar(incterm)]) in (* local atom, no module name *)
    match act with 
        | ADelete(relname, terms, condition) -> 
            map (build_clause r atom_for_on relname terms "minus") (extract_disj_list (disj_to_top condition));
        | AInsert(relname, terms, condition) -> 
            map (build_clause r atom_for_on relname terms "plus") (extract_disj_list (disj_to_top condition));
        | ADo(relname, terms, condition) -> 
            map (build_clause r atom_for_on relname terms "do") (extract_disj_list (disj_to_top condition));;     

let desugared_program_of_ast (ast: flowlog_ast): flowlog_program =
    printf "*** REMINDER: IMPORTS NOT YET HANDLED! ***\n%!";
    match ast with AST(imports, stmts) ->
        (* requires extlib *)
        let the_decls  = filter_map (function SDecl(d) -> Some d     | _ -> None) stmts in 
        let the_reacts = filter_map (function SReactive(r) -> Some r | _ -> None) stmts in 
        let the_rules  = filter_map (function SRule(r) -> Some r     | _ -> None) stmts in 

            let clauses = (fold_left (fun acc r -> (clauses_of_rule r) @ acc) [] the_rules) in 
                {decls = the_decls; reacts = the_reacts; clauses = clauses};;

let filename = try Sys.argv.(1) with exn -> raise (Failure "Input a .flg file name.");;
let ast = read_ast filename;;
let program = desugared_program_of_ast ast;;

printf "-----------\n%!";;
List.iter (fun cl -> printf "%s\n\n%!" (string_of_clause cl)) program.clauses;;