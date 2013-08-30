open Surface_Parser
open Surface_Lexer
open Flowlog_Types
open Partial_Eval
open Printf
open Arg
open Flowlog_To_Alloy

open NetCore_Types
open Packet
open OpenFlow0x01
open Lwt
open NetCore_Pattern
open NetCore_Wildcard
open NetCore_Controller

open Xsb_Communication

(* Use ExtList.List instead -- provides filter_map, but also tail-recursive combine *)
(*open List;;*)
open ExtList.List

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

(* usage message *)
let usage = Printf.sprintf "Usage: %s [-alloy] file.flg" (Filename.basename Sys.argv.(0));;
let alloy = ref false;;
let args = ref [];;
let speclist = [
  ("-alloy", Arg.Unit (fun () -> alloy := true), ": convert to Alloy");];;

let simplify_clause (cl: clause): clause =   
    {head = cl.head; orig_rule = cl.orig_rule; body = minimize_variables cl.body};;

let simplify_clauses (p: flowlog_program) =
  let newclauses = map simplify_clause p.clauses in
    {decls = p.decls; reacts = p.reacts; clauses = newclauses};;

let listenPort = ref 6633;;

let run_flowlog (p: flowlog_program): unit Lwt.t =  
  (* Start up XSB, etc. *)
  Communication.start_program p;
 
  (* Listen for incoming notifications via RPC *)
  (*Flowlog_Thrift_In.start_listening p;;
 *)
(*)
  (* Send the "startup" notification. Enables initialization, etc. in programs *)
  let startup = Types.Constant([], Types.startup_type) in
    Evaluation.respond_to_notification startup Program.program None;;
*)
  (* TODO: How to catch switch connection events in Frenetic? Was easy in Ox. *)

  (* Start the policy stream *)
  (* >> is from Lwt's Pa_lwt. But you MUST have -syntax camlp4o or it won't be recoginized. *)   
  OpenFlow0x01_Platform.init_with_port !listenPort >>
    let (gen_stream, stream) = make_policy_stream p in
    (* streams for incoming/exiting packets *)
    let (pkt_stream, push_pkt) = Lwt_stream.create () in
      Lwt.pick [gen_stream; NetCore_Controller.start_controller pkt_stream stream];;

let main () =
  let collect arg = args := !args @ [arg] in
  let _ = Arg.parse speclist collect usage in
  let filename = try hd !args with exn -> raise (Failure "Input a .flg file name.") in  
  let ast = read_ast filename in
  let program = simplify_clauses (desugared_program_of_ast ast) in    
    printf "-----------\n%!";
    List.iter (fun cl -> printf "%s\n\n%!" (string_of_clause cl)) program.clauses;

    if !alloy then write_as_alloy program (filename^".als")
    else 
      Sys.catch_break true;
      try
        Lwt_main.run (run_flowlog program)
      with exn ->
        Xsb.halt_xsb ();
        Format.printf "Unexpected exception: %s\n%s\n%!"
          (Printexc.to_string exn)
          (Printexc.get_backtrace ());
        exit 1;;
    
 main();;
