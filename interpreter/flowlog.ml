open Surface_Parser
open Surface_Lexer
open Flowlog_Types
open Flowlog_Helpers
open Partial_Eval
open Printf
open Arg
open Flowlog_To_Alloy

open NetCore_Types
open Packet
open OpenFlow0x01
open OpenFlow0x01_Platform
open OpenFlow0x01_Core
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

let negations_to_end (f: formula): formula =
  let atoms = conj_to_list f in
  let (pos, neg) = partition (function | FNot(_) -> false | _ -> true) atoms in
    build_and (pos @ neg);;

let build_clause (r: srule) (in_atom: formula) (relname: string) (terms: term list) (prefix: string option) (conj: formula): clause =
    let real_relname = (match prefix with | Some p -> (p^"_"^relname) | None -> relname) in
    let head = FAtom("", real_relname, terms) in
    let body = FAnd(in_atom, conj) in
    {orig_rule = r; head = head; body = negations_to_end body};;

let clauses_of_rule (r: srule): clause list =
    match r with Rule(increlname, incterm, act) ->    
    let atom_for_on = FAtom("", increlname, [TVar(incterm)]) in (* local atom, no module name *)
    match act with 
        | ADelete(relname, terms, condition) -> 
            map (build_clause r atom_for_on relname terms (Some minus_prefix)) (disj_to_list (disj_to_top (nnf condition)));
        | AInsert(relname, terms, condition) -> 
            map (build_clause r atom_for_on relname terms (Some plus_prefix)) (disj_to_list (disj_to_top (nnf condition)));
        | ADo(relname, terms, condition) -> 
            map (build_clause r atom_for_on relname terms None) (disj_to_list (disj_to_top (nnf condition)));;     

exception UndeclaredIncomingRelation of string;;
exception UndeclaredOutgoingRelation of string;;
exception UndeclaredTable of string;;
exception BadArityOfTable of string;;
exception UndeclaredField of string * string;;

let field_var_or_var (t: term): string =
  match t with
    | TVar(vname) -> vname
    | TConst(_) -> ""
    | TField(vname, _) -> vname;;

let well_formed_rule (decls: sdecl list) (r: srule): unit =
  let well_formed_atom (headterms: term list) (inargname: string) (at: formula) :unit =
    let well_formed_term (t: term): unit = 
      match t with 
      | TVar(vname) -> () 
      | TConst(cval) -> ()
      (* todo: make certain that the field names are valid *)
      | TField(vname, fname) when vname = inargname ->         
        ()
      | TField(vname, fname) when mem vname (map field_var_or_var headterms) ->         
        ()
      | TField(vname, fname) -> 
        raise (UndeclaredField(vname,fname)) in

    match at with 
      | FAtom(modname, relname, argtl) -> 
        (try 
          let decl = (find (function | DeclTable(dname, _) when dname = relname -> true 
                                 | DeclRemoteTable(dname, _) when dname = relname -> true 
                                 | _ -> false) decls) in

              (match decl with 
                | DeclTable(_, typeargs) 
                | DeclRemoteTable(_, typeargs) ->
                  if length typeargs <> length argtl then
                    raise (BadArityOfTable relname);
                | _ -> failwith "validate_rule");         
          iter well_formed_term argtl;
        with | Not_found -> raise (UndeclaredTable relname))

      | FEquals(t1, t2) ->
        well_formed_term t1;
        well_formed_term t2;
      | _ -> failwith "validate_rule" in

        (* TODO: ugly code, should be cleaned up *)
  match r with 
    (* DO must have outgoing relation in action *)
    | Rule(inrelname, inrelarg, ADo(outrelname, outrelterms, where)) -> 
        iter (well_formed_atom outrelterms inrelarg) (get_atoms where);        
        iter (fun (_, f) -> (well_formed_atom outrelterms inrelarg f)) (get_equalities where);        
        if not (exists (function | DeclInc(dname, _) when dname = inrelname -> true | _ -> false) decls) then
          raise (UndeclaredIncomingRelation inrelname);
        if not (exists (function | DeclOut(dname, _) when dname = outrelname -> true | _ -> false) decls) then
          raise (UndeclaredOutgoingRelation outrelname); 

    (* insert and delete must have local table in action *)
    | Rule(inrelname, inrelarg, AInsert(relname, outrelterms, where))  
    | Rule(inrelname, inrelarg, ADelete(relname, outrelterms, where)) ->
        iter (well_formed_atom outrelterms inrelarg) (get_atoms where);            
        iter (fun (_, f) -> (well_formed_atom outrelterms inrelarg f)) (get_equalities where);        
        if not (exists (function | DeclInc(dname, _) when dname = inrelname -> true | _ -> false) decls) then
          raise (UndeclaredIncomingRelation inrelname);
        if not (exists (function | DeclTable(dname, _) when dname = relname -> true                                  
                                 | _ -> false) decls) then
          raise (UndeclaredTable relname);;

let simplify_clause (cl: clause): clause =   
    {head = cl.head; orig_rule = cl.orig_rule; body = minimize_variables cl.body};;

let desugared_program_of_ast (ast: flowlog_ast): flowlog_program =
    printf "*** REMINDER: IMPORTS NOT YET HANDLED! (Remember to handle in partial eval, too.) ***\n%!"; (* TODO *)
    match ast with AST(imports, stmts) ->
        (* requires extlib *)
        let the_decls  =  built_in_decls @ 
                          filter_map (function SDecl(d) -> Some d     | _ -> None) stmts in 
        let the_reacts =  built_in_reacts @ 
                          filter_map (function SReactive(r) -> Some r | _ -> None) stmts in 
        let the_rules  =  filter_map (function SRule(r) -> Some r     | _ -> None) stmts in 
            iter (well_formed_rule the_decls) the_rules;          
            let clauses = (fold_left (fun acc r -> (clauses_of_rule r) @ acc) [] the_rules) in 
            let simplified_clauses = map simplify_clause clauses in 
            let can_fully_compile_simplified = filter can_compile_clause_to_fwd simplified_clauses in
              printf "Loaded AST. There were %d clauses, %d of which were fully compilable forwarding clauses.\n%!"
                (length simplified_clauses) (length can_fully_compile_simplified);
                {decls = the_decls; reacts = the_reacts; clauses = simplified_clauses; 
                 can_fully_compile_to_fwd_clauses = can_fully_compile_simplified};;

                

(* usage message *)
let usage = Printf.sprintf "Usage: %s [-alloy] [-notables] file.flg" (Filename.basename Sys.argv.(0));;
let alloy = ref false;;
let notables = ref false;;
let reportall = ref false;;
let args = ref [];;

let speclist = [
  ("-verbose", Arg.Int (fun lvl -> global_verbose := lvl), ": set level of debug output");
  ("-alloy", Arg.Unit (fun () -> alloy := true), ": convert to Alloy");
  ("-reportall", Arg.Unit (fun () -> reportall := true), ": report all packets. WARNING: VERY SLOW!");
  (* Not calling this "reactive" because reactive still implies sending table entries. *)
  ("-notables", Arg.Unit (fun () -> notables := true), ": send everything to controller");];;

let listenPort = ref 6633;;

let run_flowlog (p: flowlog_program): unit Lwt.t =  
  (* Start up XSB, etc. *)
  Communication.start_program p !notables;
 
  (* Listen for incoming notifications via RPC *)
  Flowlog_Thrift_In.start_listening p;

  (* Start the policy stream *)
  (* >> is from Lwt's Pa_lwt. But you MUST have -syntax camlp4o or it won't be recoginized. *)   
  OpenFlow0x01_Platform.init_with_port !listenPort >>
    let (trigger_re_policy_func, (gen_stream, stream)) = (make_policy_stream p !notables !reportall) in
    refresh_policy := Some trigger_re_policy_func;

    (* streams for incoming/exiting packets *)
    let (pkt_stream, push_pkt) = Lwt_stream.create () in        
    emit_push := Some push_pkt;

    (* Send the "startup" notification. Enables initialization, etc. in programs *)         
    respond_to_notification p {typeid="startup"; values=StringMap.empty};

      (* pick cancels all threads given if one terminates *)             
      (* DO NOT attempt to copy ox/frenetic's switch connection detection code here. It will clash with 
         Frenetic's. Instead, register a HandleSwitchEvent policy, which gives us a nice clean callback. *)
      Lwt.pick [gen_stream; NetCore_Controller.start_controller pkt_stream stream];;


let main () =
  let collect arg = args := !args @ [arg] in
  let _ = Arg.parse speclist collect usage in
  let filename = try hd !args with exn -> raise (Failure "Input a .flg file name.") in  
  let ast = read_ast filename in
  let program = (desugared_program_of_ast ast) in    
    printf "-----------\n%!";
    List.iter (fun cl -> printf "%s\n\n%!" (string_of_clause cl)) program.clauses;

    if !alloy then write_as_alloy program (filename^".als")
    else 
      (* Intercede when Ctrl-C is pressed to close XSB, etc. *)
      Sys.catch_break true;
      (* If SIGPIPE ("broken pipe") failure (exit code 141), actually give an error. 
         Without this set, the program terminates with no message. *)
      Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
      try      
        out_log := Some(open_out "log_for_flowlog.log");  
        if !notables then printf "\n*** FLOW TABLE COMPILATION DISABLED! ***\n%!";
        Lwt_main.at_exit (fun () -> return (printf "LWT exiting~\n%!") );
        at_exit (fun () -> (printf "Ocaml exiting~\n%!"));        
        Lwt_main.run (run_flowlog program);     
        printf "LWT Terminated!\n%!";
      with
        | Sys.Break ->
          Xsb.halt_xsb();
          close_log(); 
          printf "\nExiting gracefully due to break signal.\n%!";
          exit 101
        | exn ->
        Xsb.halt_xsb ();
        Format.printf "\nUnexpected exception: %s\n%s\n%!"
          (Printexc.to_string exn)
          (Printexc.get_backtrace ());
        close_log();
        exit 100;;
    
 main();;


