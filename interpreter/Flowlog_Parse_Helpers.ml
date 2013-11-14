(****************************************************************)
(* Helpers for loading ASTs, desugaring, validating input, etc. *)
(****************************************************************)

open Flowlog_Types
open Flowlog_Packets
open Flowlog_Helpers
open Surface_Parser
open Surface_Lexer
open Printf
open ExtList.List
open Partial_Eval_Validation

(* Thanks to Jon Harrop on caml-list *)
let from_case_insensitive_channel ic =
  let in_quote = ref false in
  let aux buf n =
    let i = input ic buf 0 n in
    for i=0 to i-1 do
      if buf.[i] = '"' then
        ignore (in_quote := not !in_quote)
      else if not !in_quote then
        buf.[i] <- Char.lowercase buf.[i]
    done;
    i in
  Lexing.from_function aux

let read_ast (filename : string) : flowlog_ast = 
    printf "Trying to open %s\n%!" filename;
    let lexbuf = from_case_insensitive_channel (open_in filename) in
    try 
      let result = Surface_Parser.main Surface_Lexer.token lexbuf in 
        printf "Done parsing. Resulting AST: \n%!";
        pretty_print_ast result;
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
    let atom_for_on = FAtom("", r.onrel, [TVar(r.onvar)]) in (* local atom, no module name *)
    match r.action with 
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
exception NonCondensedNoField of string;;
exception RelationHadMultipleReacts of string;;
exception RelationHadMultipleDecls of string;;

let field_var_or_var (t: term): string =
  match t with
    | TVar(vname) -> vname
    | TConst(_) -> ""
    | TField(vname, _) -> vname;;

let field_vars_in (condensed: bool) (tl: term list): string list =   
  filter_map (function 
    | TVar(vname) when condensed -> Some vname
    | TVar(vname) -> None
    | TConst(_) -> None
    | TField(vname, _) -> Some vname) tl

let well_formed_rule (decls: sdecl list) (reacts: sreactive list) (r: srule): unit =    

    (* This may be called for a term in the head OR in the body.*)
    let well_formed_term (headrelname: string) (headterms: term list) (inrelname: string) (inargname: string) (t: term): unit = 
      (* Predefined in Flowlog_Types -- "forward" and "emit" have condensed args.
         All other output relations or tables have base types for args. *)
      let condensed = (mem headrelname built_in_condensed_outrels) in    

      match t with 
      | TConst(cval) -> () (* constant is always OK *)

      | TVar(vname) -> 
        if not condensed && (vname = inargname || mem vname (field_vars_in condensed headterms)) then
          raise (NonCondensedNoField(vname))      

        (* variable name in input relation *)
      | TField(vname, fname) when vname = inargname ->    
        (try         
          let valid_fields = get_valid_fields_for_input_rel decls reacts inrelname in        
          if not (mem fname valid_fields) then 
            raise (UndeclaredField(vname, fname))
        with | Not_found -> raise (UndeclaredIncomingRelation inrelname))
        
        (* the variable here is in the clause head. e.g. "newpkt"
           if this is a DO rule and the term is a field var... deal with similar to above. 
           if this is an insert/delete rule, disallow non = inargname fields.  *)        
      | TField(vname, fname) when mem vname (field_vars_in condensed headterms) ->              
        (match r.action with 
          | ADo(_, outrelterms, where) -> 
            (try              
              (* forward is a special case: it has the type of its trigger. *)
              let valid_fields = (if headrelname <> "forward" then 
                                    get_valid_fields_for_output_rel decls reacts headrelname
                                  else 
                                    get_valid_fields_for_input_rel decls reacts inrelname) in                    
              if not (mem fname valid_fields) then 
                raise (UndeclaredField(vname, fname))
            with | Not_found -> raise (UndeclaredOutgoingRelation headrelname))
          | AInsert(_, outrelterms, where)  
          | ADelete(_, outrelterms, where) -> 
            raise (UndeclaredField(vname, fname)))

      (* any other field... must be undeclared *)
      | TField(vname, fname) ->                
          raise (UndeclaredField(vname,fname)) 
      in

  let well_formed_atom (headrelname: string) (headterms: term list) (inrelname: string) (inargname: string) (atom: formula) :unit =
    match atom with 
      | FAtom(modname, relname, argtl) -> 
        (try           
          let decl = (find (function
                                 | DeclTable(dname, _) when dname = relname -> true 
                                 | DeclRemoteTable(dname, _) when dname = relname -> true 
                                 | _ -> false) decls) in              
              (match decl with 
                | DeclTable(_, typeargs) 
                | DeclRemoteTable(_, typeargs) ->
                  if length typeargs <> length argtl then
                    raise (BadArityOfTable relname);
                | _ -> failwith "validate_rule");                   
          iter (well_formed_term headrelname headterms inrelname inargname) argtl;
        with | Not_found -> raise (UndeclaredTable relname))

      | FEquals(t1, t2) ->
        well_formed_term headrelname headterms inrelname inargname t1;
        well_formed_term headrelname headterms inrelname inargname t2;
      | _ -> failwith "validate_rule" in

  (* regardless whether this rule is DO or INSERT etc. check these: *)
  let validate_common_elements inrelname inrelarg outrelname outrelterms where = 
    iter (well_formed_atom outrelname outrelterms inrelname inrelarg) (get_atoms where);        
    iter (fun (_, f) -> (well_formed_atom outrelname outrelterms inrelname inrelarg f)) (get_equalities where);        
    iter (well_formed_term outrelname outrelterms inrelname inrelarg) outrelterms;
    if not (exists (function | DeclInc(dname, _) when dname = inrelname -> true | _ -> false) decls) then    
      raise (UndeclaredIncomingRelation inrelname)
  in 

  match r.action with     
    (* DO must have outgoing relation in action, and must be correct arity *)
    |  ADo(outrelname, outrelterms, where) -> 
        validate_common_elements r.onrel r.onvar outrelname outrelterms where;
        (try
          let dargs = first (filter_map (function | DeclOut(dname, dargs) when dname = outrelname -> Some dargs | _ -> None) decls) in
          if (length outrelterms) <> (length dargs) then
            raise (BadArityOfTable outrelname)          
        with Not_found -> raise (UndeclaredOutgoingRelation outrelname))

    (* insert and delete must have local table in action, of correct arity *)
    | AInsert(relname, outrelterms, where)  
    | ADelete(relname, outrelterms, where) ->
        validate_common_elements r.onrel r.onvar relname outrelterms where;        
        (try
          let dargs = first (filter_map (function | DeclTable(dname, dargs) when dname = relname -> Some dargs | _ -> None) decls) in
          if (length outrelterms) <> (length dargs) then
            raise (BadArityOfTable relname)          
        with Not_found -> raise (UndeclaredTable relname));;

let simplify_clause (cl: clause): clause =   
    {head = cl.head; orig_rule = cl.orig_rule; body = minimize_variables cl.body};;

let well_formed_reacts (reacts: sreactive list): unit = 
  ignore (fold_left (fun acc react -> 
    match react with 
      | ReactOut(relname, _, evtype, _, _) 
      | ReactInc(evtype, relname) -> 
        if mem relname acc then
          raise (RelationHadMultipleReacts(relname))
        else relname::acc
      | _ -> acc) [] reacts);;
      
let well_formed_decls (decls: sdecl list): unit = 
  ignore (fold_left (fun acc decl -> 
    match decl with 
      | DeclOut(relname, _) 
      | DeclInc(relname, _) 
      | DeclTable(relname, _) 
      | DeclRemoteTable(relname, _) -> 
        if mem relname acc then
          raise (RelationHadMultipleDecls(relname))
        else relname::acc
      | _ -> acc) [] decls);;

(* + There is some information overlap between declarations and reactive definitions. *)

let reacts_added_by_sugar (stmts: stmt list): sreactive list =
  (* If we have an event decl X, return an incoming reactive defn X if X is undeclared. *)
  let inc_events_with_react = filter_map (function SReactive(ReactInc(evname,_)) -> Some evname | _ -> None) stmts in  
  let event_decls = filter_map (function SDecl(DeclEvent(_,_) as d) -> Some d     | _ -> None) stmts in 
  fold_left (fun acc decl -> match decl with
                      | DeclEvent(ename, _) when not (mem ename inc_events_with_react) -> 
                          ReactInc(ename, ename)::acc
                      | _ -> failwith "reacts_added_by_sugar") [] event_decls;;

let decls_added_by_sugar (stmts: stmt list): sdecl list =
  (* If we have an event decl X, return an incoming decl X if X is undeclared. *)  
  let inc_events_with_decl = filter_map (function SDecl(DeclInc(_,evname)) -> Some evname | _ -> None) stmts in  
  let event_decls = filter_map (function SDecl(DeclEvent(_,_) as d) -> Some d     | _ -> None) stmts in 
  fold_left (fun acc decl -> match decl with
                      | DeclEvent(ename, _) when not (mem ename inc_events_with_decl) -> 
                          DeclInc(ename, ename)::acc
                      | _ -> failwith "decls_added_by_sugar") [] event_decls;;

(* built_in_where_for_vname (vname: string) (relname: string): *)
let add_built_ins (r: srule): srule =    
  let to_add_for_incoming = (built_in_where_for_variable (TVar(r.onvar)) r.onrel) in
  (* built-in outgoing additions will only work on condensed rels as written (note the length = 1 check) *)
  let to_add_for_outgoing = (match r.action with | ADo(outrel, outterms, where) when (length outterms) = 1 ->
                                                (built_in_where_for_variable (first outterms) outrel ) 
                                            | _ -> FTrue) in      
  let act' = if to_add_for_incoming <> FTrue  
             then add_conjunct_to_action r.action to_add_for_incoming 
             else r.action in
  let act'' = if to_add_for_outgoing <> FTrue 
              then add_conjunct_to_action act' to_add_for_outgoing
              else act' in
    {r with action=act''};;


(*
 * takes a list of ASTs and returns a single AST which is comprised of the
 * flattened list of statements, and a flattened, unique list of includes
 *)

let flatten_asts (asts : flowlog_ast list) : flowlog_ast =
  let merge_ast = fun l_ast r_ast ->
    let uniq = fun acc a -> if not (mem a acc)
                            then a::acc
                            else acc in
    match l_ast with AST(l_includes, l_stmts) ->
    match r_ast with AST(r_includes, r_stmts) ->
    let includes = fold_left uniq l_includes r_includes in
    AST(includes, l_stmts @ r_stmts) in

  fold_left merge_ast (AST([], [])) asts

(*
 * takes an AST and a list of previously included files, returns a new AST
 * where the included files have been turned into ASTs, appended to the given
 * AST, with their own included files recurisvely expanded.
 *
 * the list of previously included files is needed to ensure that we don't
 * include a file more than once
 *)

let rec expand_includes (ast : flowlog_ast) (prev_includes : string list) : flowlog_ast =
  match ast with AST(quoted_includes, stmts) ->
    if length quoted_includes = 0 then ast
    else
      let unquote = fun qfn -> String.sub qfn 1 ((String.length qfn) - 2) in
      let includes = map unquote quoted_includes in
      let maybe_read_ast filename = if mem filename prev_includes
                                    then AST([], [])
                                    else read_ast filename in

      let flattened_ast = flatten_asts (map maybe_read_ast includes) in

      match flattened_ast with AST(new_includes, new_stmts) ->
        let new_ast = AST(new_includes, stmts @ new_stmts) in
        expand_includes new_ast (prev_includes @ includes)

(* some duplication here from Flowlog_Graphs for now. *)
  let build_memos_for_program (rules: srule list): program_memos =   
    let memos = {out_triggers = Hashtbl.create 5; insert_triggers = Hashtbl.create 5; delete_triggers = Hashtbl.create 5;} in
    let depends_from_rule (r: srule): unit =  
      match r.action with
        | AInsert(headrel, _, fmla) ->
          Hashtbl.add memos.insert_triggers r.onrel headrel
        | ADelete(headrel, _, fmla) ->
          Hashtbl.add memos.delete_triggers r.onrel headrel
        | ADo(headrel, _, fmla) -> 
          Hashtbl.add memos.out_triggers r.onrel headrel in
     iter depends_from_rule rules;
     memos;;

let desugared_program_of_ast (ast: flowlog_ast) (filename : string): flowlog_program =
  let expanded_ast = expand_includes ast [filename] in
    match expanded_ast with AST(_, stmts) ->
        (* requires extlib *)
        let the_decls  =  built_in_decls @ 
                          filter_map (function | SDecl(d) -> Some d   
                                               | _ -> None) stmts @
                          (decls_added_by_sugar stmts) in 
        let the_reacts =  built_in_reacts @ 
                          filter_map (function | SReactive(r) -> Some r 
                                               | _ -> None) stmts @
                          (reacts_added_by_sugar stmts) in 
        let the_rules  =  filter_map (function | SRule(r) -> Some (add_built_ins r)
                                               | _ -> None) stmts in
            

            (* Validation done here and below! *)
            iter (well_formed_rule the_decls the_reacts) the_rules;   
            well_formed_reacts the_reacts;
            well_formed_decls the_decls;       

            let clauses = (fold_left (fun acc r -> (clauses_of_rule r) @ acc) [] the_rules) in 
            let simplified_clauses = map simplify_clause clauses in 
            (* pre-determine what can be fully compiled. pre-determine weakened versions of other packet-triggered clauses*)
            let can_fully_compile_simplified, weakened_cannot_compile_pt_clauses, not_fully_compiled_clauses = 
              fold_left (fun (acc_comp, acc_weaken, acc_unweakened) cl -> 
                                   let (v, t) = trim_packet_from_body cl.body in                         
                                     if v = "" then (* not packet-triggered *)
                                       (acc_comp, acc_weaken, cl::acc_unweakened)
                                     else if can_compile_clause_to_fwd cl then (* fully compilable *)
                                       ({oldpkt=v; clause={head = cl.head; orig_rule = cl.orig_rule; body = t}} :: acc_comp, acc_weaken, acc_unweakened)
                                     else (* needs weakening AND needs storing for XSB *)
                                       (acc_comp, {oldpkt=v; clause=weaken_uncompilable_packet_triggered_clause v 
                                                               {head = cl.head; orig_rule = cl.orig_rule; body = t}} :: acc_weaken, cl::acc_unweakened))
                          ([],[],[]) simplified_clauses in 

              printf "\n  Loaded AST. There were %d clauses, \n    %d of which were fully compilable forwarding clauses and\n    %d were weakened pkt-triggered clauses.\n    %d will be given, unweakened, to XSB.\n%!"              
                (length simplified_clauses) (length can_fully_compile_simplified) (length weakened_cannot_compile_pt_clauses) (length not_fully_compiled_clauses);
              printf "Reacts: %s\n%!" (String.concat ", " (map string_of_reactive the_reacts));
              printf "Decls: %s\n%!" (String.concat ", " (map string_of_declaration the_decls));

                {decls = the_decls; reacts = the_reacts; clauses = simplified_clauses; 
                 weakened_cannot_compile_pt_clauses = weakened_cannot_compile_pt_clauses;
                 can_fully_compile_to_fwd_clauses = can_fully_compile_simplified;
                 (* Remember: these are unweakened, and so can be used by XSB. *)
                 not_fully_compiled_clauses = not_fully_compiled_clauses;
                 memos = build_memos_for_program the_rules};;