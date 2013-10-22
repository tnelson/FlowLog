open Flowlog_Types
open Flowlog_Helpers
open Surface_Parser
open Surface_Lexer
open Printf
open ExtList.List
open Partial_Eval

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
        let valid_fields = get_valid_fields_for_input_rel decls reacts inrelname in        
        if not (mem fname valid_fields) then 
          raise (UndeclaredField(vname, fname))
        
        (* the variable here is in the clause head. e.g. "newpkt"
           if this is a DO rule and the term is a field var... deal with similar to above. 
           if this is an insert/delete rule, disallow non = inargname fields.  *)        
      | TField(vname, fname) when mem vname (field_vars_in condensed headterms) ->              
        (match r with 
          | Rule(inrelname, inrelarg, ADo(_, outrelterms, where)) -> 
            let valid_fields = get_valid_fields_for_output_rel decls reacts headrelname in                    
            if not (mem fname valid_fields) then 
              raise (UndeclaredField(vname, fname))
          | Rule(inrelname, inrelarg, AInsert(_, outrelterms, where))  
          | Rule(inrelname, inrelarg, ADelete(_, outrelterms, where)) -> 
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

  match r with     
    (* DO must have outgoing relation in action, and must be correct arity *)
    | Rule(inrelname, inrelarg, ADo(outrelname, outrelterms, where)) -> 
        validate_common_elements inrelname inrelarg outrelname outrelterms where;
        (try
          let dargs = first (filter_map (function | DeclOut(dname, dargs) when dname = outrelname -> Some dargs | _ -> None) decls) in
          if (length outrelterms) <> (length dargs) then
            raise (BadArityOfTable outrelname)          
        with Not_found -> raise (UndeclaredOutgoingRelation outrelname))

    (* insert and delete must have local table in action, of correct arity *)
    | Rule(inrelname, inrelarg, AInsert(relname, outrelterms, where))  
    | Rule(inrelname, inrelarg, ADelete(relname, outrelterms, where)) ->
        validate_common_elements inrelname inrelarg relname outrelterms where;        
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
      | DeclInc(relname, _) -> 
        if mem relname acc then
          raise (RelationHadMultipleDecls(relname))
        else relname::acc
      | _ -> acc) [] decls);;

let desugared_program_of_ast (ast: flowlog_ast): flowlog_program =
    printf "*** REMINDER: IMPORTS NOT YET HANDLED! (Remember to handle in partial eval, too.) ***\n%!"; (* TODO *)
    match ast with AST(imports, stmts) ->
        (* requires extlib *)
        let the_decls  =  built_in_decls @ 
                          filter_map (function SDecl(d) -> Some d     | _ -> None) stmts in 
        let the_reacts =  built_in_reacts @ 
                          filter_map (function SReactive(r) -> Some r | _ -> None) stmts in 
        let the_rules  =  filter_map (function SRule(r) -> Some r     | _ -> None) stmts in 
            
            iter (well_formed_rule the_decls the_reacts) the_rules;   
            well_formed_reacts the_reacts;
            well_formed_decls the_decls;       

            let clauses = (fold_left (fun acc r -> (clauses_of_rule r) @ acc) [] the_rules) in 
            let simplified_clauses = map simplify_clause clauses in 
            let can_fully_compile_simplified = filter can_compile_clause_to_fwd simplified_clauses in
              printf "Loaded AST. There were %d clauses, %d of which were fully compilable forwarding clauses.\n%!"
                (length simplified_clauses) (length can_fully_compile_simplified);
                {decls = the_decls; reacts = the_reacts; clauses = simplified_clauses; 
                 can_fully_compile_to_fwd_clauses = can_fully_compile_simplified};;

