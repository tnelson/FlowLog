(****************************************************************)
(* Helpers for loading ASTs, desugaring, validating input, etc. *)
(****************************************************************)

open Flowlog_Types
open Flowlog_Packets
open Flowlog_Helpers
open Flowlog_Builtins
open Surface_Parser
open Surface_Lexer
open Printf
open ExtList.List
open Partial_Eval_Validation
open Xsb_Communication

exception SyntaxAnyInPlus of formula;;
exception SyntaxUnsafe of term;;

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
            map (build_clause r atom_for_on relname terms None) (disj_to_list (disj_to_top (nnf condition)))
        | AForward(p, fmla, tout) ->
            map (build_clause r atom_for_on "forward" [p] None) (disj_to_list (disj_to_top (nnf fmla)))
        | AStash(p, where, until, thens) ->
            (* TODO: not enough yet; need to handle the "until" and "then" parts! *)
            map (build_clause r atom_for_on "stash" [p] None) (disj_to_list (disj_to_top (nnf where)));;

let field_var_or_var (t: term): string =
  match t with
    | TVar(vname) -> vname
    | TConst(_) -> ""
    | TField(vname, _) -> vname;;

let field_vars_in (tl: term list): string list =
  filter_map (function
    | TVar(vname) -> Some vname
    | TConst(_) -> None
    | TField(vname, _) -> Some vname) tl

let get_terms_to_prove_safe (p: flowlog_program) (cl: clause): term list =
  let on_context = Communication.get_on_context p cl in
  let must_be_safe = (match cl.head with
    | FAtom(modname, relname, [outarg]) when relname = "forward" || (starts_with relname "emit") ->
      (* forward/emits: use mentioned fields only*)
      let pfields = Communication.decls_expand_fields p modname relname on_context 1 outarg in
        (*iter (fun t -> printf "fwd: pfields: %s\n%!" (string_of_term t)) pfields;*)
        get_terms (fun t -> mem t pfields) cl.body

    (* If a plus rule uses ANY in the head, it's an error *)
    | FAtom(modname, relname, outargs) when (starts_with relname "plus_")  ->
      let to_check = (filter is_variable_term outargs) in
        if (exists is_ANY_term to_check) then raise (SyntaxAnyInPlus(cl.head))
        else to_check

    (* Unconstrained vars in the head of a minus rule just means "any" *)
    | FAtom(modname, relname, outargs) when (starts_with relname "minus_") ->
      []

    | FAtom(modname, relname, outargs) ->
      (filter (fun t -> match t with | TConst(_) -> false | _ -> true)
              (flatten (mapi (Communication.decls_expand_fields p modname relname on_context) outargs)))
    | _ -> failwith ("safe_clause: "^(string_of_formula cl.head))) in

      printf "checking safety of clause: %s%!" (string_of_clause cl);
      printf "MUST BE SAFE: %s\n\n%!" (String.concat ", " (map (string_of_term ~verbose:Verbose) must_be_safe));
      must_be_safe;;

let safe_clause (p: flowlog_program) (cl: clause): unit =
  (* Every new-packet field that is mentioned (new.x) must be positively constrained somewhere in the clause.
       (Perhaps a chain of intermediate variables will be used.)
     This DOES NOT HOLD for in-packet fields and intermediate variables.
     This must be checked for every rule:
       - insert/delete must be safe for every var in their tuple
       - non-forward must be safe for *every* component of their out
       - forward must be safe for every component of their out that appears *)

      (* STEP 1: Discover what terms need to be proven safe. *)
      let must_be_safe = get_terms_to_prove_safe p cl in


      (* each of these terms may be safe via a positive path of intermediate variables
         e.g. new.dlSrc = x, x = y, R(y). *)

      (* STEP 2: discover what terms are proven safe *)
      let proven_safe = (get_safe_terms cl.body) in
        (* STEP 3: are any terms that need to be proven safe, unsafe? *)
        printf "PROVEN SAFE: %s\n%!" (String.concat ", " (map (string_of_term ~verbose:Verbose) proven_safe));
        iter (fun toprove ->
          if (not (mem toprove proven_safe) &&
             (match toprove with
                | TField(x, f) ->
                  (* Also: new.locpt can appear however (TODO: not entirely sound re: xsb) *)
                  (f <> "locpt")
                | _ -> true)) then
          begin
            printf "UNSAFE TERM: %s\n%!" (string_of_term toprove);
            raise (SyntaxUnsafe(toprove))
          end)
             must_be_safe;;

let well_formed_rule (p: flowlog_program) (r: srule): unit =

    (* This may be called for a term in the head OR in the body.*)
    let well_formed_term (headrelname: string) (headterms: term list) (inrelname: string) (inargname: string) (t: term): unit =
      match t with
      | TConst(cval) -> () (* constant is always OK *)

      | TVar(vname) ->
        if vname = inargname then
          raise (NonCondensedNoField(vname))

        (* variable name in input relation *)
      | TField(vname, fname) when vname = inargname ->
        (try
          let valid_fields = get_valid_fields_for_input_rel p inrelname in
          if not (mem fname valid_fields) then
            raise (UndeclaredField(vname, fname))
        with | Not_found -> raise (UndeclaredIncomingRelation inrelname))


(* @@@ TODO parser: head of DO must be unary*)

        (* the variable here is in the clause head. e.g. "newpkt"
           if this is a DO rule and the term is a field var... deal with similar to above.
           if this is an insert/delete rule, disallow non = inargname fields.  *)
      | TField(vname, fname) when mem vname (field_vars_in headterms) ->
        (match r.action with
          | ADo(_, outrelterms, where) ->
            (try
              (* forward is a special case: it has the type of its trigger. *)
              let valid_fields = (match (get_outgoing p headrelname).outarity with
                                   | FixedEvent(evname) -> get_valid_fields_for_input_rel p evname
                                   | AnyFields -> [fname] (* any should work *)
                                   | SameAsOnFields -> get_valid_fields_for_input_rel p inrelname) in
              if not (mem fname valid_fields) then
                raise (UndeclaredField(vname, fname))
            with | Not_found -> raise (UndeclaredOutgoingRelation headrelname))
            (* TODO: kind of a tangle here. *)
          | AForward(pkt, _, _)
          | AStash(pkt, _, _, _) ->
            let valid_fields = get_valid_fields_for_input_rel p inrelname in
              if not (mem fname valid_fields) then
                raise (UndeclaredField(vname, fname))
          | AInsert(_, outrelterms, where)
          | ADelete(_, outrelterms, where) ->
            raise (UndeclaredField(vname, fname)))

      (* any other field... must be undeclared *)
      | TField(vname, fname) ->
          raise (UndeclaredField(vname,fname))
      in

  let well_formed_atom (headrelname: string) (headterms: term list) (inrelname: string) (inargname: string) (atom: formula) :unit =
    match atom with
      | FAtom(modname, relname, argtl) when Flowlog_Builtins.is_built_in relname ->
        let bip = Flowlog_Builtins.get_built_in relname in
        if (length argtl) <> (length bip.biparity) then
          raise (BadBuiltInUse relname);
        iter (well_formed_term headrelname headterms inrelname inargname) argtl;

      | FAtom(modname, relname, argtl) ->
        (try
          if (length (get_table p relname).tablearity) <> length argtl then
            raise (BadArityOfTable relname);
            iter (well_formed_term headrelname headterms inrelname inargname) argtl;
         with | Not_found -> raise (UndeclaredTable relname))

      | FEquals(t1, t2) ->
        well_formed_term headrelname headterms inrelname inargname t1;
        well_formed_term headrelname headterms inrelname inargname t2;

      | FIn(t, addr, mask) ->
        well_formed_term headrelname headterms inrelname inargname t;
        well_formed_term headrelname headterms inrelname inargname addr;
        well_formed_term headrelname headterms inrelname inargname mask;
        (* *)

      | _ -> failwith "validate_rule" in

  (* regardless whether this rule is DO or INSERT etc. check these: *)
  let validate_common_elements inrelname inrelarg outrelname outrelterms where: unit =
    iter (well_formed_atom outrelname outrelterms inrelname inrelarg) (get_atoms where);
    iter (fun (_, f) -> (well_formed_atom outrelname outrelterms inrelname inrelarg f)) (get_equalities where);
    iter (well_formed_term outrelname outrelterms inrelname inrelarg) outrelterms;
    try
      ignore (get_event p inrelname)
    with
      | Not_found -> raise (UndeclaredIncomingRelation inrelname)
  in

  match r.action with
    | AForward(p, fmla, tout) ->
      validate_common_elements r.onrel r.onvar "forward" [p] fmla;
    | AStash(p, where, until, thens) ->
      validate_common_elements r.onrel r.onvar "stash" [p] where;
      validate_common_elements r.onrel r.onvar "stash" [p] until;
    (* DO must have outgoing relation in action, and must be correct arity *)
    |  ADo(outrelname, outrelterms, where) ->
        validate_common_elements r.onrel r.onvar outrelname outrelterms where;
        (try
          match (get_outgoing p outrelname).outarity with
            | AnyFields -> ()
            | _ when (length outrelterms) <> 1 ->
                raise (BadArityOfTable outrelname)
            | _ -> ()
        with Not_found -> raise (UndeclaredOutgoingRelation outrelname))

    (* insert and delete must have local table in action, of correct arity *)
    | AInsert(relname, outrelterms, where)
    | ADelete(relname, outrelterms, where) ->
        validate_common_elements r.onrel r.onvar relname outrelterms where;
        (try
          let dargs = (get_table p relname).tablearity in
            if (length outrelterms) <> (length dargs) then
              raise (BadArityOfTable relname)
        with Not_found -> raise (UndeclaredTable relname));;

let simplify_clause (cl: clause): clause =
    {head = cl.head; orig_rule = cl.orig_rule; body = minimize_variables cl.body};;

let well_formed_reacts (reacts: sreactive list): unit =
  ignore (fold_left (fun acc react ->
    match react with
      | ReactOut(relname, _, _)
      | ReactInc(_, relname) ->
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

(* There is some information overlap between declarations and reactive definitions.
   They are now purely syntax-level constructs, though. Leaving them in in case we
   really want to expand what reactive defns can do. *)

let reacts_added_by_sugar (stmts: stmt list): sreactive list =
  (* If we have an event decl X, return an incoming reactive defn X if X is undeclared. *)
  let inc_events_with_react = filter_map (function SReactive(ReactInc(evname,_)) -> Some evname | _ -> None) stmts in
  let event_decls = filter_map (function SDecl(DeclEvent(_,_) as d) -> Some d     | _ -> None) stmts in
  fold_left (fun acc decl -> match decl with
                      | DeclEvent(ename, _) when not (mem ename inc_events_with_react) ->
                          ReactInc(ename, ename)::acc
                      | DeclEvent(_,_) -> acc
                      | _ -> failwith "reacts_added_by_sugar") [] event_decls;;

let decls_added_by_sugar (stmts: stmt list): sdecl list =
  (* If we have an event decl X, return an incoming decl X if X is undeclared. *)
  let inc_events_with_decl = filter_map (function SDecl(DeclInc(_,evname)) -> Some evname | _ -> None) stmts in
  let event_decls = filter_map (function SDecl(DeclEvent(_,_) as d) -> Some d     | _ -> None) stmts in
  let incsfromevents = fold_left (fun acc decl -> match decl with
                        | DeclEvent(ename, _) when not (mem ename inc_events_with_decl) ->
                          DeclInc(ename, ename)::acc
                        | _ -> acc) [] event_decls in
  (* If we have an outgoing reactive defn, make an outgoing decl if undeclared in program. *)
  let out_with_decl = filter_map (function SDecl(DeclOut(rname,_)) -> Some rname | _ -> None) stmts in
  let out_decls_needed = filter_map (function SReactive(ReactOut(outrel, outf, spec)) when not (mem outrel out_with_decl) -> Some(DeclOut(outrel, outf)) | _ -> None) stmts in
  (* If we have a remote-table reactive defn, make a remote-table decl if needed... *)
  let remote_with_decl = filter_map (function SDecl(DeclRemoteTable(rname,_)) -> Some rname | _ -> None) stmts in
  let remote_decls_needed = filter_map (function SReactive(ReactRemote(tblname, argtypes, _,_,_,_)) when not (mem tblname remote_with_decl) -> Some(DeclRemoteTable(tblname, argtypes)) | _ -> None) stmts in
    remote_decls_needed @ out_decls_needed @ incsfromevents;;

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
    let includes = fold_left uniq l_ast.includes r_ast.includes in
    {includes=includes; statements= l_ast.statements @ r_ast.statements} in

  fold_left merge_ast {includes=[]; statements=[]} asts

(*
 * takes an AST and a list of previously included files, returns a new AST
 * where the included files have been turned into ASTs, appended to the given
 * AST, with their own included files recurisvely expanded.
 *
 * the list of previously included files is needed to ensure that we don't
 * include a file more than once
 *)

let rec expand_includes (ast : flowlog_ast) (prev_includes : string list) : flowlog_ast =
    if length ast.includes = 0 then ast
    else
      let unquote = fun qfn -> String.sub qfn 1 ((String.length qfn) - 2) in
      let includes = map unquote ast.includes in
      let maybe_read_ast filename = if mem filename prev_includes
                                    then {includes=[]; statements=[]}
                                    else read_ast filename in

      let flattened_ast = flatten_asts (map maybe_read_ast includes) in

        let new_ast = {includes=flattened_ast.includes; statements=ast.statements @ flattened_ast.statements} in
          expand_includes new_ast (prev_includes @ includes)

(* some duplication here from Flowlog_Graphs for now. *)
  let build_memos_for_program (rules: srule list) (tables: table_def list) (outgoings: outgoing_def list) (events: event_def list): program_memos =
    let memos = {out_triggers = Hashtbl.create 5; insert_triggers = Hashtbl.create 5;
                 delete_triggers = Hashtbl.create 5;
                 tablemap = Hashtbl.create 5; eventmap = Hashtbl.create 5;
                 outgoingmap = Hashtbl.create 5;
                 } in
    let depends_from_rule (r: srule): unit =
      match r.action with
        | AInsert(headrel, _, fmla) ->
          Hashtbl.add memos.insert_triggers r.onrel headrel
        | ADelete(headrel, _, fmla) ->
          Hashtbl.add memos.delete_triggers r.onrel headrel
        | ADo(headrel, _, fmla) ->
          Hashtbl.add memos.out_triggers r.onrel headrel
        | AForward(p, fmla, tout) ->
          Hashtbl.add memos.out_triggers r.onrel "forward"
        | AStash(p, where, until, thens) ->
          Hashtbl.add memos.out_triggers r.onrel "stash" in
     iter depends_from_rule rules;
     iter (fun tdef -> Hashtbl.add memos.tablemap tdef.tablename tdef) tables;
     iter (fun odef -> Hashtbl.add memos.outgoingmap odef.outname odef) outgoings;
     iter (fun edef -> Hashtbl.add memos.eventmap edef.eventname edef) events;
     memos;;

let make_tables (decls: sdecl list) (defns: sreactive list): table_def list =
  filter_map (function | DeclTable(n, tlist) -> Some {tablename=n; tablearity=tlist; source=LocalTable}
                       | DeclRemoteTable(n, tlist) ->
                          (match (find (function | ReactRemote(tname,_,_,_,_,_) when tname = n -> true | _ -> false ) defns) with
                            | ReactRemote(_, argtypes, qid, ipaddr, tcpport, refreshsetting) ->
                              Some {tablename=n; tablearity=tlist; source=RemoteTable(qid, (ipaddr, tcpport), refreshsetting)}
                            | _ -> failwith "make_tables")
                       | _ -> None) decls;;

let make_events (decls: sdecl list) (defns: sreactive list): event_def list =
  filter_map (function | DeclEvent(n, tlist) -> Some {eventname=n; evfields=tlist}
                       | _ -> None) decls;;

(* All declared outgoings have fixed fields. Non-fixed are all built in*)
let make_outgoings (decls: sdecl list) (defns: sreactive list): outgoing_def list =
  filter_map (function | DeclOut(dname, outarity) ->
                          (match (find (function | ReactOut(tname,_,_) when tname = dname -> true | _ -> false ) defns) with
                            | ReactOut(_, _, outspec) ->
                              Some {outname=dname; outarity=outarity; react=outspec}
                            | _ -> failwith "make_tables")
                       | _ -> None) decls;;

let desugared_program_of_ast (ast: flowlog_ast) (filename : string): flowlog_program =
  let expanded_ast = expand_includes ast [filename] in
  let stmts = expanded_ast.statements in
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

            (* Validation *)
            well_formed_reacts the_reacts;
            well_formed_decls the_decls;

            (* Create and simplify clauses. Test for what can be compiled. Weaken as needed. *)
            let clauses = (fold_left (fun acc r -> (clauses_of_rule r) @ acc) [] the_rules) in
            let simplified_clauses = map simplify_clause clauses in
            (* pre-determine what can be fully compiled. pre-determine weakened versions of other packet-triggered clauses*)
            let can_fully_compile_simplified, weakened_cannot_compile_pt_clauses, not_fully_compiled_clauses =
              fold_left (fun (acc_comp, acc_weaken, acc_unweakened) cl ->
                                   let (v, t) = trim_packet_from_body cl.body in
                                     if v = "" then (* not packet-triggered *)
                                       (acc_comp, acc_weaken, cl::acc_unweakened)
                                     else
                                     begin
                                      let (newcl, fully_compiled) = validate_and_process_pkt_triggered_clause cl in

                                        (* fully compilable *)
                                        if fully_compiled then
                                           ({oldpkt=v; clause={head = cl.head; orig_rule = cl.orig_rule; body = t}} :: acc_comp, acc_weaken, acc_unweakened)

                                        (* weakened; needs storing for both compiler and XSB. weakened version should have its trigger removed *)
                                        else
                                         (acc_comp, {oldpkt=v; clause=newcl} :: acc_weaken, cl::acc_unweakened)

                                     end)
                          ([],[],[]) simplified_clauses in

              printf "\n  Loaded AST. There were %d clauses, \n    %d of which were fully compilable forwarding clauses and\n    %d were weakened pkt-triggered clauses.\n    %d will be given, unweakened, to XSB.\n%!"
                (length simplified_clauses)
                (length can_fully_compile_simplified)
                (length weakened_cannot_compile_pt_clauses)
                (length not_fully_compiled_clauses);

              printf "DECLS: %s\n%!" (String.concat ",\n"(map string_of_declaration the_decls));
              printf "REACTS: %s\n%!" (String.concat ",\n"(map string_of_reactive the_reacts));

                (* Convert decls and defns syntax (with built ins) into a program *)
                let the_tables = make_tables the_decls the_reacts in
                let the_outgoings = make_outgoings the_decls the_reacts in
                let the_events = make_events the_decls the_reacts in
                let p = {
                 desugared_decls = the_decls;
                 desugared_reacts = the_reacts;
                 tables = the_tables;
                 outgoings = the_outgoings;
                 events = the_events;
                 clauses = simplified_clauses;
                 weakened_cannot_compile_pt_clauses = weakened_cannot_compile_pt_clauses;
                 can_fully_compile_to_fwd_clauses = can_fully_compile_simplified;
                 (* Remember: these are unweakened, and so can be used by XSB. *)
                 not_fully_compiled_clauses = not_fully_compiled_clauses;
                 memos = build_memos_for_program the_rules the_tables the_outgoings the_events} in

                  (* Validation *)
                  iter (well_formed_rule p) the_rules;
                  iter (safe_clause p) simplified_clauses;
                  p;;