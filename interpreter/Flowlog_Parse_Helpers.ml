(****************************************************************)
(* Helpers for loading ASTs, desugaring, validating input, etc. *)
(****************************************************************)

open Flowlog_Types
open Flowlog_Packets
open Flowlog_Helpers
open Flowlog_Safety
open Flowlog_Builtins
open Surface_Parser
open Surface_Lexer
open Printf
open ExtList.List
open Partial_Eval_Validation
open Xsb_Communication

exception SyntaxAnyInPlus of formula;;
exception HexConstantSurvived of string;;
exception SyntaxUnsafe of term;;

(*ON switch_port(swpt):
//  INSERT (swpt.sw, swpt.pt) INTO switch_has_port;

//ON switch_down(swd):
//  DELETE (swd.sw, ANY) FROM switch_has_port;
//  DELETE (swd.sw, ANY, ANY) FROM learned;*)

(* Auto-included AST for switch_has_port *)
let switch_has_port_ast = {includes=[]; statements=
  [ASTDecl(ASTDeclTable(switch_has_port_relname, ["switchid"; "portid"]));
   ASTRule({onrel="switch_port"; onvar="sp";
            action=AInsert(switch_has_port_relname, [TField("sp", "sw"); TField("sp", "pt")], FTrue) });
   ASTRule({onrel="switch_down"; onvar="sd";
            action=ADelete(switch_has_port_relname, [TField("sd", "sw"); TVar("ANY0")], FTrue) })]};;

let built_in_asts = [switch_has_port_ast];;
let auto_tables = [switch_has_port_relname];;


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
    (*printf "Trying to open %s\n%!" filename;*)
    let lexbuf = from_case_insensitive_channel (open_in filename) in
    try
      let result = Surface_Parser.main Surface_Lexer.token lexbuf in
        if !global_verbose > 4 then
        begin
          printf "Done parsing. Resulting AST: \n%!";
          pretty_print_ast result;
        end;
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

(* Type mismatch warnings.
   I'd rather throw an error outright for a type-mismatch, but since many of these
   atomic values are "just numbers" (of a given bitwidth) I'm leery of being too draconian, here.

   negated use will throw the warning, even though it's technically OK. (e.g. p.locPt != new.locSw)
 *)

let check_clause_types (p: flowlog_program) (cl: clause): unit =
  let inferences = type_inference_of_vars p TermMap.empty cl in

  let check_multi_inferences k vs =
    if TypeIdSet.cardinal vs > 1 then
    begin
      printf "--------------------------------------------------\n";
      printf "    TYPE WARNING: \n%!";
      printf "    IN CLAUSE: %s\n" (string_of_clause cl);
      printf "    TERM %s was inferred to have multiple types:\n" (string_of_term k);
      TypeIdSet.iter (fun s -> printf "%s " s) vs;
      printf "\n\n%!"
    end in

  (* Any time multiple types are inferred, toss a warning: *)
  TermMap.iter check_multi_inferences inferences;;

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
    if !global_verbose > 4 then
      printf "producing clause for rule %s\n\n%!" (string_of_rule r);

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

      if !global_verbose > 4 then
      begin
        printf "checking safety of clause: %s%!" (string_of_clause cl);
        printf "MUST BE SAFE: %s\n\n%!" (String.concat ", " (map (string_of_term ~verbose:Verbose) must_be_safe));
      end;

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
      let proven_safe = (get_safe_terms p (Some cl) cl.body) in
        (* STEP 3: are any terms that need to be proven safe, unsafe? *)
        if !global_verbose > 4 then
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
    let well_formed_term ?(inhead: bool = false) (headrelname: string) (headterms: term list) (inrelname: string) (inargname: string) (t: term): unit =
      match t with
      | TConst(cval) when (starts_with cval "0x") ->
        (* kludge guard to deal with fact that values inside TConsts are strings, not integers.*)
        raise (HexConstantSurvived cval)
      | TConst(cval) -> () (* constant is always OK *)

      | TVar(vname) ->
        if inhead && vname = inargname && headrelname = "forward" then
          raise (ReqDifferentOutputVar(vname))
        else if (not inhead) && vname = inargname then
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
    iter (well_formed_term ~inhead:true outrelname outrelterms inrelname inrelarg) outrelterms;
    try
      (* Make sure this input relation is declared: go through reactives list *)
      ignore (get_event p (input_rel_to_eventname p inrelname))
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
  if !global_verbose > 4 then
    printf "simplifying clause: %s\n%!" (string_of_clause cl);
  {head = cl.head; orig_rule = cl.orig_rule; body = minimize_variables cl.body};;

let well_formed_reacts (reacts: sreactive list): unit =
  ignore (fold_left (fun acc react ->
    match react with
      | ReactOut(relname, _, _)
      | ReactInc(_, _, relname) ->
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
        if (is_built_in relname) || ((mem relname acc) && (mem relname auto_tables)) then
          raise (RelationDeclClashesWithBuiltin(relname))
        else if mem relname acc then
          raise (RelationHadMultipleDecls(relname))
        else relname::acc
      | _ -> acc) [] decls);;

(* There is some information overlap between declarations and reactive definitions.
   They are now purely syntax-level constructs, though. Leaving them in in case we
   really want to expand what reactive defns can do. *)

let reacts_added_by_sugar (stmts: stmt list): sreactive list =
  (* If we have an event decl X, return an incoming reactive defn X if X is undeclared.
     This sugared reactive assumes events coming from THRIFT. *)
  let inc_events_with_react = filter_map (function SReactive(ReactInc(evname,_,_)) -> Some evname | _ -> None) stmts in
  let event_decls = filter_map (function SDecl(DeclEvent(_,_) as d) -> Some d     | _ -> None) stmts in
  fold_left (fun acc decl -> match decl with
                      | DeclEvent(ename, _) when not (mem ename inc_events_with_react) ->
                          ReactInc(ename, IncThrift, ename)::acc
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

(* Types of incoming and outgoing packets assert constraints on the clauses. For instance,
   ON ip_packet(p) means that we need to add a WHERE p.dlTyp = 0x800.
   and
   emit_tcp(new) means we need to add WHERE new.dlTyp = 0x800 AND new.nwProto = 6 *)
let add_packet_type_constraints (r: srule): srule =
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
 *
 * Also add built in ASTs! (Use "" to represent built-ins)
 *)

let rec expand_includes (ast : flowlog_ast) (prev_includes : string list) : flowlog_ast =
    let maybe_built_in_asts = if mem "" prev_includes then [] else built_in_asts in

    if length ast.includes = 0 then (flatten_asts (maybe_built_in_asts@[ast]))
    else
      let unquote = fun qfn -> String.sub qfn 1 ((String.length qfn) - 2) in
      let includes = (map unquote ast.includes) in
      let maybe_read_ast filename = if mem filename prev_includes
                                    then {includes=[]; statements=[]}
                                    else read_ast filename in

      (* built_ins will be added on the first leaf; this is not a leaf *)
      let flattened_ast = flatten_asts (map maybe_read_ast includes) in

        let new_ast = {includes=flattened_ast.includes; statements=ast.statements @ flattened_ast.statements} in
          expand_includes new_ast (prev_includes @ includes)

(* some duplication here from Flowlog_Graphs for now. *)
  let build_memos_for_program (reacts: sreactive list) (rules: srule list) (tables: table_def list) (outgoings: outgoing_def list) (events: event_def list) (simp_clauses: clause list): program_memos =
    let fmlas = map (fun cl -> cl.body) simp_clauses in
    let memos = {out_triggers = Hashtbl.create 5; insert_triggers = Hashtbl.create 5;
                 delete_triggers = Hashtbl.create 5;
                 tablemap = Hashtbl.create 5; eventmap = Hashtbl.create 5;
                 outgoingmap = Hashtbl.create 5;
                 incomingmap = Hashtbl.create 5;
                 atoms_used_in_bodies = unique (fold_left (fun acc f -> (get_atoms f) @ acc) [] fmlas);
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
      iter (function | ReactInc(tid, src, relname) -> Hashtbl.add memos.incomingmap (tid, src) relname | _ -> ()) reacts;
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

let rec fmla_necessary_false ?(sign: bool = true) (f: formula): bool =
  match f with
    | FFalse when sign -> true
    | FTrue when not sign -> true
    | FTrue -> false
    | FFalse -> false
    | FAtom(_,_,_) -> false
    | FEquals(t1, t2) when not sign && t1 = t2 -> true
    | FEquals(t1, t2) -> false
    | FIn(_,_,_) -> false
    | FNot(f2) -> fmla_necessary_false ~sign:(not sign) f2
    | FAnd(f2, f3) -> fmla_necessary_false ~sign:sign f2 || fmla_necessary_false ~sign:sign f3
    | FOr(f2, f3) -> fmla_necessary_false ~sign:sign f2 && fmla_necessary_false ~sign:sign f3;;

let rule_condition_false (r: srule): bool =
  match r.action with
  | ADelete(_, _, f) -> fmla_necessary_false f
  | AInsert(_, _, f) -> fmla_necessary_false f
  | ADo(_, _, f) -> fmla_necessary_false f
  | AStash(_, _, f, _) -> fmla_necessary_false f
  | AForward(_, f, _) -> fmla_necessary_false f;;

let desugar_statements (aststmts: aststmt list): stmt list =
  fold_left (fun (acc: stmt list) (astmt: aststmt) ->
      match astmt with
        | ASTReactive(s) -> SReactive(s)::acc
        | ASTRule(s) -> SRule(s)::acc
        | ASTDecl(ASTDeclTable(tname, types)) -> SDecl(DeclTable(tname, types))::acc
        | ASTDecl(ASTDeclRemoteTable(tname, types)) -> SDecl(DeclRemoteTable(tname, types))::acc
        | ASTDecl(ASTDeclInc(s1, s2)) -> SDecl(DeclInc(s1, s2))::acc
        | ASTDecl(ASTDeclOut(s1, flds)) -> SDecl(DeclOut(s1, flds))::acc
        | ASTDecl(ASTDeclEvent(s1, flds)) -> SDecl(DeclEvent(s1, flds))::acc
          (* only desugar that adds, for now *)
            (* todo: constrain to contain one thing at a time *)
        | ASTDecl(ASTDeclVar(vname, atype, def)) ->
          (match def with
            | None -> SDecl(DeclTable(vname, [atype])) :: acc
            | Some(d) ->
              [SDecl(DeclTable(vname, [atype]));
               SRule({onrel = "startup"; onvar = "e"; action = AInsert(vname, [d], FTrue)})]
              @ acc))
   []
   aststmts;;

(* what variables appear? are they VAR names? then desugar.
   more than just rule body: may have +R(v) :- true. if v is a varvar. *)
let desugar_rule (r: srule) (vartblnames: string list): srule =
  let varvarsused = get_terms_in_rule_head_and_body
                     (function | TVar(x) when mem x vartblnames -> true | _ -> false)
                     r in
    let newact = fold_left (fun acc varvar ->
        (match varvar with
          | TVar(x) ->
            add_conjunct_to_action acc (FAtom("", x, [varvar]))
          | _ -> failwith ("desugar_rule:"^(string_of_term varvar))))
      r.action
      varvarsused in
    if !global_verbose > 4 then
    begin
      printf "old rule: %s\n%!" (string_of_rule r);
      printf "var table names: %s\nused: %s\n%!" (String.concat "," vartblnames) (string_of_list ";" string_of_term varvarsused);
      printf "new rule: %s\n\n%!" (string_of_rule {r with action=newact});
    end;
    {r with action=newact};;

(* If new.locPt appears in a FORWARD rule's WHERE outside of "new.locPt != p.locPt"
   then add switch_has_port(p.locSw, new.locPt) to make new.locPt safe. *)
let add_shp_if_needed (r: srule): srule =
  (* srule type: {onrel: string; onvar: string; action: action}*)
  match r.action with
    | AForward(TVar(newp) as newpvar, fmla, tout) ->
      (* Does TField(newp, "locpt") appear anywhere but in a flood equality?
         Is shp already asserted? *)
      let inswitch = TField(r.onvar, "locsw") in
      let nptterm = TField(newp, "locpt") in
      let optterm = TField(r.onvar, "locpt") in

      let signed_atoms = (get_atoms_with_sign fmla) in
      let has_shp_already = exists (fun (s, a) -> match a with
        | FAtom("", "switch_has_port", [t1;t2])
          when s && t1 = inswitch && t2 = nptterm -> true
        | _ -> false) signed_atoms in

      let signed_eqs = (get_equalities fmla) in

      let has_other_newpt = exists (fun (s, a) ->
          match a with
            | FAtom(_, _, args) when mem nptterm args -> true
            | FEquals(t1, t2) when
              (* A positive eq involving new.locPt, OR a negative involving new.locPt that isn't the flood atom. *)
              (s && (t1 = nptterm || t2 = nptterm)) ||
              ((not s) && (t1 = nptterm && t2 <> optterm)) ||
              ((not s) && (t2 = nptterm && t1 <> optterm)) -> true
            | _ -> false) (signed_atoms@signed_eqs) in

      (* Add SHP if not already present, and contains non-flood newpt reference. *)
      if (not has_shp_already) && has_other_newpt then
      begin
        (* positive SHP must go before other atoms. *)
        let newaction = AForward(newpvar, FAnd(FAtom("", "switch_has_port", [inswitch;nptterm]), fmla), tout) in
          printf "~~~ ADDING SHP: %s\n%!" (string_of_action newaction);
          {onrel=r.onrel;onvar=r.onvar;action=newaction}
      end
      else
      begin
        printf "~~~ Not adding SHP (hon=%b, hsa=%b): %s\n%!" has_other_newpt has_shp_already (string_of_action r.action);
        r
      end
    | _ -> r;;


(* For INSERT and DELETE rules, help out the proactive compiler by adding a
   "if something will even change..." subformula to the WHERE clause. For instance,
   INSERT (p.dlSrc) INTO seen;  would become INSERT (p.dlSrc) INTO seen WHERE NOT seen(p.dlSrc).
  IMPORTANT: These extras must NOT make it into XSB, however, as they can mess up our semantics if so.
  Consider replacing the value of T1 with the value of T2. We want to insert everything
  in T2, even if it's in T1, because we're deleting everything in T1 in the same cycle! *)
let add_id_protection_cl (tcl: triggered_clause): triggered_clause =
  let r = tcl.clause.orig_rule in
  let newbody = (match r.action with
    | ADelete(s, t, where) ->
      let protect = FAtom("", s, t) in
        if mem protect (conj_to_list tcl.clause.body) then tcl.clause.body
        else FAnd(tcl.clause.body, protect)

    | AInsert(s, t, where) ->
      let protect = FNot(FAtom("", s, t)) in
        if mem protect (conj_to_list tcl.clause.body) then tcl.clause.body
        else FAnd(tcl.clause.body, protect)

    | _ -> tcl.clause.body) in
      {tcl with clause = {tcl.clause with body = newbody}};;

let desugared_program_of_ast (ast: flowlog_ast) (filename : string): flowlog_program =
  let expanded_ast = expand_includes ast [filename] in
  let ast_stmts = expanded_ast.statements in
  let desugared_stmts = desugar_statements ast_stmts in
  let vartblnames = filter_map (function | ASTDecl(ASTDeclVar(tname, _, _)) -> Some(tname) | _ -> None) ast_stmts in
        (* requires extlib *)
        (* Compute de-sugared rules, declarations, and reactive declarations
           Note that in the docs, decls and reacts are squished into a single concept.*)
        let the_decls  =  built_in_decls @
                          filter_map (function | SDecl(d) -> Some d
                                               | _ -> None) desugared_stmts @
                          (decls_added_by_sugar desugared_stmts) in
        let the_reacts =  built_in_reacts @
                          filter_map (function | SReactive(r) -> Some r
                                               | _ -> None) desugared_stmts @
                          (reacts_added_by_sugar desugared_stmts) in
        let the_rules  =  filter_map (function | SRule(r) when (rule_condition_false r) ->
                                                  if !global_verbose > 0 then
                                                    write_log (sprintf "Ignoring rule in %s because its condition is always false: %s\n%!" filename (string_of_rule r));
                                                  None
                                               | SRule(r) ->

                                                (* Rule desugaring process: *)
                                                (* Where we see packet types, add filters on ethtyp, nwproto field *)
                                                let rule_with_type_constraints = (add_packet_type_constraints r) in

                                                (* Make sure forwarding behavior is strongly safe. *)
                                                let safe_if_fwd_rule = (add_shp_if_needed rule_with_type_constraints) in

                                                  (* Misc. other final desugaring, for instance dealing with VARs
                                                     and turning them into proper table refs *)
                                                  Some (desugar_rule safe_if_fwd_rule vartblnames)

                                               | _ -> None) desugared_stmts in

            (* Validation *)
            well_formed_reacts the_reacts;
            well_formed_decls the_decls;

            let the_tables = make_tables the_decls the_reacts in
            let the_outgoings = make_outgoings the_decls the_reacts in
            let the_events = make_events the_decls the_reacts in

            (* pre_program contains everything but the clauses. It's used for *)
            (* clause-processing (because validation, etc. needs to know the *)
            (* reacts, decls, ...*)
            let pre_program =
                {desugared_decls = the_decls;
                 desugared_reacts = the_reacts;
                 vartablenames = vartblnames;
                 tables = the_tables;
                 outgoings = the_outgoings;
                 events = the_events;
                 clauses = [];
                 weakened_cannot_compile_pt_clauses = [];
                 can_fully_compile_to_fwd_clauses = [];
                 not_fully_compiled_clauses = [];
                 memos = build_memos_for_program the_reacts the_rules the_tables the_outgoings the_events []} in

            (***********************************)
            (* Now turn ~RULES~ into ~CLAUSES~ *)

            (* Create and simplify clauses. Test for what can be compiled. Weaken as needed. *)
            let clauses = (fold_left (fun acc r -> (clauses_of_rule r) @ acc) [] the_rules) in
            let simplified_clauses = (map simplify_clause clauses) in

            (* pre-determine what can be fully compiled. pre-determine weakened versions of other packet-triggered clauses*)
            let can_fully_compile_simplified, weakened_cannot_compile_pt_clauses, not_fully_compiled_clauses =
              fold_left (fun (acc_comp, acc_weaken, acc_unweakened) cl ->
                                   let (inrel, v, t) = trim_packet_from_body cl.body in
                                     (* not packet-triggered or not from data-plane *)
                                     if (v = "") || (mem inrel built_in_cp_packet_input_tables) then
                                       (acc_comp, acc_weaken, cl::acc_unweakened)
                                     else
                                     (* DP-packet-triggered; may be weakened or unweakened *)
                                     begin
                                      let (newcl, fully_compiled) = validate_and_process_pkt_triggered_clause pre_program cl in

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

              if !global_verbose > 4 then
              begin
                printf "DECLS: %s\n%!" (String.concat ",\n"(map string_of_declaration the_decls));
                printf "REACTS: %s\n%!" (String.concat ",\n"(map string_of_reactive the_reacts));
              end;

                (* Convert decls and defns syntax (with built ins) into a program *)
                let p = {pre_program with
                 clauses = simplified_clauses; (* *without* insert/delete helpers*)

                 (* with insert/delete helpers. these fields should be used by the compiler. NEVER by XSB *)
                 weakened_cannot_compile_pt_clauses = (map add_id_protection_cl weakened_cannot_compile_pt_clauses);
                 can_fully_compile_to_fwd_clauses = (map add_id_protection_cl can_fully_compile_simplified);

                 (* These will go to XSB instead of p.clauses if "unsafe" mode is active.
                    Should NOT be used by the compiler *)
                 not_fully_compiled_clauses = not_fully_compiled_clauses;

                 (* rebuild memos *)
                 memos = build_memos_for_program pre_program.desugared_reacts the_rules the_tables the_outgoings the_events simplified_clauses} in

                  (* Validation *)
                  iter (well_formed_rule p) the_rules;
                  iter (safe_clause p) simplified_clauses;
                  iter (check_clause_types p) simplified_clauses;

                  p;;