open Printf
open ExtList.List

  type term = 
              | TConst of string 
              | TVar of string 
              | TField of string * string;;  

  type formula = 
              | FTrue 
              | FFalse 
              | FEquals of term * term
              | FNot of formula 
                (* module, relname, args*)
              | FAtom of string * string * term list 
              | FAnd of formula * formula 
              | FOr of formula * formula;;

  type action = 
              | ADelete of string * term list * formula 
              | AInsert of string * term list * formula 
              | ADo of string * term list * formula;;

  type refresh = 
      (* number, units *)
      | RefreshTimeout of int * string
      | RefreshPure
      | RefreshEvery;;

  type assignment = 
      | Assign of string * string;;

  type spec_out = 
      | ReactSend of string * assignment list * string * string;;
  type spec_in =
      | ReactInsert of string;;

  type sreactive = 
        (* table name, query name, ip, port, refresh settings *)
      | ReactRemote of string * string * string * string * refresh
        (* out relation name, args, event type name, assignments, ip, port*)
      | ReactOut of string * string list * string * assignment list * string * string 
        (* incoming event type, trigger relation name*)
      | ReactInc of string * string;;

  type sdecl = 
      | DeclTable of string * string list    
      | DeclRemoteTable of string * string list    
      | DeclInc of string * string   
      | DeclOut of string * string list    
      | DeclEvent of string * string list;;

  type srule = 
      | Rule of string * string * action;;

  type stmt = 
      | SReactive of sreactive 
      | SDecl of sdecl 
      | SRule of srule;;

  type flowlog_ast = 
      | AST of string list * stmt list;;

(*************************************************************)  

  (* Split out a formula by ORs for use with XSB *)
  (* In new semantics, no longer have "on" in clause body. It just gets made an EDB fact. *)
  (* REQUIRE: head, body fmlas atomic or equality *)
  type clause = { orig_rule: srule; 
                  head: formula;
                  body: formula; (* should be always conjunctive *)
                  };;

  type flowlog_program = {  decls: sdecl list; 
                            reacts: sreactive list; 
                            clauses: clause list; };;
(*************************************************************)

  let string_of_term (t: term) : string = 
    match t with
      | TConst(s) -> "TConst"^s^")" 
      | TVar(s) -> "TVar("^s^")"
      | TField(varname, fname) -> "TField("^varname^"."^fname^")" ;;

  let rec string_of_formula (f: formula) : string = 
    match f with
      | FTrue -> "true"
      | FFalse -> "false"
      | FEquals(t1, t2) -> (string_of_term t1) ^ " or "^ (string_of_term t2)
      | FNot(f) ->  "(not "^(string_of_formula f)^")"
      | FAtom("", relname, tlargs) -> 
          relname^"("^(String.concat "," (List.map string_of_term tlargs))^")"
      | FAtom(modname, relname, tlargs) -> 
          modname^"."^relname^"("^(String.concat "," (List.map string_of_term tlargs))^")"
      | FAnd(f1, f2) -> (string_of_formula f1) ^ " and "^ (string_of_formula f2)
      | FOr(f1, f2) -> (string_of_formula f1) ^ " or "^ (string_of_formula f2)
  
  let action_string outrel argterms fmla: string = 
    let argstring = (String.concat "," (List.map string_of_term argterms)) in
      outrel^"("^argstring^") WHERE "^(string_of_formula fmla);;

  let string_of_rule (r: srule): string =
    match r with 
      | Rule(trigrel, trigvar, act) -> 
        match act with 
          | ADelete(outrel, argterms, fmla) ->  
            "ON "^trigrel^"("^trigvar^"): DELETE "^(action_string outrel argterms fmla);                         
          | AInsert(outrel, argterms, fmla) ->
            "ON "^trigrel^"("^trigvar^"): INSERT "^(action_string outrel argterms fmla);
          | ADo(outrel, argterms, fmla) ->  
            "ON "^trigrel^"("^trigvar^"): DO "^(action_string outrel argterms fmla);;

  let string_of_declaration (d: sdecl): string =
    match d with 
      | DeclTable(tname, argtypes) -> "TABLE "^tname^(String.concat "," argtypes);
      | DeclRemoteTable(tname, argtypes) -> "REMOTE TABLE "^tname^" "^(String.concat "," argtypes);
      | DeclInc(tname, argtype) -> "INCOMING "^tname^" "^argtype;
      | DeclOut(tname, argtypes) -> "OUTGOING "^tname^(String.concat "," argtypes);
      | DeclEvent(evname, argnames) -> "EVENT "^evname^" "^(String.concat "," argnames);;

  let string_of_reactive (r: sreactive): string =
    match r with       
      | ReactRemote(tblname, qname, ip, port, refresh) ->
        tblname^" (remote) = "^qname^" @ "^ip^" "^port;
      | ReactOut(outrel, args, evtype, assignments, ip, port) ->
        outrel^"("^(String.concat "," args)^") (output rel) = "^evtype^" @ "^ip^" "^port;
      | ReactInc(evtype, relname) -> 
        relname^" (input rel) "^evtype;;
  
  let string_of_stmt (stmt: stmt): string = 
    match stmt with 
      | SReactive(rstmt) -> (string_of_reactive rstmt);
      | SDecl(dstmt) -> (string_of_declaration dstmt);
      | SRule(rstmt) -> (string_of_rule rstmt);;

  let pretty_print_program (ast: flowlog_ast): unit =
    match ast with
      | AST(imports, stmts) ->
        List.iter (fun imp -> printf "IMPORT %s;\n%!" imp) imports;
        List.iter (fun stmt -> printf "%s\n%!" (string_of_stmt stmt)) stmts;;

  let string_of_clause (cl: clause): string =
    "CLAUSE: "^(string_of_formula cl.head)^" :- "^(string_of_formula cl.body)^"\n"^
    "FROM RULE: "^(string_of_rule cl.orig_rule);;

(*************************************************************)

let product_of_lists lst1 lst2 = 
  List.concat (List.map (fun e1 -> List.map (fun e2 -> (e1,e2)) lst2) lst1);;

(* all disjunctions need to be pulled to top already *)
let rec extract_disj_list (f: formula): formula list =    
    match f with 
        | FOr(f1, f2) -> (extract_disj_list f1) @ (extract_disj_list f2);
        | _ -> [f];;

let rec disj_to_top (f: formula): formula = 
    match f with 
        | FTrue -> f;
        | FFalse -> f;
        | FEquals(t1, t2) -> f;
        | FAtom(modstr, relstr, argterms) -> f;
        | FOr(f1, f2) -> f;
        | FNot(f2) -> 
            (* De-Morgan's law if necessary *)
            let f2ds = extract_disj_list (disj_to_top f2) in
                if (length f2ds) < 2 then f
                (* start w/ first disj. don't cheat w/ FTrue.
                Note: (FNot(hd f2ds)) not FNot(hd f2ds) *)
                else fold_left (fun acc subf -> FAnd(FNot(subf), acc)) (FNot(hd f2ds)) (tl f2ds)                
        | FAnd(f1, f2) -> 
            (* Distributive law if necessary *)
            let f1ds = extract_disj_list (disj_to_top f1) in
            let f2ds = extract_disj_list (disj_to_top f2) in
            let pairs = product_of_lists f1ds f2ds in
                (* again, start with first pair, not FFalse *)
                let (firstfmla1, firstfmla2) = (hd pairs) in
                fold_left (fun acc (subf1, subf2) -> FOr(FAnd(subf1, subf2), acc)) (FAnd(firstfmla1, firstfmla2)) (tl pairs);;
        
