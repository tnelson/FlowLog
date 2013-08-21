open Printf

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
              | ADelete of string * string list * formula 
              | AInsert of string * string list * formula 
              | ADo of string * string list * formula;;

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
      | FAtom(modname, relname, tlargs) -> modname^"."^relname^"("^(String.concat "," (List.map string_of_term tlargs))^")"
      | FAnd(f1, f2) -> (string_of_formula f1) ^ " and "^ (string_of_formula f2)
      | FOr(f1, f2) -> (string_of_formula f1) ^ " or "^ (string_of_formula f2)

  let string_of_rule (r: srule): string =
    match r with 
      | Rule(trigrel, trigvar, act) -> 
        match act with 
          | ADelete(outrel, argnames, fmla) ->  
            "ON "^trigrel^"("^trigvar^"): DELETE "^
             outrel^"("^(String.concat "," argnames)^") WHERE "^(string_of_formula fmla);
          | AInsert(outrel, argnames, fmla) ->
            "ON "^trigrel^"("^trigvar^"): INSERT "^
             outrel^"("^(String.concat "," argnames)^") WHERE "^(string_of_formula fmla);
          | ADo(outrel, argnames, fmla) ->  
            "ON "^trigrel^"("^trigvar^"): DO "^
            outrel^"("^(String.concat "," argnames)^") WHERE "^(string_of_formula fmla);;

  let string_of_stmt (stmt: stmt): string = 
    match stmt with 
      | SReactive(rstmt) -> "REACT";
      | SDecl(dstmt) -> "DECL";
      | SRule(rstmt) -> (string_of_rule rstmt);;

  let pretty_print_program (ast: flowlog_ast): unit =
    match ast with
      | AST(imports, stmts) ->
        List.iter (fun imp -> printf "IMPORT %s;\n%!" imp) imports;
        List.iter (fun stmt -> printf "%s\n%!" (string_of_stmt stmt)) stmts;;