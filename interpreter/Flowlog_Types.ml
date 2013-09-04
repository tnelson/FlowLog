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

  type assignment = {afield: string; atupvar: string};;      

  type spec_out = 
      | OutForward
      | OutEmit
      | OutLoopback
      | OutSend of string * string;;

  type sreactive = 
        (* table name, query name, ip, port, refresh settings *)
      | ReactRemote of string * string * string * string * refresh
        (* out relation name, args, event type name, assignments, ip, port*)
      | ReactOut of string * string list * string * assignment list * spec_out
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

  (* context for values given by decls *)
  module StringMap = Map.Make(String);;
  type event = { typeid: string; values: string StringMap.t};;

  module FmlaMap = Map.Make(struct type t = formula let compare = compare end);;

(*************************************************************)
  let string_of_event (notif: event): string =
    notif.typeid^": ["^(String.concat ";" (map (fun (k, v) -> k^":"^v) (StringMap.bindings notif.values)))^"]";;

  (* If verbose flag is not set, prepare for XSB. Otherwise, add extra info for debug. *)
  let string_of_term ?(verbose:bool = false) (t: term): string = 
    match t with
      | TConst(s) -> 
        if verbose then "TConst("^s^")" 
        else (String.lowercase s)
      | TVar(s) ->
        if verbose then "TVar("^s^")"
        else (String.uppercase s)
      | TField(varname, fname) -> 
        if verbose then "TField("^varname^"."^fname^")" 
        else (String.uppercase (varname^"__"^fname));;

  let rec string_of_formula ?(verbose:bool = false) (f: formula): string = 
    match f with
      | FTrue -> "true"
      | FFalse -> "false"
      | FEquals(t1, t2) -> (string_of_term ~verbose:verbose t1) ^ " = "^ (string_of_term ~verbose:verbose t2)
      | FNot(f) ->  "(not "^(string_of_formula ~verbose:verbose f)^")"
      | FAtom("", relname, tlargs) -> 
          relname^"("^(String.concat "," (List.map (string_of_term ~verbose:verbose) tlargs))^")"
      | FAtom(modname, relname, tlargs) -> 
          modname^"/"^relname^"("^(String.concat "," (List.map (string_of_term ~verbose:verbose) tlargs))^")"
      | FAnd(f1, f2) -> (string_of_formula ~verbose:verbose f1) ^ ", "^ (string_of_formula ~verbose:verbose f2)
      | FOr(f1, f2) -> (string_of_formula ~verbose:verbose f1) ^ " or "^ (string_of_formula ~verbose:verbose f2)
  
  let action_string outrel argterms fmla: string = 
    let argstring = (String.concat "," (List.map (string_of_term ~verbose:true) argterms)) in
      outrel^"("^argstring^") WHERE "^(string_of_formula ~verbose:true fmla);;

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

  let string_of_outspec (spec: spec_out) =
    match spec with 
      | OutForward -> "forward"      
      | OutEmit -> "emit"
      | OutLoopback -> "loopback"
      | OutSend(ip, pt) -> ip^":"^pt;;  

  let string_of_reactive (r: sreactive): string =
    match r with       
      | ReactRemote(tblname, qname, ip, port, refresh) ->
        tblname^" (remote) = "^qname^" @ "^ip^" "^port;
      | ReactOut(outrel, args, evtype, assignments, spec) ->
        outrel^"("^(String.concat "," args)^") (output rel) = "^evtype^" @ "^(string_of_outspec spec);
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

  let string_of_clause ?(verbose: bool = false) (cl: clause): string =
    "CLAUSE: "^(string_of_formula ~verbose:verbose cl.head)^" :- "^(string_of_formula ~verbose:verbose cl.body)^"\n"^
    (if verbose then "FROM RULE: "^(string_of_rule cl.orig_rule) else "");;

(*************************************************************)


(* For every non-negated equality that has one TVar in it
   that is NOT in the exempt list, produces a tuple for substitution.
   (Exempt list is so that vars in the head of a clause won't get substituted out) *)    
let rec gather_nonneg_equalities_involving_vars 
  ?(exempt: term list = []) (f: formula) (neg: bool): (term * term) list =
  match f with 
        | FTrue -> []
        | FFalse -> []
        (* Make sure to use WHEN here, not a condition after ->. Suppose exempt=[y] and y=x. Want 2nd option to apply. *)
        | FEquals((TVar(_) as thevar), t) 
          when (not neg) && (not (thevar = t)) && (not (mem thevar exempt)) -> 
            [(thevar, t)] 
        | FEquals(t, (TVar(_) as thevar)) 
          when (not neg) && (not (thevar = t)) && (not (mem thevar exempt)) -> 
            [(thevar, t)]
        | FEquals(_, _) -> []
        | FAtom(modstr, relstr, argterms) -> []
        | FOr(f1, f2) -> 
            unique ((gather_nonneg_equalities_involving_vars ~exempt:exempt f1 neg) @ 
                    (gather_nonneg_equalities_involving_vars ~exempt:exempt f2 neg))
        | FAnd(f1, f2) -> 
            unique ((gather_nonneg_equalities_involving_vars ~exempt:exempt f1 neg) @ 
                    (gather_nonneg_equalities_involving_vars ~exempt:exempt f2 neg))
        | FNot(f2) -> 
            (gather_nonneg_equalities_involving_vars ~exempt:exempt f2 (not neg));; 

exception SubstitutionLedToInconsistency of formula;;

(* If about to substitute in something like 5=7, abort because the whole conjunction is unsatisfiable *)
let equals_if_consistent (t1: term) (t2: term): formula =
  match (t1, t2) with
    | (TConst(str1), TConst(str2)) when str1 <> str2 -> raise (SubstitutionLedToInconsistency((FEquals(t1, t2))))
    | _ -> FEquals(t1, t2);;

(* f[v -> t] *)
let rec substitute_term (f: formula) (v: term) (t: term): formula = 
    match f with
        | FTrue -> f
        | FFalse -> f
        | FEquals(t1, t2) ->
          (* Remove fmlas which will be "x=x"; avoids inf. loop. *)
          if t1 = v && t2 = t then FTrue
          else if t2 = v && t1 = t then FTrue          
          else if t1 = v then equals_if_consistent t t2
          else if t2 = v then equals_if_consistent t1 t
          else f
        | FAtom(modstr, relstr, argterms) -> 
          let newargterms = map (fun arg -> 
              (*(printf "***** %s\n%!" (string_of_term arg));*)
              if v = arg then t else arg) argterms in
            FAtom(modstr, relstr, newargterms)
        | FOr(f1, f2) ->         
            let subs1 = substitute_term f1 v t in
            let subs2 = substitute_term f2 v t in
            if subs1 = FFalse then subs2 
            else if subs2 = FFalse then subs1            
            else FOr(subs1, subs2)       
        | FAnd(f1, f2) ->
            (* remove superfluous FTrues/FFalses *) 
            let subs1 = substitute_term f1 v t in
            let subs2 = substitute_term f2 v t in
            if subs1 = FTrue then subs2 
            else if subs2 = FTrue then subs1            
            else FAnd(subs1, subs2)       
        | FNot(f2) -> 
            FNot(substitute_term f2 v t);;

(* assume a clause body. exempt gives the terms that are in the head, and thus need to not be removed *)
let rec minimize_variables ?(exempt: term list = []) (f: formula): formula = 
  (* OPTIMIZATION: don't need to do gathering step repeatedly: *)
  let var_equals_fmlas = gather_nonneg_equalities_involving_vars ~exempt:exempt f false in
    (*printf "at fmla = %s\n%!" (string_of_formula f);        
    iter (fun pr -> let (t1, t2) = pr in (printf "pair: %s, %s\n%!" 
            (string_of_term t1) (string_of_term t2))) var_equals_fmlas;    *)

    if length var_equals_fmlas < 1 then
      f
    else 
    (* select equalities involving constants first, so we don't lose their context *)
      let constpairs = filter (function | (TVar(_), TConst(_)) -> true | _ -> false) var_equals_fmlas in 
      let (x, t) = if length constpairs > 0 then hd constpairs else hd var_equals_fmlas in  

      (* subst process will discover inconsistency due to multiple vals e.g. x=7, x=9, and throw exception *)
      try
        let newf = (substitute_term f x t) in
          (*printf "will subs out %s to %s. New is: %s.\n%!" (string_of_term x) (string_of_term t) (string_of_formula newf);*)
          minimize_variables ~exempt:exempt newf
        with SubstitutionLedToInconsistency(_) -> FFalse;;


(* all lowercased by parser *)
let plus_prefix = "plus";;
let minus_prefix = "minus";;

let packet_in_relname = "packet_in";;
let switch_reg_relname = "switch_port_in";;
let packet_fields = ["locsw";"locpt";"dlsrc";"dldst";"dltyp";"nwsrc";"nwdst";"nwproto"];;
let legal_to_modify_packet_fields = ["locpt";"dlsrc";"dldst";"dltyp";"nwsrc";"nwdst"];;

let swpt_fields = ["sw";"pt"];;

let built_in_decls = [DeclInc(packet_in_relname, "packet"); 
                      DeclInc(switch_reg_relname, "switch_port"); 
                      DeclOut("forward", ["packet"]);
                      DeclOut("emit", ["packet"]);

                      DeclEvent("packet", packet_fields);
                      DeclEvent("switch_port", swpt_fields)];;

let create_id_assign (k: string): assignment = {afield=k; atupvar=k};;

let built_in_reacts = [ ReactInc("packet", packet_in_relname); 
                        ReactInc("switch_port", switch_reg_relname); 
                        ReactOut("forward", packet_fields, "packet", map create_id_assign packet_fields, OutForward); 
                        ReactOut("emit", packet_fields, "packet", map create_id_assign packet_fields, OutEmit);                         
                      ];;



