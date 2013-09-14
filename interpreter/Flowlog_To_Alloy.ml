open Printf
open Flowlog_Types
open Flowlog_Helpers
open ExtList.List
open Unix 

(* Which is the right target language: Alloy or Margrave? 

+Margrave: automatic OSEPL.
+Margrave: easier rule-blaming (Alloy can do w/ added preds or with evaluator. 
           We can probably build blaming into the compiled target?)

+Alloy: easier to express properties (e.g. correctness of TC)
+Alloy: support for TC (could add to Margrave, but would be added engineering)
+Alloy: smoother user experience, visualization, treeview, etc.

+Alloy: better known tool
+Margrave: needs use

*)

(**********************************************************)
(* Some boilerplate (packets, etc.) *)    

let alloy_boilerplate (out: out_channel): unit =
  let localtm = localtime (gettimeofday()) in 
  fprintf out "// Produced automatically by flowlog -alloy at %d:%d:%d on %d %d %d\n%!"
              localtm.tm_hour localtm.tm_min localtm.tm_sec 
              localtm.tm_mon localtm.tm_mday (localtm.tm_year + 1900); 
  fprintf out "%s\n%!" "
sig Event {}
sig Switch {}
sig MacAddr {}
sig IPAddr {}
sig EthTyp {}
sig PhysicalPort {} 
sig NwProtocol {}";;

(**********************************************************)
(* Every program's declared notifications need a sig... *)
(* ...and an extensional constraint *)
let alloy_declares (out: out_channel) (p: flowlog_program): unit =
  let declare_event (decl: sdecl) =
    match decl with 
      | DeclEvent(evname, evfields) ->  
        let ifislone = if length evfields > 0 then "" else "lone " in
          fprintf out "%ssig EV%s extends Event {\n%!" ifislone evname;
          let flddecls = map (sprintf "    %s: one X") evfields in 
            fprintf out "%s%!" (String.concat ",\n" flddecls);
            fprintf out "}\n\n%!";

        if length evfields > 0 then 
        begin
          fprintf out "fact EV%sExtensional { all ev1, ev2: EV%s | \n%!" evname evname;        
          let fieldequals = (map (fun fld -> sprintf "ev1.%s = ev2.%s" fld fld) evfields) in
          let fieldsequal = String.concat "&&" fieldequals in 
            fprintf out "(%s) implies ev1 = ev2}\n\n%!" fieldsequal;
        end;
      | _ -> failwith "declare_event"
  in
  	iter declare_event (filter (function | DeclEvent(_,_) -> true | _ -> false) p.decls);;

(**********************************************************)
(* Tables in state sig, and extensional fact for states (incl. all tables) *)

let get_tablename (tdecl: sdecl): string = 
  match tdecl with
    | DeclTable(tblname, fieldtypes) 
    | DeclRemoteTable(tblname, fieldtypes) -> 
      tblname
    | _ -> failwith "get_tablename";;

let get_table_arity (tdecl: sdecl): int = 
  match tdecl with
    | DeclTable(tblname, fieldtypes) 
    | DeclRemoteTable(tblname, fieldtypes) -> 
      length fieldtypes
    | _ -> failwith "get_table_arity";;

let alloy_state (out: out_channel) (p: flowlog_program): unit =
  let declare_state (decl: sdecl) = 
    match decl with 
      | DeclTable(tblname, fieldtypes) 
      | DeclRemoteTable(tblname, fieldtypes) -> 
        let typesproduct = String.concat " -> " fieldtypes in 
          fprintf out "    %s: %s\n%!" tblname typesproduct
      | _ -> failwith "declare_state"
  in

  let local_tables = get_local_tables p in 
  let remote_tables = (map (fun (react, decl) -> decl) (get_remote_tables p)) in
    fprintf out "sig State {\n%!";
    iter declare_state local_tables;
    iter declare_state remote_tables;
    fprintf out "}\n%!";
    fprintf out "fact StateExtensional { all st1, st2: State |\n%!";
    let stateequals = (map (fun tblname -> sprintf "st1.%s = st2.%s" tblname tblname)
                           (map get_tablename (local_tables @ remote_tables))) in
    let statesequal = String.concat " && " stateequals in 
            fprintf out "(%s) implies st1 = st2}\n\n%!" statesequal;;

(**********************************************************)
  let alloy_of_term (t: term): string = 
    match t with
      | TConst(s) -> s      
      | TVar(s) -> s      
      | TField(varname, fname) -> 
        (varname^"."^fname);;

  let rec alloy_of_formula (f: formula): string = 
    match f with
      | FTrue -> "true"
      | FFalse -> "false"
      | FEquals(t1, t2) -> (alloy_of_term t1) ^ " = "^ (alloy_of_term t2)
      | FNot(f2) ->  "not ("^(alloy_of_formula f2)^")"
      | FAtom("", relname, tlargs) -> 
          (String.concat "->" (map alloy_of_term tlargs))^" in "^relname
      | FAtom(modname, relname, tlargs) -> 
          (String.concat "->" (map alloy_of_term tlargs))^" in "^modname^"/"^relname
      | FAnd(f1, f2) -> "("^(alloy_of_formula f1) ^ " && "^ (alloy_of_formula f2)^")"
      | FOr(f1, f2) -> (alloy_of_formula f1) ^ " || "^ (alloy_of_formula f2)
  

(**********************************************************)
(* Every +, every -, every DO gets a predicate IFFing disj of appropriate rules *)

(*
Out args may be actual field names of incoming tuple. Thus, need to let them be any type

pred <outrel>[st: State, <incvar>: <reactive of increl>, <outarg0> :univ, <outarg1> :univ, ...] {
  (rule1 to alloy) or
  (rule2 to alloy) or...
  ...
}
*)

type pred_fragment = {outrel: string; increl: string; incvar: string; 
                      outargs: term list; where: formula};;

let alloy_actions (out: out_channel) (p: flowlog_program): unit =  
  let make_rule (r: srule): pred_fragment = 
    match r with 
    | Rule(increl, incvar, act) ->
      match act with
        | ADelete(outrel, outargs, where) ->
          {outrel = (minus_prefix^"_"^outrel); outargs = outargs; where = where; increl = increl; incvar = incvar}

        | AInsert(outrel, outargs, where) -> 
          {outrel = (plus_prefix^"_"^outrel);  outargs = outargs; where = where; increl = increl; incvar = incvar}
        | ADo(outrel, outargs, where) -> 
          {outrel = outrel;                    outargs = outargs; where = where; increl = increl; incvar = incvar}
  in
  
  let outarg_to_poss_equality (i: int) (outarg: term): string =
    match outarg with
      | TField(v, f) -> sprintf "out%d = %s.%s" i v f
      | _ -> "true"
  in

  let make_existential_decl (t: term): string =
    match t with 
      | TVar(vname) -> sprintf "some %s : univ | " vname
      | _ -> failwith "make_existential_decl"
  in

  let event_alloysig_for (increl: string): string =
    match get_input_defn_for_rel p increl with
      | ReactInc(typeid, _) -> "EV"^typeid
      | _ -> failwith "event_typeid_for" 
  in

  let alloy_of_pred_fragment (pf : pred_fragment): string =
  (* substitute var names: don't get stuck on rules with different args or in var name! *)      
    let to_substitute = [(TVar(pf.incvar), TVar("ev"))]
                        @ (mapi (fun i outarg -> (outarg, TVar("out"^(string_of_int i)))) pf.outargs) in
    let substituted = (substitute_terms pf.where to_substitute) in   
    printf "alloy of formula: %s\n%!" (string_of_formula substituted);
    let quantified_vars = [TVar("ev")] @ (mapi (fun i _ -> TVar("out"^(string_of_int i))) pf.outargs) in
    let freevars = get_terms (function | TVar(x) as t -> not (mem t quantified_vars) | _ -> false) substituted in
    (* explicitly quantify rule-scope existentials *)
    let freevarstr = (String.concat " " (map make_existential_decl freevars)) in
      "\n  (ev in "^(event_alloysig_for pf.increl)^" && ("^freevarstr^" "^(alloy_of_formula substituted)^")\n"^
      (* If field of invar in outargs, need to add an equality, otherwise connection is lost by alpha renaming. *)
      "      && "^(String.concat " && " (mapi outarg_to_poss_equality pf.outargs))^")"
  in

  (* Accumulate a map from outrel to rules that contribute*)
  let outrel_to_rules = fold_left (fun acc pf -> 
              if StringMap.mem pf.outrel acc then
                StringMap.add pf.outrel (pf :: StringMap.find pf.outrel acc) acc
              else
                StringMap.add pf.outrel [pf] acc) 
            StringMap.empty 
            (map make_rule (unique (map (fun cl -> cl.orig_rule) p.clauses))) in
  (* Convert each outrel to a string for Alloy*)
  let rulestrs = 
    StringMap.fold (fun outrel pfl acc -> 
                   let thispred = sprintf "pred %s[st: State, ev: univ, %s] {\n%s\n}\n" 
                                    outrel 
                                    (String.concat ", " (mapi (fun i t -> sprintf "out%d : univ" i) (hd pfl).outargs))
                                    (String.concat " ||\n" (map alloy_of_pred_fragment pfl)) in
                   StringMap.add outrel thispred acc)
                   outrel_to_rules 
                   StringMap.empty in
  StringMap.iter (fun outrel predstr -> fprintf out "%s\n%!" predstr) rulestrs;;


(**********************************************************)
(* transition: st x ev x st 
   (note this is a slight deviation from the language: packet-in becomes an event) *)
let alloy_transition (out: out_channel) (p: flowlog_program): unit =
  let build_table_transition (tdecl : sdecl): string = 
    let tablename = get_tablename tdecl in    
    let tupvec = (String.concat "," (init (get_table_arity tdecl) (fun i -> sprintf "tup%d" i))) in     
    (* - { sw: Switch, sw2: Switch | minus_ucTC[st, ev, sw, sw2] } *)
    let minus_expr = sprintf "{ %s | %s_%s[st1, ev, %s]}" tupvec minus_prefix tablename tupvec in 
    let plus_expr =  sprintf "{ %s | %s_%s[st1, ev, %s]}" tupvec plus_prefix tablename tupvec in
      sprintf "  st2.%s = (st1.%s\n            - %s)\n            + %s" 
              tablename tablename minus_expr plus_expr
  in

  let local_tables = get_local_tables p in 
  let remote_tables = (map (fun (react, decl) -> decl) (get_remote_tables p)) in
    fprintf out "pred transition[st1: State, ev: Event, st2: State] { \n%!";
    fprintf out "%s\n%!" (String.concat " &&\n\n" (map build_table_transition (local_tables @ remote_tables)));
    fprintf out "}\n%!";;

(**********************************************************)
let write_as_alloy (p: flowlog_program) (fn: string): unit =
    let out = open_out fn in 
    	alloy_boilerplate out;      
    	alloy_declares out p;
      alloy_state out p;
    	alloy_actions out p;
    	alloy_transition out p;    	
		  close_out out;
      printf "~~~ Finished compiling to Alloy. ~~~\n%!";;
