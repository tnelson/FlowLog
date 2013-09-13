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
      | FNot(f) ->  "not ("^(alloy_of_formula f)^")"
      | FAtom("", relname, tlargs) -> 
          (String.concat "->" (map alloy_of_term tlargs))^" in "^relname
      | FAtom(modname, relname, tlargs) -> 
          (String.concat "->" (map alloy_of_term tlargs))^" in "^modname^"/"^relname
      | FAnd(f1, f2) -> "("^(alloy_of_formula f1) ^ " && "^ (alloy_of_formula f2)^")"
      | FOr(f1, f2) -> (alloy_of_formula f1) ^ " || "^ (alloy_of_formula f2)
  

(**********************************************************)
(* Every RULE in the program gets a predicate *)
let alloy_rules (out: out_channel) (p: flowlog_program): unit =
  let alloy_of_action (act: action): string =
    match act with
      | ADelete(outrel, outargs, where)
      | AInsert(outrel, outargs, where)  
      | ADo(outrel, outargs, where) ->
        alloy_of_formula where
  in
  let make_rule (i: int) (r: srule): unit = 
    match r with 
    | Rule(increl, incvar, act) ->       
	    fprintf out "pred rule%d(%s: %s) { %s }\n%!" i incvar increl (alloy_of_action act)
  in

(* Not right: EV vs. relations. Also need both inc and out in pred, yes?
   alternatively, can abandon per-rule preds and just do per action/statechange*)

  iteri make_rule (unique (map (fun cl -> cl.orig_rule) p.clauses));;

(**********************************************************)
(* Every +, every -, every DO gets a predicate IFFing disj of appropriate rules *)
let alloy_actions (out: out_channel) (p: flowlog_program): unit =
	();;

(**********************************************************)
(* transition: st x ev x st 
   (note this is a slight deviation from the language: packet-in becomes an event) *)
let alloy_transition (out: out_channel) (p: flowlog_program): unit =
	();;

(**********************************************************)
let write_as_alloy (p: flowlog_program) (fn: string): unit =
    let out = open_out fn in 
    	alloy_boilerplate out;      
    	alloy_declares out p;
      alloy_state out p;
    	alloy_rules out p;
    	alloy_actions out p;
    	alloy_transition out p;    	
		  close_out out;
      printf "~~~ Finished compiling to Alloy. ~~~\n%!";;
