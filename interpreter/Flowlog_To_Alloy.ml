(****************************************************************)
(* Automatic translation from Flowlog to Alloy                  *)
(****************************************************************)

open Printf
open Flowlog_Types
open Flowlog_Packets
open Flowlog_Helpers
open ExtList.List
open Unix 

let alloy_filename (flfn: string): string =
  (Filename.chop_extension flfn)^".als";;

(**********************************************************)
(* Some boilerplate (packets, etc.) *)    

let alloy_boilerplate (out: out_channel): unit =
  let localtm = localtime (gettimeofday()) in 
  fprintf out "// Produced automatically by flowlog -alloy at %d:%d:%d on %d %d %d\n%!"
              localtm.tm_hour localtm.tm_min localtm.tm_sec 
              localtm.tm_mon localtm.tm_mday (localtm.tm_year + 1900); 
  fprintf out "%s\n%!" "
pred true[] {}
pred false[] { some none }

abstract sig Event {}

sig Switchid {}
sig Macaddr {}
sig Ipaddr {}
sig Ethtyp {}
sig Portid {} 
sig Nwprotocol {}
// TODO: If a base type is unused, don't declare it. 
sig UdpPort {}
sig TcpPort {}

sig FLString {} 
sig FLInt{} 
";;

(* These functions take the _relation name_, not the event name
   (same for rule_uses, though) *)
let plus_rule_exists (p: flowlog_program) (tblname: string): bool =
  exists (fun cl -> match cl.orig_rule.action with 
    | AInsert(rtbl, _, _) when tblname = rtbl -> true
    | _ -> false) p.clauses;;
let minus_rule_exists (p: flowlog_program) (tblname: string): bool =
  exists (fun cl -> match cl.orig_rule.action with 
    | ADelete(rtbl, _, _) when tblname = rtbl -> true
    | _ -> false) p.clauses;;
let do_rule_exists (p: flowlog_program) (tblname: string): bool =
  exists (fun cl -> match cl.orig_rule.action with 
    | ADo(rtbl, _, _) when tblname = rtbl -> true
    | _ -> false) p.clauses;;
let rule_uses (p: flowlog_program) (tblname: string): bool =
  exists (fun cl -> cl.orig_rule.onrel = tblname) p.clauses;;


type alloy_ontology = {
  constants: (string * typeid) list;
  events_used: (string * event_def) list;
  tables_used: (string * table_def) list;
}

(**********************************************************)
  let alloy_of_term (t: term): string = 
    match t with
      | TConst(s) -> "C_"^s      
      | TVar(s) -> s      
      | TField(varname, fname) -> 
        (varname^"."^fname);;

  let rec alloy_of_formula (stateid: string) (f: formula): string = 
    match f with
      | FTrue -> "true[]"
      | FFalse -> "false[]"
      | FEquals(t1, t2) -> (alloy_of_term t1) ^ " = "^ (alloy_of_term t2)
      | FNot(f2) ->  "not ("^(alloy_of_formula stateid f2)^")"
      | FAtom("", relname, tlargs) -> 
          (String.concat "->" (map alloy_of_term tlargs))^" in "^stateid^"."^relname
      | FAtom(modname, relname, tlargs) -> 
          (String.concat "->" (map alloy_of_term tlargs))^" in "^stateid^"."^modname^"_"^relname
      | FAnd(f1, f2) -> "("^(alloy_of_formula stateid f1) ^ " && "^ (alloy_of_formula stateid f2)^")"
      | FOr(f1, f2) -> (alloy_of_formula stateid f1) ^ " || "^ (alloy_of_formula stateid f2)
  



(**********************************************************)

let event_is_used (p: flowlog_program) (ev_def: event_def): bool = 
  (* This event triggers a rule, or some outgoing_def triggered by a DO rule sends this event. *)
  let outrels_for_event = (filter_map (fun outd -> (match outd.react with 
      | OutSend(evn, _, _) when evn = ev_def.eventname -> Some(outd.outname)
      | OutEmit(evn) when evn = ev_def.eventname -> Some(outd.outname)
      | _ -> None)) p.outgoings) in
  (rule_uses p ev_def.eventname) || (exists (fun outrel -> do_rule_exists p outrel) outrels_for_event);;

let alloy_fieldtype (fldtype: string): string = 
  match fldtype with 
  | "string" -> "FLString"
  | "int" -> "FLInt"
  | _ -> String.capitalize fldtype;;


(* Actually print the ontology
   TODO: cleanup *)
let write_alloy_ontology (out: out_channel) (o: alloy_ontology): unit =  
  
  (****** Boilerplate ******************)  
  alloy_boilerplate out;

  (****** Events ******************)
  (* Every program's declared notifications need a sig... *)
  (* ...and an extensional constraint *)
  let declare_event (_,ev: string*event_def) = 
    let ifislone = if length ev.evfields > 0 then "" else "lone " in
    let supertypename = (match (get_superflavor_typename ev.eventname) with | Some(super) -> ("EV"^super) | None -> "Event") in
        fprintf out "%ssig EV%s extends %s {\n%!" ifislone ev.eventname supertypename;
        let flddecls = map (fun (fldname,fldtype) -> (sprintf "    %s: one %s") fldname (alloy_fieldtype fldtype)) ev.evfields in 
          fprintf out "%s%!" (String.concat ",\n" flddecls);
          fprintf out "}\n\n%!";

        if length ev.evfields > 0 then 
        begin
          fprintf out "fact EV%sExtensional { all ev1, ev2: EV%s | \n%!" ev.eventname ev.eventname;        
          let fieldequals = (map (fun (fld, _) -> sprintf "ev1.%s = ev2.%s" fld fld) ev.evfields) in
          let fieldsequal = String.concat " && " fieldequals in 
            fprintf out "(%s) implies ev1 = ev2}\n\n%!" fieldsequal;
        end in  
    iter declare_event o.events_used;

  (****** State ******************)
  let declare_state (_,decl: string*table_def): string = 
    let typesproduct = String.concat " -> " (map String.capitalize decl.tablearity) in 
        sprintf "    %s: set (%s)%!" decl.tablename typesproduct in

    fprintf out "sig State {\n%!";
    fprintf out "%s\n%!" (String.concat ",\n" (map declare_state o.tables_used));
    fprintf out "}\n%!";
    fprintf out "fact StateExtensional { all st1, st2: State |\n%!";
    let stateequals = (map (fun (_,tbl) -> sprintf "st1.%s = st2.%s" tbl.tablename tbl.tablename)
                           o.tables_used) in
    let statesequal = String.concat " && " stateequals in 
            fprintf out "(%s) implies st1 = st2}\n\n%!" statesequal;

  (****** Constants ******************)
  iter (fun (c_n, c_t) -> fprintf out "lone sig C_%s extends %s {}\n" c_n c_t) o.constants;
  fprintf out "\n%!";;

(* Extract ontology; don't print it *)
let program_to_ontology (p: flowlog_program): alloy_ontology =
  (* Identify the constants (like "0x1001") used and declare them. *)
  {constants= (map (fun c -> ((alloy_of_term c), "[FILL]")) 
                (fold_left (fun acc cl -> (unique ( acc @ (get_terms (function | TConst(_) -> true | _ -> false) cl.body))))
                           []
                           p.clauses));
   events_used=filter_map (fun ev -> if (event_is_used p ev) then Some((ev.eventname, ev)) else None) p.events;
   tables_used=(map (fun tbl -> (tbl.tablename, tbl)) ((get_local_tables p) @ (get_remote_tables p)))};;

(* TODO: support table-widening, etc. *)
(* For now, require tables to have same arity/types. *)
let resolve_tables (o1: alloy_ontology) (o2: alloy_ontology): (string * table_def) list =
  fold_left (fun acc tbl ->    
              let tbl_n, tbl_def = tbl in            
              if mem_assoc tbl_n acc && (assoc tbl_n acc) <> tbl_def then
                failwith (sprintf "The programs had different declared arities for table %s" tbl_n)
              else if mem_assoc tbl_n acc then
                acc
              else
                tbl :: acc)
            [] (o1.tables_used @ o2.tables_used);;

(* TODO: duplicate code in resolve funcs *)
(* EVENTS need to have the same shape/fields *)
let resolve_events (o1: alloy_ontology) (o2: alloy_ontology): (string * event_def) list =
  fold_left (fun acc ev ->    
              let ev_n, ev_def = ev in    
              if mem_assoc ev_n acc && (assoc ev_n acc) <> ev_def then
                failwith (sprintf "The programs had different notions of event %s" ev_n)
              else if mem_assoc ev_n acc then
                acc
              else
                ev :: acc)
            [] (o1.events_used @ o2.events_used);;

(* Better agree on types! o.constants is an association list. *)
let resolve_constants (o1: alloy_ontology) (o2: alloy_ontology): (string * typeid) list =
  fold_left (fun acc con ->
              let con_n, con_t = con in  
              if mem_assoc con_n acc && assoc con_n acc <> con_t then
                failwith (sprintf "The programs had different inferred types for constant %s: %s vs. %s" con_n con_t (assoc con_n acc))
              else if mem_assoc con_n acc then
                acc
              else
                con :: acc)
            [] (o1.constants @ o2.constants);;


let programs_to_ontology (p1: flowlog_program) (p2: flowlog_program): alloy_ontology =
  (* Detect conflicts + combine *)
  let o1 = program_to_ontology p1 in
  let o2 = program_to_ontology p2 in
    {constants=resolve_constants o1 o2; events_used=resolve_events o1 o2; tables_used=resolve_tables o1 o2};;

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
    match r.action with
      | ADelete(outrel, outargs, where) ->
        {outrel = (minus_prefix^"_"^outrel); outargs = outargs; where = where; increl = r.onrel; incvar = r.onvar}
      | AInsert(outrel, outargs, where) -> 
        {outrel = (plus_prefix^"_"^outrel);  outargs = outargs; where = where; increl = r.onrel; incvar = r.onvar}
      | ADo(outrel, outargs, where) -> 
        {outrel = outrel;                    outargs = outargs; where = where; increl = r.onrel; incvar = r.onvar}
  in
  
  let outarg_to_poss_equality (evrestricted: string) (i: int) (outarg: term): string =
    match outarg with
      | TField(v, f) -> sprintf "out%d = %s.%s" i evrestricted f (* NOT v and NOT "ev" *)
      | _ -> "true[]"
  in

  let make_quantified_decl (tqs: (term * bool) list): string list =
    (* negative occur of an "any" term becomes universal. TODO: risk of string muddling *)
    let quantify_helper (tq: term*bool) =
      match tq with     
        | TVar(vname), false when starts_with vname "any" -> sprintf "all %s : univ | " vname
        | TVar(vname), _ -> sprintf "some %s : univ | " vname      
        | _ -> failwith "make_quantified_decl" in
    
    let trimmed_list = fold_left (fun acc tq -> 
                                    let t, sn = tq in 
                                      if (exists (fun (ot,_) -> ot = t) acc) then acc else tq::acc)
                         [] tqs in
    map quantify_helper trimmed_list
  in

  let event_alloysig_for (increl: string): string =
    "EV"^increl in

  let alloy_of_pred_fragment (stateid: string) (pf : pred_fragment): string =
  (* substitute var names: don't get stuck on rules with different args or in var name! *)      
    let evtypename = (event_alloysig_for pf.increl) in
    let evrestrictedname = (sprintf "(%s <: ev)" evtypename) in
    let to_substitute = [(TVar(pf.incvar), TVar(evrestrictedname))](* [(TVar(pf.incvar), TVar("ev"))]*)
                        @ (mapi (fun i outarg -> (outarg, TVar("out"^(string_of_int i)))) pf.outargs) in
    let substituted = (substitute_terms pf.where to_substitute) in   
    (*printf "alloy of formula: %s\n%!" (string_of_formula substituted);*)
    let quantified_vars = [TVar("ev")] @ (mapi (fun i _ -> TVar("out"^(string_of_int i))) pf.outargs) in
    let freevars_signed = get_terms_with_sign (function | TVar(x) as t -> not (mem t quantified_vars) | _ -> false) true substituted in  
    (* If the free var is an ANY, be careful how to bind it. If it is an ANY that appears within a negation, must be ALL not EXISTS *)
    (* explicitly quantify rule-scope existentials *)
    let freevarstr = (String.concat " " (make_quantified_decl freevars_signed)) in

      "\n  (ev in "^evtypename^" && ("^freevarstr^" "^(alloy_of_formula stateid substituted)^")\n"^

      (* If field of invar in outargs, need to add an equality, otherwise connection is lost by alpha renaming. *)
      "      && "^(String.concat " && " (mapi (outarg_to_poss_equality evrestrictedname) pf.outargs))^")"
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
                   let thispred = sprintf "pred %s[st: State, ev: Event, %s] {\n%s\n}\n" 
                                    outrel 
                                    (String.concat ", " (mapi (fun i t -> sprintf "out%d : univ" i) (hd pfl).outargs))
                                    (String.concat " ||\n" (map (alloy_of_pred_fragment "st") pfl)) in
                   StringMap.add outrel thispred acc)
                   outrel_to_rules
                   StringMap.empty in
  StringMap.iter (fun outrel predstr -> fprintf out "%s\n%!" predstr) rulestrs;;

(**********************************************************)
(* transition: st x ev x st 
   (note this is a slight deviation from the language: packet-in becomes an event) *)
let alloy_transition (out: out_channel) (p: flowlog_program): unit =
  let build_table_transition (tbl : table_def): string =       
    let tupdvec = (String.concat "," (mapi  (fun i typ -> sprintf "tup%d: %s" i (String.capitalize typ)) tbl.tablearity)) in   
    let tupavec = (String.concat "," (init (length tbl.tablearity) (fun i -> sprintf "tup%d" i))) in   

    (* - { sw: Switch, sw2: Switch | minus_ucTC[st, ev, sw, sw2] } *)
    let minus_expr = if minus_rule_exists p tbl.tablename then sprintf "- { %s | %s_%s[st1, ev, %s]}" tupdvec minus_prefix tbl.tablename tupavec 
                     else "" in 
    let plus_expr =  if plus_rule_exists p tbl.tablename then sprintf "+ { %s | %s_%s[st1, ev, %s]}" tupdvec plus_prefix tbl.tablename tupavec 
                     else "" in
      sprintf "  st2.%s = (st1.%s\n            %s)\n            %s" 
              tbl.tablename tbl.tablename minus_expr plus_expr
  in

  let local_tables = get_local_tables p in 
  let remote_tables = get_remote_tables p in
    fprintf out "pred transition[st1: State, ev: Event, st2: State] { \n%!";
    fprintf out "%s\n%!" (String.concat " &&\n\n" (map build_table_transition (local_tables @ remote_tables)));
    fprintf out "}\n\n%!";;

let alloy_outpolicy (out: out_channel) (p: flowlog_program): unit =
  let alloy_out_difference (d: outgoing_def): string option =
    if do_rule_exists p d.outname then
      Some(sprintf "  %s[st1, ev, ev2]" d.outname)
    else None
  in
    fprintf out "pred outpolicy[st1: State, ev: Event, ev2: Event] { \n%!";
    fprintf out "%s" (String.concat " ||\n" (filter_map alloy_out_difference p.outgoings)); 
    fprintf out "}\n%!";;

(**********************************************************)
let alloy_boilerplate_pred (out: out_channel): unit = 
  fprintf out "
pred testPred[] {
  //some st1, st2: State, ev: Event |
  //   transition[st1, ev, st2] &&
  //   st1 != st2 and //no st1.learned &&
  //   no st1.switch_has_port
}
run testPred for 3 but 1 Event, 2 State\n%!";;

let write_as_alloy (ontology_fn: string option) (p: flowlog_program) (fn: string): unit =
  if not (ends_with fn ".als") then 
    failwith "Alloy filename must end with .als, so as not to accidently overwrite .flg files.";

    let out = open_out fn in 
    	(*alloy_boilerplate out;    
      alloy_constants out p;  
    	alloy_declares out p;
      alloy_state out p;*)      
      (match ontology_fn with
        | None -> 
          fprintf out "module %s\n" (Filename.chop_extension fn);
          write_alloy_ontology out (program_to_ontology p)
        | Some(ofn) -> 
          fprintf out "module %s\n" (Filename.chop_extension fn);
          fprintf out "open %s as o\n" ofn);
    	alloy_actions out p;
    	alloy_transition out p;    
      alloy_outpolicy out p;	
      alloy_boilerplate_pred out;
		  close_out out;
      printf "~~~ Finished compiling %s to Alloy. ~~~\n%!" fn;;

(**********************************************************)

(**********************************************************)
(* Produce a compatable ontology for these two programs. 
   - constants
   - state relations
   - events used *)
let write_shared_ontology (fn: string) (ontol: alloy_ontology): unit =
  let out = open_out fn in 
    write_alloy_ontology out ontol;
    close_out out;;

let build_starting_state_trace (ontol: alloy_ontology): string =
  let tracestrs = map (fun (n,_ ) -> sprintf "no overall.trace.first.%s" n) ontol.tables_used in
  String.concat "\n" tracestrs;;

(* *)
let write_as_alloy_change_impact (p1: flowlog_program) (fn1: string) (p2: flowlog_program) (fn2: string) (reach: bool): unit = 
  let modname1 = (Filename.chop_extension (Filename.basename fn1)) in
  let modname2 = (Filename.chop_extension (Filename.basename fn2)) in
  let ofn = ("ontology_"^modname1^"_vs_"^modname2) in
  let ontol = programs_to_ontology p1 p2 in
  write_shared_ontology (ofn^".als") ontol;
  
  write_as_alloy (Some ofn) p1 fn1;
  write_as_alloy (Some ofn) p2 fn2;
  
  let out = open_out "change-impact.als" in 
  if not reach then
  begin
  (* 3 states since prestate, newstate1, newstate 2. *)
  (* 2 events since ev, outev *)      
      fprintf out "
module cimp

open %s as o
open %s as prog1
open %s as prog2

pred changeImpact[] { 
  some ev: Event, st: State |
  some newst1, newst2: State |
    (prog1/transition[st, ev, newst1] and 
     prog2/transition[st, ev, newst2] and 
     newst1 != newst2)
    || 
    some outev: Event | 
      (prog1/outpolicy[st, ev, outev] and not prog2/outpolicy[st, ev, outev]) ||
      (prog2/outpolicy[st, ev, outev] and not prog1/outpolicy[st, ev, outev])
}
run changeImpact for 5 but 3 State, 2 Event
\n%!" 
  ofn
  (Filename.chop_extension fn1)
  (Filename.chop_extension fn2);
  end
  else
  begin
    (* with reachability *)
    fprintf out "
module cimp

open %s as o
open %s as prog1
open %s as prog2

one sig overall { 
  trace: seq State 
}

// Don't always have pigeonhole: could loop back to prior state.
//fact noDuplicateStates {
// instead, force all states to live in the trace
fact allStatesInSeq {
  State = overall.trace.elems
}

fact startingState { 
  %s 
}


// TODO: concern that really we ought to NOT add the 2 extra states, but rather 
// should just check for diffs in plus and minus relations (mod what's already there)
// That would be faster, but give less useful info.

// Can't say this naively: we may have branching in the last state, due to ch imp.
fact orderRespectsTransitionsAndStops {
  all i : overall.trace.inds - overall.trace.lastIdx |  
    let s = overall.trace[i] | 
    let nexts = overall.trace[i+1] |
    let prevs = overall.trace[i-1] |

    // If this state is immediately after a 'branching ch imp' pre-state
    // the next state is the branch by prog2 and the LAST state
    // (This lets us avoid having two separate sequences)
    ((i > 0 && some chev: Event | changeStateTransition[prevs, chev])
      // Need to say it's the SAME event that bridges from the chimp state to poststates
      => (some nexts && (some ev: Event | prog2/transition[s, ev, nexts]
                                                                   && prog1/transition[prevs, ev, s]) 
          && nexts = overall.trace.last) 
      else (some ev: Event | prog1/transition[s, ev, nexts]))

  // Make sure a change-impact scenario has a 'next'
// TODO this fails. because this ALL doesn't hold for the last element.
  and
  ((some chev: Event | changeStateTransition[s, chev]) implies some nexts)

    // If this state is a 'policy change chimp' pre-state then
    // the next state is the last state.
    and // don't forget parens here!
    ((some chev: Event | changePolicyOutput[s, chev]) implies 
      nexts = overall.trace.last)
  // TODO check: what if we have a CST followed by a CPO? or vice versa?
}

// Make sense to divide the two types of change impact,
//  to avoid confusion with what the final state in a trace represents.
pred changeStateTransition[prestate: State, ev: Event]
{
  some disj newst1, newst2: State |
    (prog1/transition[prestate, ev, newst1] and 
     prog2/transition[prestate, ev, newst2])
}
pred changePolicyOutput[prestate: State, ev: Event] { 
    some outev: Event | 
      (prog1/outpolicy[prestate, ev, outev] and not prog2/outpolicy[prestate, ev, outev]) ||
      (prog2/outpolicy[prestate, ev, outev] and not prog1/outpolicy[prestate, ev, outev])
}
pred changeImpact[prestate: State, ev: Event] { 
    changeStateTransition[prestate, ev]   
    || changePolicyOutput[prestate, ev]
}

// If go above 8 (7?) states, be sure to increase the size of int
// seq and State should always have the same bound
run changeImpact for 6 but 4 State, 5 Event, 4 seq

\n%!" 
  ofn
  (Filename.chop_extension fn1)
  (Filename.chop_extension fn2)
  (build_starting_state_trace ontol);
  end;
      close_out out;
      printf "WARNING: Make sure the two files have the same ontology, or the generated file may not run.\n%!";
      printf "~~~ Finished change-impact query file. ~~~\n%!";;
