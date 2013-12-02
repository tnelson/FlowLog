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


(*sig EVpacket extends Event
           {locsw: Switchid, locpt: Portid,
            dlsrc: Macaddr, dldst: Macaddr, dltyp: Ethtyp,
            nwsrc: Ipaddr, nwdst: Ipaddr, nwproto: Nwprotocol }
*)

(**********************************************************)
(* Identify the constants (like "0x1001") used and declare them. *)
let alloy_constants (out: out_channel) (p: flowlog_program): unit =
  let constants = fold_left (fun acc cl -> unique ( acc @ (get_terms (function | TConst(_) -> true | _ -> false) cl.body)))
                  []
                  p.clauses in
  iter (function | TConst(c) -> fprintf out "lone sig C_%s extends [FILL] {}\n" c | _ -> failwith "alloy_constants") constants;
  fprintf out "\n%!";;

(**********************************************************)
(* Every program's declared notifications need a sig... *)
(* ...and an extensional constraint *)

let event_is_used (p: flowlog_program) (evname: string): bool = 
  (* This event triggers a rule, or some outgoing_def triggered by a DO rule sends this event. *)
  let outrels_for_event = (filter_map (fun outd -> (match outd.react with 
      | OutSend(evn, _, _) when evn = evname -> Some(outd.outname)
      | OutEmit(evn) when evn = evname -> Some(outd.outname)
      | _ -> None)) p.outgoings) in
  (rule_uses p evname) || (exists (fun outrel -> do_rule_exists p outrel) outrels_for_event);;

let alloy_fieldtype (fldtype: string): string = 
  match fldtype with 
  | "string" -> "FLString"
  | "int" -> "FLInt"
  | _ -> String.capitalize fldtype;;

let alloy_declares (out: out_channel) (p: flowlog_program): unit =
  let declare_event (decl: sdecl) =
    match decl with
      | DeclEvent(evname, evfields) when event_is_used p evname ->  
          let ifislone = if length evfields > 0 then "" else "lone " in
          let supertypename = (match (get_superflavor_typename evname) with | Some(super) -> ("EV"^super) | None -> "Event") in
            fprintf out "%ssig EV%s extends %s {\n%!" ifislone evname supertypename;
            let flddecls = map (fun (fldname,fldtype) -> (sprintf "    %s: one %s") fldname (alloy_fieldtype fldtype)) evfields in 
              fprintf out "%s%!" (String.concat ",\n" flddecls);
              fprintf out "}\n\n%!";

        if length evfields > 0 then 
        begin
          fprintf out "fact EV%sExtensional { all ev1, ev2: EV%s | \n%!" evname evname;        
          let fieldequals = (map (fun (fld, _) -> sprintf "ev1.%s = ev2.%s" fld fld) evfields) in
          let fieldsequal = String.concat " && " fieldequals in 
            fprintf out "(%s) implies ev1 = ev2}\n\n%!" fieldsequal;
        end;      
      | _ -> ()
  in
  	iter declare_event (filter (function | DeclEvent(_,_) -> true | _ -> false) p.desugared_decls);;

(**********************************************************)
(* Tables in state sig, and extensional fact for states (incl. all tables) *)

let get_tablename (tdecl: sdecl): string = 
  match tdecl with
    | DeclTable(tblname, fieldtypes) 
    | DeclRemoteTable(tblname, fieldtypes) -> 
      tblname
    | _ -> failwith "get_tablename";;

let get_table_fieldtypes (tdecl: sdecl): string list = 
  match tdecl with
    | DeclTable(tblname, fieldtypes) 
    | DeclRemoteTable(tblname, fieldtypes) -> 
      fieldtypes
    | _ -> failwith "get_table_fieldtypes";;

let alloy_state (out: out_channel) (p: flowlog_program): unit =
  let declare_state (decl: table_def): string = 
    let typesproduct = String.concat " -> " (map String.capitalize decl.tablearity) in 
        sprintf "    %s: set (%s)%!" decl.tablename typesproduct
  in

  let local_tables = get_local_tables p in 
  let remote_tables = get_remote_tables p in
    fprintf out "sig State {\n%!";
    fprintf out "%s\n%!" (String.concat ",\n" (map declare_state (local_tables @ remote_tables)));
    fprintf out "}\n%!";
    fprintf out "fact StateExtensional { all st1, st2: State |\n%!";
    let stateequals = (map (fun tbl -> sprintf "st1.%s = st2.%s" tbl.tablename tbl.tablename)
                           (local_tables @ remote_tables)) in
    let statesequal = String.concat " && " stateequals in 
            fprintf out "(%s) implies st1 = st2}\n\n%!" statesequal;;

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
  (*let do_assignment_subs (evid: string) (assns: assignment list) (tupvar: string): string = 
    let foundassign = find (fun assn -> assn.atupvar = tupvar) assns in
      sprintf "%s.%s" evid foundassign.afield
  in*) 

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
    printf "alloy of formula: %s\n%!" (string_of_formula substituted);
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
  some st1, st2: State, ev: Event |
     transition[st1, ev, st2] &&
     st1 != st2 and //no st1.learned &&
     no st1.switch_has_port
}
run testPred for 3 but 1 Event, 2 State\n%!";;

let write_as_alloy (p: flowlog_program) (fn: string): unit =
  if not (ends_with fn ".als") then 
    failwith "Alloy filename must end with .als, so as not to accidently overwrite .flg files.";

    let out = open_out fn in 
    	alloy_boilerplate out;    
      alloy_constants out p;  
    	alloy_declares out p;
      alloy_state out p;
    	alloy_actions out p;
    	alloy_transition out p;    
      alloy_outpolicy out p;	
      alloy_boilerplate_pred out;
		  close_out out;
      printf "~~~ Finished compiling %s to Alloy. ~~~\n%!" fn;;

(**********************************************************)
let write_as_alloy_change_impact (p1: flowlog_program) (fn1: string) (p2: flowlog_program) (fn2: string): unit = 

  write_as_alloy p1 fn1;
  write_as_alloy p2 fn2;
  let out = open_out "change-impact.als" in 
      fprintf out "
module cimp

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
}\n%!" (Filename.chop_extension fn1) (Filename.chop_extension fn2);
  (* 3 states since prestate, newstate1, newstate 2. *)
  (* 2 events since ev, outev *)
      fprintf out "run changeImpact for 3 State, 2 Event";
      close_out out;
      printf "WARNING: Make sure the two files have the same ontology, or the generated file may not run.\n%!";
      printf "~~~ Finished change-impact query file. ~~~\n%!";;
