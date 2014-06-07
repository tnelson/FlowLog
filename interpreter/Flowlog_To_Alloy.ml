(****************************************************************)
(* Automatic translation from Flowlog to Alloy                  *)
(****************************************************************)

(* TODO: current translation drops destination for "send to" events.
   So two separate named pipes with the same output event type will be
   indistinguishable in this model. *)


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
pred false[] { not true[] }

abstract sig Event {}

sig Switchid {}
sig Macaddr {}
sig Ipaddr {}
sig Ethtyp {}
sig Portid {}
sig Nwprotocol {}
// TODO: If a base type is unused, don't declare it.
sig Tpport {} // transport-layer port (TCP or UDP) number

sig FLString {}
sig FLInt{}

one sig BuiltIns {
   add: ((Tpport+FLInt+Portid)->(Tpport+FLInt+Portid))-> (Tpport+FLInt+Portid)

// Constrain add to be a partial function. Can help or hinder performance.
//   add: ((Tpport+FLInt+Portid)->(Tpport+FLInt+Portid))-> lone (Tpport+FLInt+Portid)

//  univ^3 is far too big. And cross-use of constants means can't separate cleanly by types
//  add: univ -> univ -> univ
}

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
let mod_rule_exists (p: flowlog_program) (tblname: string) (sign: bool): bool =
  if sign then plus_rule_exists p tblname
  else minus_rule_exists p tblname;;

let do_or_forward_rule_exists (p: flowlog_program) (tblname: string): bool =
  exists (fun cl -> match cl.orig_rule.action with
    | ADo(rtbl, _, _) when tblname = rtbl -> true
    | AForward(_, _, _) when tblname = "forward" -> true
    | _ -> false) p.clauses;;
let rule_uses (p: flowlog_program) (tblname: string): bool =
  exists (fun cl -> cl.orig_rule.onrel = tblname) p.clauses;;


type alloy_ontology = {
  filename: string;
  constants: (string * typeid) list;
  events_used: (string * event_def) list;
  tables_used: (string * table_def) list;
}

(**********************************************************)
  let add_freevar_sym_str (v: string): string =
    "var_"^v;;

  let add_freevar_sym (v: term): term =
    match v with
      | TVar(vname) -> TVar(add_freevar_sym_str vname)
      | _ -> failwith (sprintf "add_freevar_sym: %s" (string_of_term v));;

  let alloy_of_term (t: term): string =
    match t with
      | TConst(s) when (starts_with s "-") -> "C_minus"^(String.sub s 1 ((String.length s)-1))
      | TConst(s) -> "C_"^s
      | TVar(s) -> s
      | TField(varname, fname) ->
        (varname^"."^fname);;

  let rec alloy_of_formula (o: alloy_ontology) (stateid: string) (f: formula): string =
    match f with
      | FTrue -> "true[]"
      | FFalse -> "false[]"
      | FEquals(t1, t2) -> (alloy_of_term t1) ^ " = "^ (alloy_of_term t2)
      | FIn(t, addr, mask) -> sprintf "%s -> %s -> %s in in_ipv4_range" (alloy_of_term t) (alloy_of_term addr) (alloy_of_term mask)
      | FNot(f2) ->  "not ("^(alloy_of_formula o stateid f2)^")"
      | FAtom("", relname, tlargs) when (exists (fun (tname, _) -> tname=relname) o.tables_used) ->
          (String.concat "->" (map alloy_of_term tlargs))^" in "^stateid^"."^relname
      | FAtom("", relname, tlargs) ->
          (String.concat "->" (map alloy_of_term tlargs))^" in o/BuiltIns."^relname
      | FAtom(modname, relname, tlargs) ->
          (String.concat "->" (map alloy_of_term tlargs))^" in "^stateid^"."^modname^"_"^relname
      | FAnd(f1, f2) ->
        (Format.sprintf "(@[%s@] && @[%s@])"
          (alloy_of_formula o stateid f1)
          (alloy_of_formula o stateid f2))
      | FOr(f1, f2) -> (alloy_of_formula o stateid f1) ^ " || "^ (alloy_of_formula o stateid f2)


(**********************************************************)
let event_is_used (p: flowlog_program) (ev_def: event_def): bool =
  (* This event triggers a rule, or some outgoing_def triggered by a DO rule sends this event. *)
  let outrels_for_event = (filter_map (fun outd -> (match outd.react with
      | OutSend(evn, _, _) when evn = ev_def.eventname -> Some(outd.outname)
      | OutEmit(evn) when evn = ev_def.eventname -> Some(outd.outname)
      | _ -> None)) p.outgoings) in
  (rule_uses p ev_def.eventname) || (exists (fun outrel -> do_or_forward_rule_exists p outrel) outrels_for_event);;

let assemble_needed_events (p: flowlog_program) (ev_def: event_def): event_def list =
  (* Is this a packet flavor? If so, its superflavors are needed *)
  let supers = built_in_supertypes ev_def.eventname in
    printf "DEBUG: assembling needed events for %s. Got %s\n%!" ev_def.eventname (String.concat "," supers);
    map (get_event p) supers;;

let typestr_to_alloy (fldtype: string): string =
  match fldtype with
  | "string" -> "FLString"
  | "int" -> "FLInt"
  | _ -> String.capitalize fldtype;;


let get_bottom_fields (o: alloy_ontology) (ev: event_def) =
  match get_superflavor_typename ev.eventname with
    | None -> ev.evfields
    | Some(supername) ->
      let superev = assoc supername o.events_used in
        subtract ev.evfields superev.evfields;;


(********************************************************************)

(* Return a map from basic types to integers that give a bound
   for representing the fields of an arbitrary event *)
let single_event_ceilings (o: alloy_ontology): (int) StringMap.t =
  let count_ceiling (e: event_def): (int) StringMap.t =
    fold_left (fun acc (fldname, t) ->
      let alloyt = typestr_to_alloy t in
      (*printf "Processing field %s with type %s. mem = %b\n%!" fldname alloyt (StringMap.mem alloyt acc);*)
      if StringMap.mem alloyt acc then
        StringMap.add alloyt (1+(StringMap.find alloyt acc)) acc
      else
        StringMap.add alloyt 1 acc)
    StringMap.empty
    e.evfields in

  let merge_ceilings (k: string) opt1 opt2: int option =
    match opt1, opt2 with
      | None, Some(_) -> opt2
      | Some(_), None -> opt1
      | Some(v1), Some(v2) -> Some(max v1 v2)
      | _ -> None in

  fold_left (fun acc (_, evdef) ->
    let localceiling = count_ceiling evdef in
      StringMap.merge merge_ceilings acc localceiling)
  StringMap.empty
  o.events_used;;

(********************************************************************)

(* Actually print the ontology
   TODO: cleanup *)
let write_alloy_ontology (out: out_channel) (o: alloy_ontology): unit =

  printf "Writing ontology. Tables used: %s\n%!" (string_of_list "," (fun (n, d) -> n) o.tables_used);

  (****** Boilerplate ******************)
  alloy_boilerplate out;

  (****** Events ******************)
  (* Every program's declared notifications need a sig... *)
  (* ...and an extensional constraint *)
  let declare_event (_,ev: string*event_def) =
    let ifislone = if length ev.evfields > 0 then "" else "lone " in
    let supertypename = (match (get_superflavor_typename ev.eventname) with | Some(super) -> ("EV"^super) | None -> "Event") in
        fprintf out "%ssig EV%s extends %s {\n%!" ifislone ev.eventname supertypename;
        let flddecls = map (fun (fldname,fldtype) -> (sprintf "    %s: one %s") fldname (typestr_to_alloy fldtype)) (get_bottom_fields o ev) in
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
    let typesproduct = String.concat " -> " (map String.capitalize (map typestr_to_alloy decl.tablearity)) in
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
  iter (fun (c_n, c_t) -> fprintf out "lone sig %s extends %s {}\n" c_n c_t) o.constants;
  fprintf out "\n%!";;

(* If packet-flavors are used, need to declare their parent flavors *)
let get_needed_events (p: flowlog_program) =
  let supers_if_used (ev: event_def) =
      if (event_is_used p ev) then
        Some(assemble_needed_events p ev)
      else
        None in
    unique (flatten (filter_map supers_if_used p.events));;

(* Extract ontology; don't print it yet *)
let program_to_ontology (p: flowlog_program): alloy_ontology =

  (* ASSUMPTION: constants have the same type across the entire program.
     This is not generally true, and could lead to clashes in some programs.
     In case of a clash, leave the [FILL] in and print a warning. *)

  let inferences = fold_left (fun acc cl -> infer_type_of_vars p acc cl) TermMap.empty p.clauses in

  TermMap.iter (fun k v ->
      printf "Infered for %s: " (string_of_term k);
      TypeIdSet.iter (fun t -> printf "%s " t) v;
      printf "\n%!")
    inferences;

  let get_inferred_typestr (c: term): typeid =
    if TermMap.mem c inferences then
    begin
      let theset = TermMap.find c inferences in
        if TypeIdSet.cardinal theset != 1 then
        begin
          printf "Warning: Could not infer type for term: %s; it had more than one inference:\n%!" (string_of_term c);
          TypeIdSet.iter (fun t -> printf ": %s\n%!" t) theset;
          "[FILL]"
        end
        else
        begin
          TypeIdSet.choose theset
        end
    end
    else
    begin
      printf "Warning: Could not infer type for term: %s.\n%!" (string_of_term c);
      "[FILL]"
    end in

  (* Identify the constants (like "0x1001") used and declare them. *)
  (* Need to grab constants from both body and head. E.g., INSERT (10000) into x;*)
  {constants= (map (fun c -> ((alloy_of_term c), (get_inferred_typestr c)))
                (fold_left (fun acc cl -> (unique ( acc @ (get_terms (function | TConst(_) -> true | _ -> false)
                                                            (FAnd(cl.head, cl.body))))))
                           []
                           p.clauses));

   events_used=map (fun edec -> (edec.eventname, edec)) (get_needed_events p);

   tables_used=(map (fun tbl -> (tbl.tablename, tbl)) ((get_local_tables p) @ (get_remote_tables p)));
   filename = ""};;

(* TODO: support table-widening, etc. *)
(* For now, require tables to have same arity/types. *)
let resolve_tables (o1: alloy_ontology) (o2: alloy_ontology): (string * table_def) list =
  fold_left (fun acc tbl ->
              let tbl_n, tbl_def = tbl in
              if mem_assoc tbl_n acc && (assoc tbl_n acc) <> tbl_def then
              begin
                failwith (sprintf "resolve_tables unexpected mismatching arities: %s" tbl_n)
              end
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
    {constants=resolve_constants o1 o2; events_used=resolve_events o1 o2; tables_used=resolve_tables o1 o2;
     filename=""};;

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

(* A pred_fragment describes a single way for a state table or output relation to be modified.
   It is roughly analogous to a Flowlog rule. Pred fragments for the same table will be combined
   into a single Alloy pred. *)
type pred_fragment = {fortable: string option; outrel: string; increl: string; incvar: string;
                      outargs: term list; where: formula};;


let alloy_actions (out: out_channel) (o: alloy_ontology) (p: flowlog_program): unit =

  let make_rule (r: srule): pred_fragment =
    match r.action with
      | ADelete(outrel, outargs, where) ->
        {fortable=Some(outrel);  outrel = (minus_prefix^"_"^outrel); outargs = outargs; where = where; increl = r.onrel; incvar = r.onvar}
      | AInsert(outrel, outargs, where) ->
        {fortable=Some(outrel);  outrel = (plus_prefix^"_"^outrel);  outargs = outargs; where = where; increl = r.onrel; incvar = r.onvar}
      | ADo(outrel, outargs, where) ->
        {fortable=None; outrel = outrel;                    outargs = outargs; where = where; increl = r.onrel; incvar = r.onvar}
      | AForward(pkt, where, tout) ->
        {fortable=None; outrel = "forward";                 outargs = [pkt]; where = where; increl = r.onrel; incvar = r.onvar}
      | AStash(pkt, where, until, thens) ->
        {fortable=None; outrel = "stash";                   outargs = [pkt]; where = where; increl = r.onrel; incvar = r.onvar}
  in

  let outarg_to_poss_equality (evrestricted: string) (i: int) (outarg: term): string =
    match outarg with
      | TField(v, f) -> sprintf "out%d = %s.%s" i evrestricted f (* NOT v and NOT "ev" *)
      | TConst(_) -> sprintf "out%d = %s" i (alloy_of_term outarg)
      | _ -> "true[]"
  in

  let make_quantified_decl (tqs: (term * bool) list): string list =
    (* negative occur of an "any" term becomes universal. TODO: risk of string muddling *)
    let quantify_helper (tq: term*bool) =
      match tq with
        | TVar(vname), false when starts_with vname "any" ->
          printf "... Universally quantifying %s\n%!" vname;
          sprintf "all %s : univ | " (add_freevar_sym_str vname)
        | TVar(vname), _ ->
          printf "... Existentially quantifying %s\n%!" vname;
          sprintf "some %s : univ | " (add_freevar_sym_str vname)
        | _ -> failwith "make_quantified_decl" in

    let trimmed_list = fold_left (fun acc tq ->
                                    let t, sn = tq in
                                      if (exists (fun (ot,_) -> ot = t) acc) then acc else tq::acc)
                         [] tqs in
    map quantify_helper trimmed_list
  in

  let event_alloysig_for (increl: string): string =
    "EV"^increl in

  let build_emit_defaults (pf: pred_fragment) (tid: string): string =
    let ev_def = get_event p tid in
    (*let outv = string_of_term (first pf.outargs) in*)
    let outv = "out0" in
    let constrs = filter_map (fun (fname, _) ->
                  try
                    let v = assoc (tid, fname) Flowlog_Packets.defaults_table in
                      Some (outv^"."^fname^" = "^v)
                  with Not_found -> None) ev_def.evfields in
      String.concat " && " constrs in

  let build_forward_defaults (pf: pred_fragment): string =
    let ev_def = get_event p pf.increl in (* use type from "ON" rather than base packet *)
    let outv = String.lowercase (string_of_term (first pf.outargs)) in
    let terms_used = get_terms (fun _ -> true) pf.where in
    let constrs = filter_map (fun (fname, _) ->
                  let target = TField(outv, fname) in (* using correct outv var*)
                  if not (mem target terms_used) then
                    Some ("out0."^fname^" = "^"ev."^fname)
                  else None) ev_def.evfields in
      String.concat " && " constrs in


  let build_defaults (pf: pred_fragment): string =
    match pf.fortable with
      | Some(_) -> ""
      | None ->
        let out_def = get_outgoing p pf.outrel in
          let defstr = (match out_def.react with
            | OutForward -> (build_forward_defaults pf)
            | OutEmit(tid) -> (build_emit_defaults pf tid)
            | _ -> "") in
              if (String.length defstr) == 0 then ""
              else "&&"^defstr in

  (* Produce an Alloy constraint (string) for this Flowlog rule (as pred_fragment) *)
  let alloy_of_pred_fragment (stateid: string) (pf : pred_fragment): string =
  (* substitute var names: don't get stuck on rules with different args or in var name! *)
    let evtypename = (event_alloysig_for pf.increl) in
    let evrestrictedname = (sprintf "(%s <: ev)" evtypename) in

    (* will include constants as well*)
    let quantified_vars = [TVar(pf.incvar)] @ pf.outargs in

    (* free vars won't be replaced with "outx" or "ev" *)
    let freevars_signed = get_terms_with_sign
      (function | TVar(x) as t -> not (mem t quantified_vars) | _ -> false) true pf.where in

    printf "freevars: %s\n%!"
      (string_of_list ";" (fun (t, s) -> (string_of_term t)^":"^(string_of_bool s)) freevars_signed);


    let to_substitute =
      (* Domain-restrict ev for Alloy's type-checker*)
      [(TVar(pf.incvar), TVar(evrestrictedname))] @
      (* Generic "out" vars. This unifies multiple Flowlog rules with same input/output
         but that use different variable names. Just alpha renaming. *)
      (mapi (fun i outarg ->
        match outarg with | TConst(_) -> (outarg, outarg) | _ -> (outarg, TVar("out"^(string_of_int i))))
      pf.outargs) @
      (* Rule-scope variables: these need alpha-renaming too to avoid clashes with tables, etc. *)
      (map (fun (v, _) -> (v, add_freevar_sym v)) freevars_signed)
       in

    printf "Substituting in WHERE: %s\n%!"
      (string_of_list "; "
        (fun (a,b) -> ((string_of_term a)^"->"^(string_of_term b)))
        to_substitute);

    let substituted = (substitute_terms pf.where to_substitute) in
    (*printf "alloy of formula: %s\n%!" (string_of_formula substituted);*)

    (* If the free var is an ANY, be careful how to bind it.
       If it is an ANY that appears within a negation, must be ALL not EXISTS *)
    (* explicitly quantify rule-scope existentials *)
    let freevarstr = (String.concat " " (make_quantified_decl freevars_signed)) in

    (* Finally, should any defaults be added on? (e.g., new.locsw = p.locsw)*)
    let defaultsstr = build_defaults pf in

      (* Final pred fragment Alloy: *)
      Format.sprintf "\n  (@[ev in %s%s@] && @[(%s %s)@]\n      && @[%s@])"
        evtypename defaultsstr
        freevarstr
        (alloy_of_formula o stateid substituted)

        (* If field of invar in outargs, need to add an equality,
           otherwise connection is lost by alpha renaming. Same thing
           holds for constants. *)
        (String.concat " && " (mapi (outarg_to_poss_equality evrestrictedname) pf.outargs))
  in

  (* Accumulate a map from outrel to rules that contribute*)
  let outrel_to_rules = fold_left (fun acc pf ->
              if StringMap.mem pf.outrel acc then
                StringMap.add pf.outrel (pf :: StringMap.find pf.outrel acc) acc
              else
                StringMap.add pf.outrel [pf] acc)
            StringMap.empty
            (map make_rule (unique (map (fun cl -> cl.orig_rule) p.clauses))) in

  (* out3: Macaddr *)
  let build_arg_decls (outrel: string) (pfl: pred_fragment): string =
    match pfl.fortable with
      | None ->
        (sprintf "out0: %s" (event_alloysig_for (outspec_type (get_outgoing p outrel).react)))
      | Some(orig_rel) ->
        let tarity = ((get_table p orig_rel).tablearity) in
          (String.concat ", " (mapi (fun i t -> sprintf "out%d : %s" i (typestr_to_alloy (nth tarity i)))
                                    pfl.outargs)) in
  (* out3 in Macaddr *)
  let build_arg_force (outrel: string) (pfl: pred_fragment): string =
    match pfl.fortable with
      | None ->
        (sprintf "out0 in %s" (event_alloysig_for (outspec_type (get_outgoing p outrel).react)))
      | Some(orig_rel) ->
        let tarity = ((get_table p orig_rel).tablearity) in
          (String.concat " && " (mapi (fun i t -> sprintf "out%d in %s" i (typestr_to_alloy (nth tarity i)))
                                    pfl.outargs)) in


  (* Convert each outrel (e.g., "forward", "emit") to an Alloy pred declaration string*)
  let rulestrs =
    StringMap.fold (fun outrel (pfls:pred_fragment list) acc ->
                   (* Give a type to the result. Since the pfls should be for the same output, just use the hd. *)
                   let thisargdecls = build_arg_decls outrel (hd pfls) in

                   (* Type annotations get ignored by Alloy (Appendix B.6.4)*)
                   let thisargforce = build_arg_force outrel (hd pfls) in

                   let thispred = sprintf "pred %s[st: State, ev: Event, %s] {\n %s && (%s)\n}\n"
                                    outrel
                                    thisargdecls
                                    thisargforce
                                    (String.concat " ||\n" (map (alloy_of_pred_fragment "st") pfls)) in
                   StringMap.add outrel thispred acc)
                   outrel_to_rules
                   StringMap.empty in
  StringMap.iter (fun outrel predstr -> fprintf out "%s\n%!" predstr) rulestrs;;

(**********************************************************)
(* transition: st x ev x st
   (note this is a slight deviation from the language: packet-in becomes an event) *)

(* construct transition string for a particular table (including plus and minus rules) *)
let build_table_transition (p: flowlog_program) (tbl : table_def): string =
    let tupdvec = (String.concat "," (mapi  (fun i typ -> sprintf "tup%d: %s" i (typestr_to_alloy typ)) tbl.tablearity)) in
    let tupavec = (String.concat "," (init (length tbl.tablearity) (fun i -> sprintf "tup%d" i))) in

    (* - { sw: Switch, sw2: Switch | minus_ucTC[st, ev, sw, sw2] } *)
    let minus_expr = if minus_rule_exists p tbl.tablename then sprintf "- { %s | %s_%s[st1, ev, %s]}" tupdvec minus_prefix tbl.tablename tupavec
                     else "" in
    let plus_expr =  if plus_rule_exists p tbl.tablename then sprintf "+ { %s | %s_%s[st1, ev, %s]}" tupdvec plus_prefix tbl.tablename tupavec
                     else "" in
      sprintf "  st2.%s = (st1.%s\n            %s)\n            %s"
              tbl.tablename tbl.tablename minus_expr plus_expr;;

(* same as build_table_transition, but constructs change-impact strings *)
let build_prestate_table_compare (p1: flowlog_program) (p2: flowlog_program) (_,tbl : string * table_def): string option =
    let tupdvec = (String.concat "," (mapi (fun i typ -> sprintf "tup%d: %s" i (typestr_to_alloy typ)) tbl.tablearity)) in
    let tupavec = (String.concat "," (init (length tbl.tablearity) (fun i -> sprintf "tup%d" i))) in
    let tupproduct = (String.concat "->" (init (length tbl.tablearity) (fun i -> sprintf "tup%d" i))) in

    let construct_compare_frag p1 p2 tbl (modsign: bool): string option =
      let modstr = if modsign then "plus" else "minus" in
      let memstr = if modsign then "not in" else "in" in

    if (mod_rule_exists p1 tbl.tablename modsign) &&
       (mod_rule_exists p2 tbl.tablename modsign)
    then
      Some(sprintf "
{ %s | %s %s prestate.%s && prog1/%s_%s[prestate, ev, %s]} !=
{ %s | %s %s prestate.%s && prog2/%s_%s[prestate, ev, %s]}"
        tupdvec tupproduct memstr tbl.tablename modstr tbl.tablename tupavec
        tupdvec tupproduct memstr tbl.tablename modstr tbl.tablename tupavec)

    else if (mod_rule_exists p2 tbl.tablename modsign) then
      Some(sprintf "some { %s | %s %s prestate.%s && prog2/%s_%s[prestate, ev, %s]}"
        tupdvec tupproduct memstr tbl.tablename modstr tbl.tablename tupavec)
    else if (mod_rule_exists p1 tbl.tablename modsign) then
      Some(sprintf "some { %s | %s %s prestate.%s && prog1/%s_%s[prestate, ev, %s]}"
        tupdvec tupproduct memstr tbl.tablename modstr tbl.tablename tupavec)
    else None in

    let minus_expr = construct_compare_frag p1 p2 tbl false in
    let plus_expr = construct_compare_frag p1 p2 tbl true in
      match plus_expr, minus_expr with
        | Some(minus), Some(plus) -> Some (minus ^ "\n||\n" ^ plus)
        | None, Some(minus) -> Some (minus ^ "\n")
        | Some(plus), None -> Some(plus ^ "\n")
        | _ -> None;;


let alloy_transition (out: out_channel) (p: flowlog_program) (ont: alloy_ontology): unit =
    fprintf out "pred transition[st1: State, ev: Event, st2: State] { \n%!";
    fprintf out "%s\n%!" (String.concat " &&\n\n" (map (build_table_transition p) (map (fun (a,b) -> b) ont.tables_used)));
    fprintf out "}\n\n%!";;

let alloy_outpolicy (out: out_channel) (p: flowlog_program): unit =
  let alloy_out_difference (d: outgoing_def): string option =
    printf "~~ Processing outgoing defn: %s\n%!" d.outname;
    if do_or_forward_rule_exists p d.outname then
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

(* May be standalone or part of change-impact, so need to be able to accept an external ontology *)
let write_as_alloy (p: flowlog_program) (fn: string) (merged_ontology: alloy_ontology option): unit =
  if not (ends_with fn ".als") then
    failwith "Alloy filename must end with .als, so as not to accidently overwrite .flg files.";

    let out = open_out fn in
      fprintf out "module %s\n" (Filename.chop_extension fn);

      let ontology =
        (match merged_ontology with
          | None ->
            let o = (program_to_ontology p) in
              write_alloy_ontology out o;
              o
          | Some(o) ->
            fprintf out "open %s as o\n" o.filename;
            o) in

    	alloy_actions out ontology p;
    	alloy_transition out p ontology;
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

(* In case of ontology mismatch *)
let needed_table_substitutions (p1: flowlog_program) (p2: flowlog_program): ((string * string) list * (string * string) list) =

    let mismatches = fold_left
      (fun acc td ->
        try
          let td2 = (get_table p2 td.tablename) in
            if td <> td2 then (td.tablename)::acc
            else acc
        with
          | _ -> acc) (* no such table in p2 *)
      []
      p1.tables in

    let subs1 = map (fun tn -> (tn, tn^"_1")) mismatches in
    let subs2 = map (fun tn -> (tn, tn^"_2")) mismatches  in

    if (length mismatches) <> 0 then
    begin
      printf "WARNING: The programs had different declared arities for tables %s.\n%!"
        (string_of_list ", " identity mismatches);
      printf "  Both versions of the table will be included in the Alloy model,\n%!";
      printf "  which may result in excess instances and require additional constraints to be added.\n%!";
    end;
    (subs1, subs2);;

let rec subs_table_in_formula (subs: (string * string) list) (f: formula): formula =
    match f with
      | FTrue -> f
      | FFalse -> f
      | FEquals(t1, t2) -> f
      | FIn(t, addr, mask) -> f
      | FNot(f2) -> FNot(subs_table_in_formula subs f2)
      | FAtom(modname, relname, tlargs) when mem_assoc relname subs ->
        FAtom(modname, assoc relname subs, tlargs)
      | FAtom(modname, relname, tlargs) -> f
      | FAnd(f1, f2) ->
        FAnd(subs_table_in_formula subs f1, subs_table_in_formula subs f2)
      | FOr(f1, f2) ->
        FOr(subs_table_in_formula subs f1, subs_table_in_formula subs f2);;

let add_pm_to_subs (subs: (string * string) list): (string * string) list =
  fold_left (fun acc (o,n) ->
    [(o,n);(plus_prefix^"_"^o, plus_prefix^"_"^n);(minus_prefix^"_"^o, minus_prefix^"_"^n)]
    @acc) [] subs;;

let subs_table_in_rule (subs: (string * string) list) (r: srule): srule =
  let apply_subs reln: string =
    if mem_assoc reln subs then assoc reln subs else reln in
  let newaction = (match r.action with
              | ADelete(reln, tl, fmla) ->
                ADelete(apply_subs reln, tl, subs_table_in_formula subs fmla)
              | AInsert(reln, tl, fmla) ->
                AInsert(apply_subs reln, tl, subs_table_in_formula subs fmla)
              | ADo(reln, tl, fmla) ->
                ADo(apply_subs reln, tl, subs_table_in_formula subs fmla)
              | AStash(_,_,_,_) ->
                failwith "stash in subs_table"
              | AForward(t, fmla, iopt) ->
                AForward(t, subs_table_in_formula subs fmla, iopt)) in
  (* assuming triggers won't need substitution *)
  {r with action=newaction};;

let subs_tables_in_clause (subs: (string * string) list) (cl: clause): clause =
  (* plus and minus rules will need some string ops*)
  {orig_rule=subs_table_in_rule subs cl.orig_rule;
   head=subs_table_in_formula (add_pm_to_subs subs) cl.head;
   body=subs_table_in_formula subs cl.body};;

let substitute_tables_in_program (subs: (string * string) list) (p: flowlog_program): flowlog_program =
  (* tablemap *)
  iter (fun (o,n) ->
    let newdef = {(get_table p o) with tablename=n} in
      Hashtbl.remove p.memos.tablemap o;
      Hashtbl.add p.memos.tablemap n newdef)
    subs;

  {p with clauses=map (subs_tables_in_clause subs) p.clauses;

          tables=map (fun td ->
            if mem_assoc (td.tablename) subs then
              {td with tablename=assoc (td.tablename) subs}
            else
              td) p.tables};;

let write_as_alloy_change_impact (orig_p1: flowlog_program) (fn1: string) (orig_p2: flowlog_program) (fn2: string) (reach: bool): unit =

  (* Before anything else, check for conflicts that need resolution via substitution in the programs.
     For instance, TABLE R(macaddr) vs. TABLE R(macaddr, ipaddr). Needs R_1 and R_2 with substitution. *)
  let (subs1, subs2) = needed_table_substitutions orig_p1 orig_p2 in
  let p1 = (substitute_tables_in_program subs1 orig_p1) in
  let p2 = (substitute_tables_in_program subs2 orig_p2) in

  pretty_print_program p1;

  let modname1 = (Filename.chop_extension (Filename.basename fn1)) in
  let modname2 = (Filename.chop_extension (Filename.basename fn2)) in
  let ofn = ("ontology_"^modname1^"_vs_"^modname2) in
  let ontol = {(programs_to_ontology p1 p2) with filename = ofn} in

  write_shared_ontology (ofn^".als") ontol;
  write_as_alloy p1 fn1 (Some(ontol));
  write_as_alloy p2 fn2 (Some(ontol));

  let out = open_out "change_impact.als" in
  if not reach then
  begin
  (* 3 states since prestate, newstate1, newstate 2. *)
  (* 2 events since ev, outev *)
      fprintf out "
module cimp

open %s as o
open %s as prog1
open %s as prog2

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

run changeStateTransition for 4 but 3 State, 1 Event
run changePolicyOutput for 4 but 2 State, 2 Event
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

// Since we are looking for a path to the prestate, and not seeking,
// post-states, we can disregard the possibility of state repeats:
fact noDuplicateStates {
  all s: State | one overall.trace.indsOf[s]
}
// 'one' above covers this
//fact allStatesInSeq {
//  State = overall.trace.elems
//}

fact startingState {
  %s
}


fact orderRespectsTransitionsAndStops {
  all i : overall.trace.inds - overall.trace.lastIdx |
    (some ev: Event | prog1/transition[overall.trace[i], ev, overall.trace[i+1]])
}


////////////////////////////////////////
// Different from atemporal/instantaneous/static change impact:
// Don't want to waste a state or two on the post-states. So re-frame
// changeStateTransition to use plus/minus preds instead.

pred changePolicyOutput[prestate: State, ev: Event] {
    some outev: Event |
      (prog1/outpolicy[prestate, ev, outev] and not prog2/outpolicy[prestate, ev, outev]) ||
      (prog2/outpolicy[prestate, ev, outev] and not prog1/outpolicy[prestate, ev, outev])
}

pred changeImpactLast[ev: Event] {
    some State &&
    (changeStateTransition[overall.trace.last, ev]
    || changePolicyOutput[overall.trace.last, ev])
}

pred changeStateTransition[prestate: State, ev: Event]
{
  %s
}

// If go above 8 (7?) states, be sure to increase the size of int
// seq and State should always have the same bound
run changeImpactLast for 6 but 4 State, 5 Event, 4 seq

/// ^^ This doesn't reflect OSEPL guarantees: need 2x mac per packet, for instance
// do NOT need one per state though. TODO: revise.

\n%!"
  ofn
  (Filename.chop_extension fn1)
  (Filename.chop_extension fn2)
  (build_starting_state_trace ontol)
  (String.concat "||\n" (filter_map (build_prestate_table_compare p1 p2) ontol.tables_used));
  end;
      close_out out;

      (* TODO: not full ceilings yet. Testing... *)
      let ceilings = single_event_ceilings ontol in
        StringMap.iter (fun k v -> printf "Single-event ceiling for %s was %d\n%!" k v) ceilings;

      printf "~~~ Finished change_impact.als query file. ~~~\n%!";;


(* *********************************************************************** *)
