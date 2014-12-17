(****************************************************************)
(* Automatic translation from Flowlog to Alloy                  *)
(****************************************************************)

(* TODO: current translation drops destination for "send to" events.
   So two separate named pipes with the same output event type will be
   indistinguishable in this model. *)

(* TODO: well behaved variables facts (e.g., lone VAR and empty VAR -> empty state*)


open Printf
open Flowlog_Types
open Flowlog_Packets
open Flowlog_Helpers
open ExtList.List
open Unix

let alloy_filename (flfn: string): string =
  (Filename.chop_extension flfn)^".als";;

let typestr_to_alloy (fldtype: string): string =
  match fldtype with
  | "string" -> "FLString"
  | "int" -> "FLInt"
  | _ -> String.capitalize fldtype;;

let string_of_bounds (prefix: string) (m: int StringMap.t): string =
  sprintf "0 but %s %s" prefix (String.concat "," (StringMap.fold (fun k v acc -> (sprintf "%d %s" v (typestr_to_alloy k))::acc) m []));;


(**********************************************************)
(* Some boilerplate (packets, etc.) *)

let alloy_ontology_boilerplate (out: out_channel): unit =
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

// May require custom facts, e.g., that there are no cycles
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
  separate_spec: bool;
  constants: (string * typeid) list;
  events_used: (string * event_def) list;
  tables_used: (string * table_def) list;
  inferences: TypeIdSet.t TermMap.t;
}

(**********************************************************)
  let add_freevar_sym_str (v: string): string =
    "var_"^v;;

  let add_freevar_sym (v: term): term =
    match v with
      | TVar(vname) -> TVar(add_freevar_sym_str vname)
      | _ -> failwith (sprintf "add_freevar_sym: %s" (string_of_term v));;

  let format_constant_for_alloy (infers: TypeIdSet.t TermMap.t) (t: term) (strval: string): string =
    if TermMap.mem t infers then
    begin
      let theset = TermMap.find t infers in
        if TypeIdSet.cardinal theset != 1 then strval
        else
        begin
          let tid = TypeIdSet.choose theset in
          let non_alloy_str = pretty_print_value tid strval in
              (* Replace any dots or colons with underscore *)
              (* Replace a negative sign with a "neg"*)
              if tid = "int" then
                tid^"_"^(Str.global_replace (Str.regexp "-") "neg" non_alloy_str)
              else
                tid^"_"^(Str.global_replace (Str.regexp "\\.\\|:") "_" non_alloy_str)
        end
    end
    else
      strval;;

  let alloy_of_term (infers: TypeIdSet.t TermMap.t) ?(any: bool = true) (t: term): string =
    match t with
      | TConst(s) -> "C_"^(format_constant_for_alloy infers t s)
      | TVar(s) when any && (starts_with s "var_any") -> "univ"
      | TVar(s) -> s
      | TField(varname, fname) ->
        (varname^"."^fname);;

  let rec reduce_relation_expr (ontol: alloy_ontology) (startwith: string) (tlargs: term list): string =
    match tlargs with
      | arg::[] ->
        let lastterm = (alloy_of_term ontol.inferences ~any:true arg) in
          if lastterm = "univ" then
            sprintf "some %s" startwith
          else
            sprintf "%s in %s" lastterm startwith
      | arg::rest ->
        (reduce_relation_expr ontol (sprintf "%s[%s]" startwith (alloy_of_term ontol.inferences ~any:true arg)) rest)
      | [] -> failwith "reduce_relation_expr empty list";;

  let rec alloy_of_formula (o: alloy_ontology) (stateid: string) (f: formula): string =
    match f with
      | FTrue -> "true[]"
      | FFalse -> "false[]"
      | FEquals(t1, t2) ->
        (alloy_of_term o.inferences t1) ^ " = "^ (alloy_of_term o.inferences t2)
      | FIn(t, addr, mask) ->
        sprintf "%s -> %s -> %s in in_ipv4_range"
          (alloy_of_term o.inferences t) (alloy_of_term o.inferences addr) (alloy_of_term o.inferences mask)
      | FNot(f2) ->  "not ("^(alloy_of_formula o stateid f2)^")"
      | FAtom("", relname, tlargs) when (exists (fun (tname, _) -> tname=relname) o.tables_used) ->

(*
// 100ms
// ((((((((((not ((EVarp_packet <: ev).arp_tpa->var_any3 in st.cached) && not ((EVarp_packet <: ev).arp_tpa in st.queued.univ.univ.univ.univ)) &&
// 6 sec
// ((((((((((not ((EVarp_packet <: ev).arp_tpa->var_any3 in st.cached) && not (some x1,x2,x3,x4: univ | ((EVarp_packet <: ev).arp_tpa->x1->x2->x3->x4 in st.queued))) &&
*)
      (* Incredibly inefficient in translation vs. relational operators if ANYs are broken out and quantified *)
          (* (String.concat "->" (map alloy_of_term tlargs))^" in "^stateid^"."^relname*)
      (* Instead, use join as much as possible *)
        (reduce_relation_expr o (stateid^"."^relname) tlargs)

      | FAtom("", relname, tlargs) ->
          let onto_prefix = if o.separate_spec then "o/" else "" in
            (String.concat "->" (map (alloy_of_term o.inferences) tlargs))^" in "^onto_prefix^"BuiltIns."^relname
      | FAtom(modname, relname, tlargs) ->
          (String.concat "->" (map (alloy_of_term o.inferences) tlargs))^" in "^stateid^"."^modname^"_"^relname
      | FAnd(f1, f2) ->
        (Format.sprintf "(@[%s@] && @[%s@])"
          (alloy_of_formula o stateid f1)
          (alloy_of_formula o stateid f2))
      | FOr(f1, f2) -> "("^(alloy_of_formula o stateid f1) ^ " || "^ (alloy_of_formula o stateid f2)^")"


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
      (* keep lowercase for combination with var counts *)
      (*let alloyt = typestr_to_alloy t in*)
      let alloyt = t in
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
  alloy_ontology_boilerplate out;

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

  let inferences = fold_left (fun acc cl -> type_inference_of_vars p acc cl) TermMap.empty p.clauses in

  TermMap.iter (fun k v ->
      printf "Infered for %s: " (string_of_term k);
      TypeIdSet.iter (fun t -> printf "%s " t) v;
      printf "\n%!")
    inferences;

  let get_inferred_typeid (c: term): typeid =
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
  {constants= (map (fun c -> ((alloy_of_term inferences c), (typestr_to_alloy (get_inferred_typeid c))))
                (fold_left (fun acc cl -> (unique ( acc @ (get_terms (function | TConst(_) -> true | _ -> false)
                                                            (FAnd(cl.head, cl.body))))))
                           []
                           p.clauses));

   events_used=map (fun edec -> (edec.eventname, edec)) (get_needed_events p);

   tables_used=(map (fun tbl -> (tbl.tablename, tbl)) ((get_local_tables p) @ (get_remote_tables p)));
   filename = "";
   separate_spec = false; (* will set to true later if multiple files *)
   inferences=inferences};;

(* TODO: support table-widening, etc. *)
(* For now, require tables to have same arity/types. *)
let resolve_tables (ontos: alloy_ontology list): (string * table_def) list =
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
            [] (fold_left (fun acc ont -> acc @ ont.tables_used) [] ontos);;

(* TODO: duplicate code in resolve funcs *)
(* EVENTS need to have the same shape/fields *)
let resolve_events (ontos: alloy_ontology list): (string * event_def) list =
  fold_left (fun acc ev ->
              let ev_n, ev_def = ev in
              if mem_assoc ev_n acc && (assoc ev_n acc) <> ev_def then
                failwith (sprintf "The programs had different notions of event %s" ev_n)
              else if mem_assoc ev_n acc then
                acc
              else
                ev :: acc)
            [] (fold_left (fun acc ont -> acc @ ont.events_used) [] ontos);;

(* Better agree on types! o.constants is an association list. *)
let resolve_constants (ontos: alloy_ontology list): (string * typeid) list =
  fold_left (fun acc con ->
              let con_n, con_t = con in
              if mem_assoc con_n acc && assoc con_n acc <> con_t then
                failwith (sprintf "The programs had different inferred types for constant %s: %s vs. %s" con_n con_t (assoc con_n acc))
              else if mem_assoc con_n acc then
                acc
              else
                con :: acc)
            [] (fold_left (fun acc ont -> acc @ ont.constants) [] ontos);;


let programs_to_ontology (programs: flowlog_program list): alloy_ontology =
  (* Detect conflicts + combine *)
  let ontos = map program_to_ontology programs in
    {constants=resolve_constants ontos; events_used=resolve_events ontos; tables_used=resolve_tables ontos;
     filename="";
     separate_spec=true;
     inferences=(fold_left (fun acc ont -> combine_inferences ont.inferences acc) TermMap.empty ontos)};;

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
      | TConst(_) -> sprintf "out%d = %s" i (alloy_of_term o.inferences outarg)
      | _ -> "true[]"
  in

  let make_quantified_decl (tqs: (term * bool) list): string list =
    (* negative occur of an "any" term becomes universal. TODO: risk of string muddling *)
    let quantify_helper (tq: term*bool) =
      match tq with
        | TVar(vname), false when starts_with vname "any" ->
          (* printf "... Universally quantifying %s\n%!" vname;
          sprintf "all %s : univ | " (add_freevar_sym_str vname)*)
          "" (* No longer quantify ANY. They are universally interpreted, but
                can be represented much more efficiently via dot-join. *)

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

  let build_emit_defaults (pf: pred_fragment) (tid: string): formula =
    let ev_def = get_event p tid in
    let outv = string_of_term (first pf.outargs) in
    (*let outv = "out0" in*)
    let constrs = filter_map (fun (fname, _) ->
                  try
                    let v = assoc (tid, fname) Flowlog_Packets.defaults_table in
                      Some(FEquals(TField(outv, fname),TConst(v)))
                  with Not_found -> None) ev_def.evfields in
      build_and constrs in

  let build_forward_defaults (pf: pred_fragment): formula =
    let ev_def = get_event p pf.increl in (* use type from "ON" rather than base packet *)
    let outv = String.lowercase (string_of_term (first pf.outargs)) in
    let terms_used = get_terms (fun _ -> true) pf.where in
    let constrs = filter_map (fun (fname, _) ->
                  let target = TField(outv, fname) in (* using correct outv var*)
                  if not (mem target terms_used) then
                    Some(FEquals(TField(outv, fname),TField(pf.incvar,fname)))
                  else None) ev_def.evfields in
      build_and constrs in


  let build_defaults (pf: pred_fragment): formula =
    match pf.fortable with
      | Some(_) -> FTrue
      | None ->
        let out_def = get_outgoing p pf.outrel in
          (match out_def.react with
            | OutForward -> (build_forward_defaults pf)
            | OutEmit(tid) -> (build_emit_defaults pf tid)
            | _ -> FTrue) in

  (* Produce an Alloy constraint (string) for this Flowlog rule (as pred_fragment) *)
  let alloy_of_pred_fragment (stateid: string) (pf : pred_fragment): string =
  (* substitute var names: don't get stuck on rules with different args or in var name! *)
    let evtypename = (event_alloysig_for pf.increl) in
    let evrestrictedname = (sprintf "(%s <: ev)" evtypename) in

    (* will include constants as well*)
    (* TODO: shouldn't this be called "free vars"? *)
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
    let defaultsfmla = (substitute_terms (build_defaults pf) to_substitute) in

      (* Final pred fragment Alloy: *)
      Format.sprintf "\n  (@[ev in %s && %s@] && @[(%s %s)@]\n      && @[%s@])"
        evtypename
        (alloy_of_formula o stateid defaultsfmla)
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
  let build_arg_decls (outrel: string) (pfl: pred_fragment) (useincrel: bool): string =
    match pfl.fortable with
      | None ->
        (sprintf "out0: %s" (event_alloysig_for (outspec_type (get_outgoing p outrel).react pfl.increl useincrel)))
      | Some(orig_rel) ->
        let tarity = ((get_table p orig_rel).tablearity) in
          (String.concat ", " (mapi (fun i t -> sprintf "out%d : %s" i (typestr_to_alloy (nth tarity i)))
                                    pfl.outargs)) in

  (* out3 *)
  let build_arg_vector (pfl: pred_fragment): string =
    match pfl.fortable with
      | None ->
        "out0"
      | Some(orig_rel) ->
          (String.concat ", " (mapi (fun i t -> sprintf "out%d" i) pfl.outargs)) in

  (* out3 in Macaddr *)
  let build_arg_force (outrel: string) (pfl: pred_fragment): string =
    match pfl.fortable with
      | None ->
        (sprintf "out0 in %s" (event_alloysig_for (outspec_type (get_outgoing p outrel).react pfl.increl true)))
      | Some(orig_rel) ->
        let tarity = ((get_table p orig_rel).tablearity) in
          (String.concat " && " (mapi (fun i t -> sprintf "out%d in %s" i (typestr_to_alloy (nth tarity i)))
                                    pfl.outargs)) in


  (* Convert each outrel (e.g., "forward", "emit") to an Alloy pred declaration string*)
  let rulestrs =
    StringMap.fold (fun outrel (pfls:pred_fragment list) acc ->

                    let pred_decls, invocations = split (mapi (fun i pfl ->
                      let thisargdecls = build_arg_decls outrel pfl true in
                      let thisargvector = build_arg_vector pfl in
                      (* Type annotations get ignored by Alloy (Appendix B.6.4)*)
                      let thisargforce = build_arg_force outrel pfl in
                      let pred_decl = sprintf "pred %s_%d[st: State, ev: Event, %s] {\n %s && (%s)\n}\n"
                                        outrel i thisargdecls thisargforce
                                        (alloy_of_pred_fragment "st" pfl) in
                      let invocation = sprintf "%s_%d[st,ev,%s]" outrel i thisargvector in
                        (pred_decl, invocation)) pfls) in

                   let thispred = sprintf "pred %s[st: State, ev: Event, %s] {\n %s\n}\n%s\n"
                                    outrel
                                    (build_arg_decls outrel (hd pfls) false)
                                    (String.concat " ||\n" invocations)
                                    (String.concat "\n" pred_decls) in
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
{ %s | %s %s prestate1.%s && prog1/%s_%s[prestate1, ev, %s]} !=
{ %s | %s %s prestate2.%s && prog2/%s_%s[prestate2, ev, %s]}"
        tupdvec tupproduct memstr tbl.tablename modstr tbl.tablename tupavec
        tupdvec tupproduct memstr tbl.tablename modstr tbl.tablename tupavec)

    (* Only one program has a mod rule for this table; it's a diff if it ever _changes_
       t in and removed; or t not in and added *)
    else if (mod_rule_exists p2 tbl.tablename modsign) then
      Some(sprintf "some { %s | %s %s prestate2.%s && prog2/%s_%s[prestate2, ev, %s]}"
        tupdvec tupproduct memstr tbl.tablename modstr tbl.tablename tupavec)
    else if (mod_rule_exists p1 tbl.tablename modsign) then
      Some(sprintf "some { %s | %s %s prestate1.%s && prog1/%s_%s[prestate1, ev, %s]}"
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

       let (pvs: vars_count_report) = (get_program_var_counts p) in
       let (sevc: int StringMap.t) = (single_event_ceilings ontology) in
        printf "PVS: %s\n%!" (string_of_varcount pvs);
        printf "SEVC: %s\n%!" (string_of_bounds "" sevc);


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
  let tracestrs = map (fun (n,_ ) -> sprintf "no overall.trace1.first.%s" n) ontol.tables_used in
  String.concat "\n" tracestrs;;

let accumulate_defs (acc: (table_def list) StringMap.t) (prog: flowlog_program): (table_def list) StringMap.t =
  fold_left
      (fun acc2 td ->
        match StringMap.mem td.tablename acc2 with
          | true when mem td (StringMap.find td.tablename acc2) -> acc2
          | true -> StringMap.add td.tablename (td::(StringMap.find td.tablename acc2)) acc2
          | false -> StringMap.add td.tablename [td] acc2)
      acc
      prog.tables;;

(* In case of ontology mismatch *)
let needed_table_substitutions (progs: flowlog_program list): (string * string) list list =
    let accumulated_table_defs = (fold_left accumulate_defs StringMap.empty progs) in
    let mismatches = StringMap.fold (fun tname defs acc -> if (length defs) > 1 then tname::acc else acc)
                       accumulated_table_defs [] in

    (* One list of str->str for each program, saying how to rename the clashing tables *)
    let subs = mapi (fun i _ -> (map (fun tn -> (tn, tn^"_"^(string_of_int (i+1)))) mismatches)) progs in

    if (length mismatches) <> 0 then
    begin
      printf "WARNING: The programs had different declared arities for tables %s.\n%!"
        (string_of_list ", " identity mismatches);
      printf "  All versions of the table will be included in the Alloy model,\n%!";
      printf "  which may result in excess instances and require additional constraints to be added.\n%!";
    end;

    subs;;

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

(*******************************************************************************************)

(* Produce bounds from varcounts *)

(* cpo = 1 State, 2 Event, 2x(Event ceiling) +
   act2-E + act2-U + act2-E + act2-U
   The Us are included because negation:

 some outev: Event |
      (prog1/outpolicy[prestate, ev, outev] and not prog2/outpolicy[prestate, ev, outev]) ||
      (prog2/outpolicy[prestate, ev, outev] and not prog1/outpolicy[prestate, ev, outev])

     *)

(* cst = 3 State, 1 Event, 1x(Event ceiling) +
      transition1-e + transition2-e

     (prog1/transition[prestate, ev, newst1] and
     prog2/transition[prestate, ev, newst2])

   *)

let mulbounds (m: int StringMap.t) (mul: int): int StringMap.t =
  StringMap.map (fun v -> mul*v) m;;

let addbounds (m1: int StringMap.t) (m2: int StringMap.t): int StringMap.t =
  (* note the ordering in the fold function. m2 serves as the initial acc. *)
  StringMap.fold (fun k v acc ->
    match StringMap.mem k acc with
      | true -> StringMap.add k ((StringMap.find k acc)+v) acc
      | false -> StringMap.add k v acc)
    m1 m2;;

(******************************************************************)
let cpo_bounds_string (p1: flowlog_program) (p2: flowlog_program) (ontol: alloy_ontology): string =
  let (p1vs: vars_count_report) = (get_program_var_counts p1) in
  let (p2vs: vars_count_report) = (get_program_var_counts p2) in
  let (ceilings: int StringMap.t) = (single_event_ceilings ontol) in
  let (p1ext: int StringMap.t) = p1vs.action_qvars_ext in
  let (p2ext: int StringMap.t) = p2vs.action_qvars_ext in
  let (p1uni: int StringMap.t) = p1vs.action_qvars_uni in
  let (p2uni: int StringMap.t) = p2vs.action_qvars_uni in

  (* Need 2 events! *)
  let final = (addbounds (addbounds (addbounds (addbounds (mulbounds ceilings 2) p1ext) p2ext) p1uni) p2uni) in
  string_of_bounds "1 State, 2 Event," final;;

let cst_bounds_string (p1: flowlog_program) (p2: flowlog_program) (ontol: alloy_ontology): string =
  let (p1vs: vars_count_report) = (get_program_var_counts p1) in
  let (p2vs: vars_count_report) = (get_program_var_counts p2) in
  let (ceilings: int StringMap.t) = (single_event_ceilings ontol) in
  let (p1ext: int StringMap.t) = p1vs.transition_qvars_ext in
  let (p2ext: int StringMap.t) = p2vs.transition_qvars_ext in

  (* Need 3 States, but only one Event. *)
  let final = (addbounds (addbounds ceilings p1ext) p2ext) in
  string_of_bounds "3 State, 1 Event," final;;

(* seq must be max sequence length (max of bounds on events and states). must also have enough ints for the indices
   ints are 2s-complement, so the default 3 int only gets you {-4, -3, -2, -1, 0, 1, 2, 3} *)
let rcst_bounds_string (ontol: alloy_ontology): string =
  let (ceilings: int StringMap.t) = (single_event_ceilings ontol) in
  let final = (mulbounds ceilings 4) in
  string_of_bounds "3 State, 4 Event, 4 seq, 4 int, " final;;

let rcpo_bounds_string (ontol: alloy_ontology): string =
  let (ceilings: int StringMap.t) = (single_event_ceilings ontol) in
  let final = (mulbounds ceilings 4) in
  string_of_bounds "3 State, 4 Event, 4 seq, 4 int, " final;;


(*******************************************************************************************)
let build_diff_preds (ontol: alloy_ontology) (p1: flowlog_program) (p2: flowlog_program): string =
  sprintf
"
// These automatically-generated predicates are linked to the first and second programs
// given. To reference others instead, just change the 'prog1' and 'prog2' to the appropriate
// identifiers. (See the top of this module for filename vs. identifier mapping.) The predicates
// are otherwise identical!
//
// Sufficient bounds for each can be obtained by calling
// rcpo_bounds_string and
// rcst_bounds_string
// with the appropriate programs.
// (The bounds may vary by program since they depend on the variables used in each rule.)
//
// If doing delta-of-delta, SUM the necessary bounds:
// CPO for (prog1 vs prog2) vs (prog1 vs prog3) would use the sum of the bounds for the two pairs.
//
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

run changeStateTransition for %s
run changePolicyOutput for %s"
  (cst_bounds_string p1 p2 ontol)
  (cpo_bounds_string p1 p2 ontol);;

let write_as_alloy_change_impact (orig_progs: (flowlog_program * string) list) (reach: bool): unit =

  (* Before anything else, check for conflicts that need resolution via substitution in the programs.
     For instance, TABLE R(macaddr) vs. TABLE R(macaddr, ipaddr). Needs R_1 and R_2 with substitution. *)
  let orig_progs_solo,_ = split orig_progs in
  let subs = needed_table_substitutions orig_progs_solo in
  let progs = (map (fun (s,(p,fn)) -> ((substitute_tables_in_program s p),fn)) (combine subs orig_progs)) in

  (*pretty_print_program (hd progs);*)

  let ofn = "ontology_"^
    (string_of_list "_" (fun (op, fn) ->
        (Filename.chop_extension (Filename.basename fn))) orig_progs) in
  let ontol = {(programs_to_ontology (map (fun (p,fn) -> p) progs)) with filename = ofn} in

  write_shared_ontology (ofn^".als") ontol;
  iter (fun (p,fn) ->  write_as_alloy p fn (Some(ontol))) progs;

  let p1,_ = (hd progs) in
  let p2,_ = (hd (tl progs)) in

  let out = open_out "change_impact.als" in
  if not reach then
  begin
  (* 3 states since prestate, newstate1, newstate 2. *)
  (* 2 events since ev, outev *)
      fprintf out "
module cimp

open %s as o
%s

%s
\n%!"
  ofn
  (string_of_list "\n" identity (mapi (fun i (_,fn) -> sprintf "open %s as prog%d" (Filename.chop_extension fn) (i+1)) progs))
  (build_diff_preds ontol p1 p2);
  end
  else
  begin
    (* with reachability *)
    fprintf out "
module cimp

open %s as o
%s

/////////////////////////////////////////////////

one sig overall {
  // Ordering won't allow repeats; we need to allow repeats
  trace1, trace2: seq State,
  // The events used to take us to trace1.last and trace2.last. The change-impact event comes after.
  traceevents: seq Event
}{
  // Both start at starting state
  trace1.first = trace2.first
  // Both are the same length
  #trace1 = #trace2
  // The scenario will have no extra events (the trace +1 for the change impact event)
  #traceevents = #trace1 - 1
}

// Misnamed fact; read comments.
fact noDuplicateStates {

  // every state is used
  State = overall.trace1.elems + overall.trace2.elems

  // Cannot rule out repeats in general:
  // (1) startup event MUST be first, and it often doesn't change the state
  // (2) Consider {0->1->0->1 ; 0->1->2->0. CST will detect on state 3 (one trace no dupes)
  //   but *CPO* might only fire on the fourth (both need dupes to get there)}
}

fact orderRespectsTransitionsAndStops {
  // same length, so can share \"i\". same event.
  // refactored whole fact
  all i : overall.trace1.inds - overall.trace1.lastIdx | {
    prog1/transition[overall.trace1[i], overall.traceevents[i], overall.trace1[i+1]]
      prog2/transition[overall.trace2[i], overall.traceevents[i], overall.trace2[i+1]] }
  // the first event is always the startup event
  overall.traceevents.first in EVstartup
}

/////////////////////////////////////////////////

fact startingState {
  %s
}

////////////////////////////////////////
// Different from atemporal/instantaneous/static change impact:
// Don't want to waste a state or two on the post-states. So re-frame
// changeStateTransition to use plus/minus preds instead.

pred reachChangePolicyOutput[prestate1, prestate2: State, ev: Event] {
    some outev: Event |
      (prog1/outpolicy[prestate1, ev, outev] and not prog2/outpolicy[prestate2, ev, outev]) ||
      (prog2/outpolicy[prestate2, ev, outev] and not prog1/outpolicy[prestate1, ev, outev])
}

pred reachChangeStateTransition[prestate1, prestate2: State, ev: Event]
{
  %s
}

// @@@ ev isn't necessarily in overall.traceevents; it fires on the final states
pred reachCSTLast[ev: Event] {
  some State
  reachChangeStateTransition[overall.trace1.last, overall.trace2.last, ev]
}
pred reachCPOLast[ev: Event] {
  some State
  reachChangePolicyOutput[overall.trace1.last, overall.trace2.last, ev]
}

// If go above 8 (7?) states, be sure to increase the size of int
// seq and State should always have the same bound
// ^ Not going to get this far due to Kodkod's MAXINT

run reachCPOLast for %s

run reachCSTLast for %s

  /// ^^ Warning: there are no OSEPL guarantees for reachability-aware predicates!
  /// It is possible to generate bounds for individual steps, but beyond 1 or 2 states
  /// either scales poorly or pushes us over Alloy's MAXINT limitations. Instead, pick
  /// a reasonable bound: here enough for 4 TCP packets. Since existential values
  /// in the state have to come from an event (modulo var declarations) this should
  /// be a complete check up to 3 states (2 steps)

\n%!"
  ofn
  (string_of_list "\n" identity (mapi (fun i (_,fn) -> sprintf "open %s as prog%d" (Filename.chop_extension fn) (i+1)) progs))
  (build_starting_state_trace ontol)
  (String.concat "||\n" (filter_map (build_prestate_table_compare p1 p2) ontol.tables_used))
  (rcst_bounds_string ontol)
  (rcpo_bounds_string ontol);

  end;
    (* end of branch by reach/nonreach *)

    (* output how many of each type one needs PER EVENT for completeness *)
    StringMap.iter (fun k v -> fprintf out "// Single-event ceiling for %s was %d\n%!" k v)
      (single_event_ceilings ontol);

    (* output how many of each type one needs PER EVENT for completeness *)
    fprintf out "\n/* VARCOUNTS p1:\n %s */\n" (string_of_varcount (get_program_var_counts p1));
    fprintf out "\n/* VARCOUNTS p2:\n %s */\n" (string_of_varcount (get_program_var_counts p2));

    (* IMPORTANT: Completeness guarantees depend on the shape of the predicate(s) being checked.
       Just adding up these numbers is not sufficient.

       Note further that Flowlog eliminates quantified variables that it can show unnecessary.
       For instance a clause R(x) :- x=y and P(y) will be reduced to involve only x. This happens more
       often than you'd think, since we often separate extracting data from tables in the ON's WHERE
       and using that extracted data later. (See NAT example.)*)

    close_out out;
    printf "~~~ Finished change_impact.als query file. ~~~\n%!";;


(* *********************************************************************** *)




