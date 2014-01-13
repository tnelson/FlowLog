(****************************************************************)
(* Most type definitions and a few helpers                      *)
(* Includes both AST-level and program-level types              *)
(****************************************************************)

open Printf
open ExtList.List
open NetCore_Types


(***********************************************)
(* Formula types are shared by AST and program *)
  type term =
              | TConst of string
              | TVar of string
              | TField of string * string;;
  type formula =
              | FTrue
              | FFalse
              | FEquals of term * term
              (* pkt.nwsrc in 10.0.1.1/24 *)
              | FIn of term * term * term
              | FNot of formula
                (* module, relname, args*)
              | FAtom of string * string * term list
              | FAnd of formula * formula
              | FOr of formula * formula;;

type typeid = string;;

(**************************************************)
(* AST-level definitions for syntactic rules, etc.*)

  type action =
              | ADelete of string * term list * formula
              | AInsert of string * term list * formula
              | ADo of string * term list * formula
              (* pkt var to stash; where clause; until clause; then clause*)
              | AStash of term * formula * formula * action list
              | AForward of term * formula * int option;;

  type refresh =
      (* number, units *)
      | RefreshTimeout of int * string
      | RefreshPure
      | RefreshEvery;;

  (*type assignment = {afield: string; atupvar: string};;      *)

  (* SameAsOnFields: Unary relation. Type of argument is same type from the ON trigger.
                     Used by "forward".
     AnyFields: Any arity, any types. Used for behavior like "print"ing.
     Fixedfield: single event type  *)
  type outgoing_fields = | SameAsOnFields | AnyFields | FixedEvent of typeid;;

  type spec_out =
      | OutForward
      | OutEmit of typeid
      | OutLoopback
      | OutPrint
      | OutSend of typeid * string * string;;

  type sreactive =
        (* table name, query name, ip, port, refresh settings *)
      | ReactRemote of string * typeid list * string * string * string * refresh
        (* out relation name, outgoing type, spec*)
      | ReactOut of string * outgoing_fields * spec_out
        (* incoming event type, trigger relation name*)
      | ReactInc of typeid * string;;

  type sdecl =
      | DeclTable of string * typeid list
      | DeclRemoteTable of string * typeid list
      | DeclInc of string * string
      | DeclOut of string * outgoing_fields
      | DeclEvent of string * (string * typeid) list;;

  type srule = {onrel: string; onvar: string; action: action};;

  type stmt =
      | SReactive of sreactive
      | SDecl of sdecl
      | SRule of srule;;

  type flowlog_ast = {includes: string list; statements: stmt list};;

(*************************************************************)

  (* Split out a formula by ORs for use with XSB *)
  (* In new semantics, no longer have "on" in clause body. It just gets made an EDB fact. *)
  (* REQUIRE: head, body fmlas atomic or equality *)
  type clause = { orig_rule: srule;
                  head: formula;
                  body: formula; (* should be always conjunctive *)
                  };;

  (* Every event has a name and a string of fieldnames, each with a type. *)
  type event_def = { eventname: string;
                     evfields: (string * typeid) list };;


  (* partial-evaluation needs to know what variable is being used for the old packet in a clause *)
  type triggered_clause = {clause: clause; oldpkt: string};;

  type outgoing_def = { outname: string;
                        outarity: outgoing_fields;
                        react: spec_out};;

  type queryid = string;;
  type agent = string * string;;
  type table_source = | LocalTable
                      | RemoteTable of queryid * agent * refresh;;

  type table_def = { tablename: string;
                    tablearity: typeid list;
                    source: table_source };;

  (* triggers: inrels -> outrels *)
  (* We use Hashtbl's built in find_all function to extend the values to string list. *)
  type program_memos = {out_triggers: (string, string) Hashtbl.t;
                        insert_triggers: (string, string) Hashtbl.t;
                        delete_triggers: (string, string) Hashtbl.t;
                        tablemap: (string, table_def) Hashtbl.t;
                        eventmap: (string, event_def) Hashtbl.t;
                        outgoingmap: (string, outgoing_def) Hashtbl.t;
                       };;

  type flowlog_program = {  desugared_decls: sdecl list; (* for use in errors, alloy conversion, etc. *)
                            desugared_reacts: sreactive list; (* for use in errors, alloy conversion, etc. *)

                            (* for state tables only (local or remote)*)
                            tables: table_def list;

                            (* events and outgoings will require some built_ins *)
                            (* incomings are entirely defined by event defns *)
                            events: event_def list;
                            outgoings: outgoing_def list;

                            clauses: clause list;

                            (* subsets of <clauses> used to avoid recomputation*)
                            can_fully_compile_to_fwd_clauses: triggered_clause list;
                            weakened_cannot_compile_pt_clauses: triggered_clause list;
                            (* These are *UNWEAKENED* for direct use in XSB. Therefore not same as clauses minus can_fully_compile... *)
                            not_fully_compiled_clauses: clause list;
                            (* Additional info used to avoid recomputation *)
                            memos: program_memos};;

  (* context for values given by decls *)
  module StringMap = Map.Make(String);;
  type event = { typeid: string; values: string StringMap.t};;

  module FmlaMap = Map.Make(struct type t = formula let compare = compare end);;

(*************************************************************)
  let allportsatom = SwitchAction({id with outPort = NetCore_Pattern.All});;

  type printmode = Verbose | Brief;;
  type xsbmode = Xsb | XsbForcePositive | XsbAddUnderscoreVars;;

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
        | FIn(_,_,_) -> []
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

(* If about to substitute in something like 5=7, abort because the whole conjunction is unsatisfiable
   Alternatively, return a FFalse *)
let equals_if_consistent (report_inconsistency: bool) (t1: term) (t2: term): formula =
  match (t1, t2) with
    | (TConst(str1), TConst(str2)) when str1 <> str2 ->
      if report_inconsistency then
        raise (SubstitutionLedToInconsistency((FEquals(t1, t2))))
      else
        FFalse
    | _ -> FEquals(t1, t2);;


let is_in_ip_range (v: Int32.t) (addr: Int32.t) (mask: int): bool =
  (* MAXINT (32 bit) *)
  let nwfield = Int32.of_int (4294967295 lsl (32-mask)) in
    (Int32.logand v nwfield) = (Int32.logand addr nwfield);;

let in_if_consistent (report_inconsistency: bool) (v: term) (addr: term) (mask: term): formula =
  match v, addr, mask with
    | TConst(vval), TConst(addrval), TConst(maskval)
      when not (is_in_ip_range (Int32.of_string vval) (Int32.of_string addrval) (int_of_string maskval)) ->
      if report_inconsistency then
        raise (SubstitutionLedToInconsistency((FIn(v, addr, mask))))
      else
        FFalse
    | _ -> FIn(v, addr, mask);;

(* f[v -> t]
   Substitutions of variables apply to fields of that variable, too. *)
let rec substitute_term (report_inconsistency: bool) (f: formula) (v: term) (t: term): formula =
  let substitute_term_result (curr: term): term =
    match curr, v, t with
      | x, y, _ when x = y -> t
      (* curr is a field of v, replace with field of t *)
      | TField(x, fx), TVar(y), TVar(z) when x = y -> TField(z, fx)
      | _ -> curr in

    match f with
        | FTrue -> f
        | FFalse -> f
        | FEquals(t1, t2) ->
          let st1 = substitute_term_result t1 in
          let st2 = substitute_term_result t2 in
          (* Remove fmlas which will be "x=x"; avoids inf. loop. *)
          if st1 = st2 then FTrue
          else equals_if_consistent report_inconsistency st1 st2
        | FIn(t, addr, mask) ->
            let newt = substitute_term_result t in
            let newaddr = substitute_term_result addr in
            let newmask = substitute_term_result mask in
              in_if_consistent report_inconsistency newt newaddr newmask
        | FAtom(modstr, relstr, argterms) ->
          let newargterms = map (fun arg -> substitute_term_result arg) argterms in
            FAtom(modstr, relstr, newargterms)
        | FOr(f1, f2) ->
            let subs1 = substitute_term report_inconsistency f1 v t in
            let subs2 = substitute_term report_inconsistency f2 v t in
            if subs1 = FFalse then subs2
            else if subs2 = FFalse then subs1
            else FOr(subs1, subs2)
        | FAnd(f1, f2) ->
            (* remove superfluous FTrues/FFalses *)
            let subs1 = substitute_term report_inconsistency f1 v t in
            let subs2 = substitute_term report_inconsistency f2 v t in
            if subs1 = FTrue then subs2
            else if subs2 = FTrue then subs1
            else FAnd(subs1, subs2)
        | FNot(f2) ->
            let innerresult = substitute_term report_inconsistency f2 v t in
            if innerresult = FFalse then FTrue
            else if innerresult = FTrue then FFalse
            else FNot(innerresult);;

let substitute_terms ?(report_inconsistency: bool = true) (f: formula) (subs: (term * term) list): formula =
  fold_left (fun fm (v, t) -> substitute_term report_inconsistency fm v t) f subs;;

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
        let newf = (substitute_term true f x t) in
          (*printf "will subs out %s to %s. New is: %s.\n%!" (string_of_term x) (string_of_term t) (string_of_formula newf);*)
          minimize_variables ~exempt:exempt newf
        with SubstitutionLedToInconsistency(_) -> FFalse;;

let add_conjunct_to_action (act: action) (f: formula) =
  match act with
    | _ when f = FTrue -> act
    | ADelete(a, b, fmla) -> ADelete(a, b, FAnd(f, fmla))
    | AInsert(a, b, fmla) -> AInsert(a, b, FAnd(f, fmla))
    | ADo(a, b, fmla) -> ADo(a, b, FAnd(f, fmla))
    | AForward(p, fmla, tout) -> AForward(p, FAnd(f, fmla), tout)
    | AStash(p, where, until, thens) -> AStash(p, FAnd(f, where), until, thens);;

(*
    | AForward(p, fmla, tout) ->
    | AStash(p, where, until, thens) ->
*)
(************************************************************************************)
(* BUILT-IN CONSTANTS, MAGIC-NUMBERS, EVENTS, REACTIVE DEFINITIONS, ETC.            *)
(* Packet flavors and built-in relations are governed by the Flowlog_Packets        *)
(************************************************************************************)

(* Note: all program text is lowercased by parser *)

(* These are prepended to relation names when we pass insert/delete rules to Prolog *)
let plus_prefix = "plus";;
let minus_prefix = "minus";;
(* Avoid using strings for hard-coded type names and relation names.
   Use these definitions instead. *)
let packet_in_relname = "packet_in";;
let switch_reg_relname = "switch_port_in";;
let switch_down_relname = "switch_down";;
let startup_relname = "startup";;

