open Flowlog_Types
open ExtList.List
open Printf
open NetCore_Types

(* output verbosity *)
(* 0 = default, no debug info at all *)
(* 2 adds XSB listings between events *)
(* 3 adds weakening and partial-evaluation info *)
(* 10 = even XSB messages *)
let global_verbose = ref 0;;

let global_unsafe = ref false;;

exception UndeclaredIncomingRelation of string;;
exception UndeclaredOutgoingRelation of string;;
exception UndeclaredTable of string;;
exception BadBuiltInUse of string;;
exception BadArityOfTable of string;;
exception UndeclaredField of string * string;;
exception NonCondensedNoField of string;;
exception RelationHadMultipleReacts of string;;
exception RelationHadMultipleDecls of string;;
exception RelationDeclClashesWithBuiltin of string;;
exception NoDefaultForField of string * string;;
exception ReqDifferentOutputVar of string;;

let out_log = ref None;;

let write_log (ln: string): unit =
  match !out_log with
  | None -> printf "Unable to write to log file.\n%!"
  | Some(out) -> fprintf out "%s\n%!" ln;;

let string_of_list (sep: string) (pro: 'a -> string) (alist: 'a list): string =
  (String.concat sep (map pro alist));;

let string_of_list_list (sep: string) (sep2: string) (pro: 'a -> string) (alist: 'a list list): string =
  (String.concat sep (map (string_of_list sep2 pro) alist));;

let identity (x: 'a): 'a = x;;

  let string_of_term ?(verbose:printmode = Brief) (t: term): string =
    match t with
      | TConst(s) ->
        if verbose = Verbose then "TConst("^s^")"
        else s
      | TVar(s) ->
        (match verbose with
          | Verbose -> "TVar("^s^")"
          |  _ -> (String.uppercase s))
      | TField(varname, fname) ->
        (match verbose with
          | Verbose -> "TField("^varname^"."^fname^")"
          | _ -> (String.uppercase (varname^"__"^fname)));;

  let rec string_of_formula ?(verbose:printmode = Brief) (f: formula): string =
    match f with
      | FTrue -> "true"
      | FFalse -> "false"
      | FEquals(t1, t2) -> (string_of_term ~verbose:verbose t1) ^ " = "^ (string_of_term ~verbose:verbose t2)
      | FIn(t, addr, mask) ->
          (string_of_term ~verbose:verbose t) ^ " IN "^ (string_of_term ~verbose:verbose addr) ^ "/" ^ (string_of_term ~verbose:verbose mask)
      | FNot(f) ->
        "(not "^(string_of_formula ~verbose:verbose f)^")"
      | FAtom("", relname, tlargs) ->
          relname^"("^(String.concat "," (map (string_of_term ~verbose:verbose) tlargs))^")"
      | FAtom(modname, relname, tlargs) ->
          modname^"/"^relname^"("^(String.concat "," (map (string_of_term ~verbose:verbose) tlargs))^")"
      | FAnd(f1, f2) -> (string_of_formula ~verbose:verbose f1) ^ ", "^ (string_of_formula ~verbose:verbose f2)
      | FOr(f1, f2) -> (string_of_formula ~verbose:verbose f1) ^ " or "^ (string_of_formula ~verbose:verbose f2);;

(* True if string str1 ends with string str2 *)
let ends_with (str1 : string) (str2 : string) : bool =
	if String.length str2 > String.length str1 then false
    else (String.sub str1 ((String.length str1) - (String.length str2)) (String.length str2)) = str2;;

(* ends_with plus mirror-universe beard*)
let starts_with (str1 : string) (str2 : string) : bool =
  if String.length str2 > String.length str1 then false
    else (String.sub str1 0 (String.length str2)) = str2;;

let is_positive_atom (f: formula): bool =
  match f with
    | FAtom(_,_,_) -> true
    | _ -> false;;

let is_ANY_term (t: term): bool =
  match t with
    | TVar(x) when (starts_with x "any") -> true
    | _ -> false;;

let is_variable_term (t: term): bool =
  match t with
    | TVar(x) -> true
    | _ -> false;;

let is_constant_term (t: term): bool =
  match t with
    | TConst(x) -> true
    | _ -> false;;

let is_dltyp_assign (f: formula): bool =
  match f with
    | FEquals(TField(_, "dltyp"), TConst(_))
    | FEquals(TConst(_), TField(_, "dltyp")) -> true
    | _ -> false;;

let is_dltyp_neg (f: formula): bool =
  match f with
    | FNot(FEquals(TField(_, "dltyp"), TConst(_)))
    | FNot(FEquals(TConst(_), TField(_, "dltyp"))) -> true
    | _ -> false;;

let is_nwproto_assign (f: formula): bool =
  match f with
    | FEquals(TField(_, "nwproto"), TConst(_))
    | FEquals(TConst(_), TField(_, "nwproto")) -> true
    | _ -> false;;

let is_nwproto_neg (f: formula): bool =
  match f with
    | FNot(FEquals(TField(_, "nwproto"), TConst(_)))
    | FNot(FEquals(TConst(_), TField(_, "nwproto"))) -> true
    | _ -> false;;

(* expects no atomic rel fmlas! *)
let rec has_tp_field_reference (f: formula): bool =
  match f with
    | FEquals(TField(_, "tpsrc"), _)
    | FEquals(_, TField(_, "tpsrc"))
    | FEquals(TField(_, "tpdst"), _)
    | FEquals(_, TField(_, "tpdst")) -> true

    | FNot(f2) -> has_tp_field_reference f2
    | FAnd(f2, f3) -> has_tp_field_reference f2 || has_tp_field_reference f3
    | FOr(f2, f3) -> has_tp_field_reference f2 || has_tp_field_reference f3
    | _ -> false;;
let rec has_nw_field_reference (f: formula): bool =
  match f with
    | FEquals(TField(_, "nwsrc"), _)
    | FEquals(_, TField(_, "nwsrc"))
    | FEquals(TField(_, "nwdst"), _)
    | FEquals(_, TField(_, "nwdst"))
    | FIn(TField(_, "nwsrc"), _, _)
    | FIn(TField(_, "nwdst"), _, _) -> true

    | FNot(f2) -> has_nw_field_reference f2
    | FAnd(f2, f3) -> has_nw_field_reference f2 || has_nw_field_reference f3
    | FOr(f2, f3) -> has_nw_field_reference f2 || has_nw_field_reference f3
    | _ -> false;;


let validate_ordering (eqlist: formula list) =
  (* fold_left to follow ordering *)
  fold_left (fun (seendltyp, seennwproto) eqfmla ->
     if is_dltyp_assign eqfmla then (true, seennwproto)
     else if is_nwproto_assign eqfmla then (seendltyp, true)
     else if (not seendltyp) && has_nw_field_reference eqfmla then
       failwith (sprintf "validate_ordering (dltyp): %s" (String.concat "," (map string_of_formula eqlist)))
     else if ((not seennwproto) || (not seendltyp)) && has_tp_field_reference eqfmla then
       failwith (sprintf "validate_ordering (nwproto): %s" (String.concat "," (map string_of_formula eqlist)))
     else (seendltyp, seennwproto))
    (false, false)
    eqlist;;

let field_order = Hashtbl.create(10);;
Hashtbl.add field_order "locsw" 0;;
Hashtbl.add field_order "locpt" 2;;
Hashtbl.add field_order "dltyp" 4;;
Hashtbl.add field_order "nwproto" 6;;
Hashtbl.add field_order "dlsrc" 8;;
Hashtbl.add field_order "dldst" 10;;
Hashtbl.add field_order "nwsrc" 12;;
Hashtbl.add field_order "nwdst" 14;;
Hashtbl.add field_order "tpsrc" 16;;
Hashtbl.add field_order "tpdst" 18;;
(* others unordered *)

let get_field_order_index (fmla: formula): int =
  match fmla with
    | FEquals(TField(_, fld), _)
    | FEquals(_, TField(_, fld)) ->
      if Hashtbl.mem field_order fld then Hashtbl.find field_order fld
      else 100
    | FNot(FEquals(TField(_, fld), _))
    | FNot(FEquals(_, TField(_, fld))) ->
      if Hashtbl.mem field_order fld then (Hashtbl.find field_order fld) + 1
      else 101
    | _ -> 200;;

(* If we have nwSrc before dltyp (for instance), NetCore produces unsound results.
   Even more, NetCore is sensitive to ordering within conjunctions, etc.
   While this ordering is not guaranteed to be optimal, it is guaranteed to be sound
   (and in practice, seems to lead to reasonably efficient flow tables.) *)
let order_clause_conjunct (f1: formula) (f2: formula): int =
  let idx1 = get_field_order_index f1 in
  let idx2 = get_field_order_index f2 in
  if idx1 - idx2 > 0 then 1
  else -1;;

let construct_map (bindings: (string * string) list): (string StringMap.t) =
  fold_left (fun acc (bx, by) -> StringMap.add bx by acc) StringMap.empty bindings

(* return list of terms that match pred *)
let rec get_terms (pred: term -> bool) (f: formula) : term list =
	match f with
		| FTrue -> []
		| FFalse -> []

		| FAtom(_, _, tlargs) ->
			filter pred (unique tlargs)
		| FEquals(t1, t2) ->
			filter pred (unique [t1; t2])
    | FIn(t,_,_) -> filter pred [t]
		| FAnd(f1, f2) ->
      unique ((get_terms pred f1) @ (get_terms pred f2))
    | FOr(f1, f2) ->
      unique ((get_terms pred f1) @ (get_terms pred f2))
		| FNot(innerf) ->
			get_terms pred innerf;;

let rec get_terms_with_sign (pred: term -> bool) (startsign : bool) (f: formula) : (term*bool) list =
  match f with
    | FTrue -> []
    | FFalse -> []
    | FAtom(_, _, tlargs) ->
      filter_map (fun t -> if pred t then Some (t, startsign) else None) (unique tlargs)
    | FEquals(t1, t2) ->
      filter_map (fun t -> if pred t then Some (t, startsign) else None) (unique [t1; t2])
    | FIn(t,_,_) -> [(t, startsign)]
    | FAnd(f1, f2) ->
      unique ((get_terms_with_sign pred startsign f1) @ (get_terms_with_sign pred startsign f2))
    | FOr(f1, f2) ->
      unique ((get_terms_with_sign pred startsign f1) @ (get_terms_with_sign pred startsign f2))
    | FNot(innerf) ->
      get_terms_with_sign pred (not startsign) innerf;;


let rec get_vars (f: formula) : term list =
	get_terms (function | TVar(_) -> true |  _ -> false) f;;
(* as get_vars, but includes fields as well *)
let rec get_vars_and_fieldvars (f: formula) : term list =
	(*printf "get_vars_and_fieldvars: %s\n%!" (string_of_formula f);*)
	let varlist = get_terms
		(function | TVar(_) -> true | TField(_,_) -> true | _ -> false) f in
		(*printf "result of gvf: %s\n%!" (String.concat ";" (map string_of_term varlist));*)
		varlist;;

let get_head_vars (cls : clause) : term list =
	get_vars cls.head;;

let get_all_clause_vars (cls : clause) : term list =
	unique ((get_vars_and_fieldvars cls.head ) @ (get_vars_and_fieldvars cls.body));;

(* Gotta get preprocessor macros...
   Using this as a very rough measure of how much our naive list-based impl is
   being stressed. *)
let build_and_count = ref 0;;

(* Important: This needs to use fold_right, not fold_left, or ordering will not be preserved.
   Some functions (such as moving negated atoms to the end, for XSB) depend on an order-preserving
   build_and. ExtList's fold_right is tail-recursive. *)
let rec build_and (fs: formula list): formula =
  if !global_verbose > 2 then build_and_count := !build_and_count + 1;
  fold_right (fun f acc -> match f with
      | FTrue -> acc
      | FFalse -> FFalse
      | _ when acc = FTrue -> f
      | _ -> FAnd(f, acc))
    fs FTrue;;

let rec build_or (fs: formula list): formula =
  fold_right (fun f acc -> match f with
      | FFalse -> acc
      | FTrue -> FTrue
      | _ when acc = FFalse -> f
      | _ -> FOr(f, acc))
    fs FFalse;;

let after_equals (str : string) : string =
	let equals_index = try String.index str '=' with Not_found -> -1 in
		String.trim (String.sub str (equals_index + 1) (String.length str - equals_index - 1));;


(* XSB returns tuples like ["5", "3", "foo"].
   In the context of some variables TVar(x), etc.
   Produce [FEquals(TVar(x), TConst("5")), ...]

   Context: a PACKET-TRIGGERED clause, triggered by incpkt.
   Thus one of: (1) total compilation to flow fwd rules,
                (2) weakened compilation to flow controller rules,
                (3) wasn't a forward clause, so compilation to flow controller rules.
   Assume: the only TFields are fields of inc and out packet, and should be kept.
   All variables that are not fields should be ignored: under the above assumptions,
   either they are head vars of a non-fwd clause, or are existentials.
   (This assumes weakening has already taken place if needed by join.) *)

(* Remember to deal with escapes for non-numeric constants!
   Also make variables if returned _...*)
let reassemble_xsb_term (tstr: string): term =
  if (starts_with tstr "_") then
    TVar(tstr)
  else if (starts_with tstr "constesc") then
    TConst(String.sub tstr 8 ((String.length tstr) - 8))
  else TConst(tstr);;

let reassemble_xsb_equality (tlargs: term list) (tuple: term list) : formula list =
    map2 (fun origterm xsbterm ->
		  match xsbterm,origterm with
        | TVar(_), _ -> failwith "reassemble_xsb_equality: unconstrained variable"
        | TField(_, _), _ -> failwith "field def in xsb term returned"
        (* COMPILATION: free variable. Keep this assertion around in case it's needed. *)
        | TConst(c), (TVar(vname) as avar) when not (is_ANY_term avar) -> FEquals(TVar(vname), TConst(c))
        (* + ANY variables need elimination since they are bound universally under negation: *)
        | TConst(c), TVar(vname) -> failwith "reassemble_xsb_equality saw ANY term"
		    | _ -> FEquals(origterm, xsbterm))
    	 tlargs tuple;;

let reassemble_xsb_atom (modname:string) (relname: string) (tuple: string list): formula =
    FAtom(modname, relname, map reassemble_xsb_term tuple);;

let subtract (biglst: 'a list) (toremove: 'a list): 'a list =
  (filter (fun ele -> not (mem ele toremove)) biglst);;

let list_intersection (l1: 'a list) (l2: 'a list): 'a list =
  filter (fun ele1 -> (mem ele1 l2)) l1;;

(* everything in l2 is in l1. "l1 contains l2" *)
let list_contains (l1: 'a list) (l2: 'a list): bool =
  for_all (fun e2 -> mem e2 l1) l2;;

let is_field (t: term): bool =
  match t with | TField(_,_) -> true | _ -> false;;

let is_forward_clause (cl: clause): bool =
	match cl.head with
	| FAtom("", "forward", _) -> true
	| _ -> false;;

let rec uses_relation (goal_modname: string) (goal_relname: string) (f: formula): bool =
	match f with
		| FTrue -> false
		| FFalse -> false
		| FEquals(t1, t2) -> false
    | FIn(t,_,_) -> false
		| FAnd(f1, f2) -> (uses_relation goal_modname goal_relname f1) || (uses_relation goal_modname goal_relname f2)
		| FOr(f1, f2) -> (uses_relation goal_modname goal_relname f1) || (uses_relation goal_modname goal_relname f2)
		| FNot(innerf) -> uses_relation goal_modname goal_relname innerf
		| FAtom(modname, relname, tlargs) ->
			relname = goal_relname && modname = goal_modname;;

let product_of_lists lst1 lst2 =
  concat (map (fun e1 -> map (fun e2 -> (e1,e2)) lst2) lst1);;

let rec conj_to_list (f: formula): formula list =
	match f with
		| FAnd(f1, f2) -> (conj_to_list f1) @ (conj_to_list f2);
		| _ -> [f];;

let rec disj_to_list (f: formula): formula list =
    match f with
        | FOr(f1, f2) -> (disj_to_list f1) @ (disj_to_list f2);
        | _ -> [f];;

let rec nnf (f: formula): formula =
  match f with
        | FTrue -> f
        | FFalse -> f
        | FEquals(_, _) -> f
        | FAtom(_,_,_) -> f
        | FIn(_,_,_) -> f
        | FOr(f1, f2) -> FOr(nnf f1, nnf f2)
        | FAnd(f1, f2) -> FAnd(nnf f1, nnf f2)
        | FNot(f2) ->
          match f2 with
            | FTrue -> FFalse
            | FFalse -> FTrue
            | FEquals(_, _) -> f (* f, not f2. want to keep the negation. *)
            | FIn(_,_,_) -> f
            | FAtom(_,_,_) -> f
            | FNot(f3) -> nnf f3
            | FOr(f1, f2) -> FAnd(nnf (FNot f1), nnf (FNot f2))
            | FAnd(f1, f2) -> FOr(nnf (FNot f1), nnf (FNot f2));;

(* Assume: NNF before calling this *)
let rec disj_to_top ?(ignore_negation: bool = false) (f: formula): formula =
    match f with
        | FTrue -> f;
        | FFalse -> f;
        | FEquals(_, _) -> f;
        | FAtom(_, _, _) -> f;
        | FIn(_,_,_) -> f
        | FOr(f1, f2) ->
          FOr(disj_to_top ~ignore_negation:ignore_negation f1, disj_to_top ~ignore_negation:ignore_negation f2);
        | FNot(f2) when (not ignore_negation) ->
          (match f2 with
            | FTrue | FFalse
            | FAtom(_,_,_)
            | FEquals(_,_)
            | FIn(_,_,_) -> f
            | _  -> failwith ("disj_to_top: expected nnf formula; got unexpected negation: "^(string_of_formula f)))
        | FNot(_) -> f

        | FAnd(f1, f2) ->
            (* Distributive law if necessary *)
            let f1ds = disj_to_list (disj_to_top ~ignore_negation:ignore_negation f1) in
            let f2ds = disj_to_list (disj_to_top ~ignore_negation:ignore_negation f2) in

            (*printf "f: %s\n%!" (string_of_formula f);
            printf "f1ds: %s\n%!" (String.concat "; " (map string_of_formula f1ds));
            printf "f2ds: %s\n%!" (String.concat "; " (map string_of_formula f2ds));*)


            let pairs = product_of_lists f1ds f2ds in
                (* again, start with first pair, not FFalse *)
                let (firstfmla1, firstfmla2) = (hd pairs) in
               (*printf "PAIRS: %s\n%!" (String.concat "," (map (fun (f1, f2) -> (string_of_formula f1)^" "^(string_of_formula f2)) pairs));*)
                fold_left (fun acc (subf1, subf2) ->  (*(printf "%s %s: %s\n%!" (string_of_formula subf1) (string_of_formula subf2)) (string_of_formula  (FOr(acc, FAnd(subf1, subf2))));*)
                                                      FOr(acc, FAnd(subf1, subf2)))
                          (FAnd(firstfmla1, firstfmla2))
                          (tl pairs);;


(*****************************************************)

  let get_local_tables (prgm: flowlog_program): table_def list =
    filter (fun t -> match t.source with | LocalTable -> true | RemoteTable(_,_,_) -> false) prgm.tables;;

  let get_remote_tables (prgm: flowlog_program): table_def list =
    filter (fun t -> match t.source with | LocalTable -> false | RemoteTable(_,_,_) -> true) prgm.tables;;

  let get_table (prgm: flowlog_program) (goalrel: string) : table_def =
    Hashtbl.find prgm.memos.tablemap goalrel;;
  let get_remote_table (prgm: flowlog_program) (goalrel: string) : table_def =
    let tbl = (get_table prgm goalrel) in
      match tbl.source with
        | RemoteTable(_,_,_) -> tbl
        | _ -> raise Not_found;;

let input_rel_to_eventname (p: flowlog_program) (rname: string): string =
  let evnames = (filter_map (function
        | ReactInc(e, src, r) when r=rname -> Some e
        | _-> None) p.desugared_reacts) in
      if (length evnames) < 1 then raise (UndeclaredIncomingRelation rname);
      if (length evnames) > 1 then failwith "input_rel_to_eventname: more than one event type for an input relation";
      (hd evnames);;

  let get_event (prgm: flowlog_program) (evname: string) : event_def =
    Hashtbl.find prgm.memos.eventmap evname;;
  let get_outgoing (prgm: flowlog_program) (goalrel: string) : outgoing_def =
    (*Hashtbl.iter (fun k v -> printf "%s %s %b\n%!" goalrel k (k = goalrel)) prgm.memos.outgoingmap;*)
    Hashtbl.find prgm.memos.outgoingmap goalrel;;

  let is_local_table (prgm: flowlog_program) (relname: string): bool =
    try
      match (get_table prgm relname).source with | LocalTable -> true | RemoteTable(_,_,_) -> false
    with | Not_found -> false;;

  let is_remote_table (prgm: flowlog_program) (relname: string): bool =
     try
       match (get_table prgm relname).source with | LocalTable -> false | RemoteTable(_,_,_) -> true
     with | Not_found -> false;;

  let is_table (prgm: flowlog_program) (relname: string): bool =
    try
      match (get_table prgm relname).source with | _ -> true
    with | Not_found -> false;;

  let is_incoming_table (prgm: flowlog_program) (relname: string): bool =
    try ignore (get_event prgm (input_rel_to_eventname prgm relname));
        true
    with | Not_found -> false | UndeclaredIncomingRelation(_) -> false;;

  let is_outgoing_table (prgm: flowlog_program) (relname: string): bool =
    try ignore (get_outgoing prgm relname); true with | Not_found -> false;;

  let is_io_rel (prgm: flowlog_program) (relname: string): bool =
    is_incoming_table prgm relname || is_outgoing_table prgm relname;;

(* This version is meant to work on a list of AST decls. *)
(*let get_fields_for_type_preproc (decls: sdecl list) (etype: string): string list =
      let decl = find (function
        | DeclEvent(evname, evfielddecls) when evname = etype -> true
        | _ -> false) decls in
      match decl with
        | DeclEvent(evname, evfielddecls) ->
          (map (fun (fname, _) -> fname) evfielddecls)
        | _ -> failwith "get_fields_for_type";;*)

  let get_fields_for_type (prgm: flowlog_program) (etype: string): string list =
    map (fun (n,_) -> n) (get_event prgm etype).evfields;;
  let get_type_for_field (prgm: flowlog_program) (notif: event) (k: string): typeid =
    try
      let _, typ = find (fun (n,_) -> n = k)
                        (get_event prgm notif.typeid).evfields in
        typ
    with | Not_found -> failwith ("get_type_for_field: "^notif.typeid^" "^k^"; possibly missing built-in definitions in Flowlog_Packets?");;

  (* What is the reactive decl for this input relation?
     (Formerly we could assume that the rname was the same as the event type name. No longer!) *)
  let get_valid_fields_for_input_rel (p: flowlog_program) (rname: string): (string list) =
    try
        let evname = (input_rel_to_eventname p rname) in
         (* printf "event name: %s\n%!" evname;*)
          let result = map (fun (fname, _) -> fname) (get_event p evname).evfields in
            (*printf "result: %s\n%!" (string_of_list ";" identity result);*)
            result
    with | Not_found -> raise (UndeclaredIncomingRelation rname);;



(* The "int_string" String Type
 *
 * An "int_string" is an integer string (eg, "0x0A000001" representing the IP
 * address 10.0.0.1) and can be in hex, decimal, or any other OCaml-supported
 * numeric base. This is distinct from what we might think of as "IP string"
 * which is the canonical representation (10.0.0.1). Such strings can be handled
 * by ocaml-packet's ip_of_string and string_of_ip.
 *
 * int_string is currently the internal representation of IP and MAC addresses
 * in XSB.
 *)

(* Replacement for Int32.of_string which understands wrap-around
 *
 * Unfortunately, OCaml only supports signed 32 bit ints, therefore we need this
 * to handle IP addresses >= 128.0.0.0 (which will be received as >= 0x80000000)
 *)
let nwaddr_of_int_string (s: string): Int32.t =
  let n = Int64.of_string s in
    Int64.to_int32 n;;

(* Note: This will result in negative integers for IP addr >= 128.0.0.0 *)
let nwaddr_to_int_string (n: Int32.t): string = Int32.to_string n

(* Helper functions to also be explicit for 48-bit MAC addresses represented
 * as integer strings.
 *
 * Canonical MAC addresses representations can be handled via ocaml-packet's
 * mac_of_string and string_of_mac.
 *)

let macaddr_of_int_string (s: string): Int64.t = Int64.of_string s
let macaddr_to_int_string (n: Int64.t): string = Int64.to_string n

(* OpenFlow 1.0 Ports, by contrast, are only 16 bits, so we are safe from
 * wrap-around. They are also canonically represented as int strings anyway.
 *
 * These are explicit function so that we can ban use of "Int32.of_string"
 * outside this file.
 *)
let nwport_of_string (s: string): Int32.t = Int32.of_string s
let nwport_to_string (n: Int32.t): string = Int32.to_string n

(* For transport-layer ports *)
let tpport_of_int_string (s: string): int = int_of_string s
let tpport_to_int_string (n: int): string = string_of_int n

let hex_str_to_int_string (s: string): string = Int64.to_string (Int64.of_string s);;

(*************************************************************)
 (* improve this when we have more than strings running around
    We should also use option rather than empty string to indicate "use the default" *)
 let pretty_print_value (typename: string) (strval: string): string =
 try
   if strval = "" then "<DEFAULT>"
   else (match typename with
      | "ipaddr" -> Packet.string_of_ip (nwaddr_of_int_string strval)
      | "macaddr" -> Packet.string_of_mac (macaddr_of_int_string strval)
      | "portid" -> strval
      | "nwprotocol" -> strval
      | "switchid" -> OpenFlow0x01.string_of_switchId (Int64.of_string strval)
      | "ethtyp" -> Packet.string_of_dlTyp (int_of_string strval)
      | _ -> strval)
  with
    (* Account for BB queries sending variable names instead of, say, IP addresses *)
    | _ -> strval;;

 let pretty_print_term (typename: string) (c: term): string =
      pretty_print_value typename
                         (match c with | TConst(s) -> s
                                       | _ -> string_of_term c);;

  (* This function needs to know the program context, because that is where the type of each field is stored.
     Even if we stored values as ints, etc. that might not be enough either, since there is a
     difference between how we'd display an "int" and how we'd display an "ipaddr". *)
  let string_of_event (p: flowlog_program) (notif: event): string =
    notif.typeid^": ["^(String.concat ";"
      (map (fun (k, v) -> k^":"^(pretty_print_value (get_type_for_field p notif k) v))
           (StringMap.bindings notif.values)))^"]";;

let get_terms_in_rule_head_and_body (f: term -> bool) (r: srule): term list =
  match r.action with
    | ADelete(_,terms,fmla)
    | AInsert(_,terms,fmla)
    | ADo(_,terms,fmla) ->
      unique ((filter f terms)@(get_terms f fmla))
    | AForward(term,fmla,_) ->
      unique ((filter f [term])@(get_terms f fmla))
    | AStash(_, fmla,_,_) ->
      get_terms f fmla;;



let atom_to_relname (f: formula): string =
  match f with
    | FAtom(_, r, _) -> r
    | _ -> failwith "atom_to_relname";;

let rec get_atoms_with_sign ?(sign: bool = true) (f: formula): (bool * formula) list =
	match f with
		| FTrue -> []
		| FFalse -> []
    | FIn(_,_,_) -> []
		| FAtom(modname, relname, tlargs) -> [(sign, f)]
		| FEquals(t1, t2) -> []
		| FAnd(f1, f2) ->
			(unique (get_atoms_with_sign ~sign:sign f1) @ (get_atoms_with_sign ~sign:sign f2))
    | FOr(f1, f2) ->
      (unique (get_atoms_with_sign ~sign:sign f1) @ (get_atoms_with_sign ~sign:sign f2))
		| FNot(innerf) ->
			get_atoms_with_sign ~sign:(not sign) innerf;;

let get_atoms (f: formula): formula list =
  map (fun (s,a) -> a) (get_atoms_with_sign f);;

let rec get_equalities ?(sign: bool = true) (f: formula): (bool * formula) list =
  match f with
    | FTrue -> []
    | FFalse -> []
    | FAtom(modname, relname, tlargs) -> []
    | FEquals(t1, t2) -> [(sign, f)]
    | FIn(_,_,_) -> []
    | FAnd(f1, f2) ->
      (unique (get_equalities ~sign:sign f1) @ (get_equalities ~sign:sign f2))
    | FOr(f1, f2) ->
      (unique (get_equalities ~sign:sign f1) @ (get_equalities ~sign:sign f2))
    | FNot(innerf) ->
      get_equalities ~sign:(not sign) innerf;;

(* TODO: so many lists... Ocaml has sets. *)

let write_log_and_print (ln: string): unit =
  write_log ln;
  printf "%s\n%!" ln;;

let close_log (): unit =
  match !out_log with
  | Some(out) -> close_out out
  | _ -> ();;

let appendall (lsts: 'a list list): 'a list =
  fold_left (fun acc l -> acc @ l) [] lsts;;

let safe_compare_action_atoms_int (a1: action_atom) (a2: action_atom): int =
    match (a1, a2) with
      | (SwitchAction(sa1), SwitchAction(sa2)) ->
        Pervasives.compare sa1 sa2 (* sa1 = sa2*)
          (* VITAL ASSUMPTION: only one callback used here *)
      | (ControllerAction(_), ControllerAction(_)) -> 0
          (* Same assumption --- separate switch event callback (was false) *)
      | _ -> 1;;

let safe_compare_actions_int (al1: action) (al2: action): int =
  if (length al1) > (length al2) then 1 (* first is bigger *)
  else if (length al2) > (length al1) then -1
  else fold_left (fun acc v -> if acc <> 0 then acc else v) (* compare by first mismatched action *)
    0
    (map2 safe_compare_action_atoms_int al1 al2);;

let safe_compare_actions (al1: action) (al2: action): bool =
  (* same ordering? TODO probably not intended *)
  safe_compare_actions_int al1 al2 = 0;;

  (* (length al1 = length al2) && for_all2 safe_compare_action_atoms al1 al2;;*)

let rec gather_predicate_or (pr: pred): pred list =
  match pr with
    | Nothing -> [pr]
    | Everything -> [pr]
    | Not(_) -> [pr]
    | Or(p1, p2) -> (gather_predicate_or p1) @ (gather_predicate_or p2)
    | And(_, _) -> [pr]
    | Hdr(pat) -> [pr]
    | OnSwitch(sw) -> [pr];;

let rec gather_predicate_and (pr: pred): pred list =
  match pr with
    | Nothing -> [pr]
    | Everything -> [pr]
    | Not(_) -> [pr]
    | Or(_,_) -> [pr]
    | And(p1, p2) ->
        (gather_predicate_and p1) @ (gather_predicate_and p2)
    | Hdr(pat) -> [pr]
    | OnSwitch(sw) -> [pr];;

exception UnsatisfiableFlag;;

(* assumption: conjunctive list *)
let remove_contradictions (subpreds: pred list): pred list =
    (* Hdr(...), OnSwitch(...) If contradictions, this becomes Nothing*)
    let process_pred acc p =
      let (sws, hdrs, complex) = acc in
      match p with
      (* Remember that (sw=1 and sw!=2) and (sw!=1 and sw!=2) are both ok! *)
      | OnSwitch(sw) ->
        if exists (fun asw -> (asw = Int64.neg sw) || (asw > Int64.zero && asw <> sw)) sws then raise UnsatisfiableFlag
        else (sw :: sws, hdrs, complex)
      | Not(OnSwitch(sw)) ->
        if exists (fun asw -> asw = sw) sws then raise UnsatisfiableFlag
        else (Int64.neg sw :: sws, hdrs, complex)

      | Hdr(_) as newhdr ->
        if exists (fun ahdr -> ahdr = Not(newhdr)) hdrs then raise UnsatisfiableFlag
        else (sws, newhdr :: hdrs, complex)
      | Not(Hdr(_) as newhdrneg) as newnot ->
        if exists (fun ahdr -> ahdr = newhdrneg) hdrs then raise UnsatisfiableFlag
        else (sws, newnot :: hdrs, complex)

      | Everything -> acc
      | Nothing -> raise UnsatisfiableFlag

      | Not(innerp) as np ->
        if exists (fun apred -> apred = innerp) complex then raise UnsatisfiableFlag
        else (sws, hdrs, np :: complex)

        (* could be smarter TODO *)
      | Or(p1, p2) ->
        (sws, hdrs, p :: complex)
      | And(p1, p2) ->
        (sws, hdrs, p :: complex) in


      (*| _ -> failwith( (sprintf "remove_contradiction: expected only atomic preds: %s" (NetCore_Pretty.string_of_pred p)) in*)
      try
        let _ = fold_left process_pred ([],[],[]) subpreds in
          subpreds
      with UnsatisfiableFlag ->
       (* printf "unsatisfiable: %s\n%!" (String.concat ";" (map NetCore_Pretty.string_of_pred subpreds)); *)
        [Nothing];;


let build_predicate_and (prs: pred list): pred =
  fold_left (fun acc pr ->
      if acc = Nothing || pr = Nothing then Nothing
      else if acc = Everything then pr
      else if pr = Everything then acc
      else And(acc, pr))
    Everything
    (remove_contradictions prs);;

let build_predicate_or (prs: pred list): pred =
  fold_left (fun acc pr ->
      if acc = Everything || pr = Everything then Everything
      else if acc = Nothing then pr
      else if pr = Nothing then acc
      else Or(acc, pr))
    Nothing
    prs;; (* DO NOT call remove_contradictions here, as in b_p_and. *)

let rec simplify_netcore_predicate (pr: pred): pred =
  match pr with
    | Nothing -> Nothing
    | Everything -> Everything
    | Not(ip) -> Not(simplify_netcore_predicate ip)
    | Or(p1, p2) ->
        let sp1 = simplify_netcore_predicate p1 in
        let sp2 = simplify_netcore_predicate p2 in
          if sp1 = Everything || sp2 = Everything then Everything
          else if sp1 = Nothing then sp2
          else if sp2 = Nothing then sp1
          else Or(sp1, sp2)
    | And(p1, p2) ->
      (* TODO: wasting a ton of time on these calls when sets would be much faster than lists *)
        let unique_unsimp_conjuncts = (unique ((gather_predicate_and p1) @ (gather_predicate_and p2))) in
        (*printf "uuconjs: %s\n%!" (string_of_list ";" NetCore_Pretty.string_of_pred unique_unsimp_conjuncts);*)
        let conjuncts = unique (map simplify_netcore_predicate unique_unsimp_conjuncts) in
        (*printf "conjs: %s\n%!" (string_of_list ";" NetCore_Pretty.string_of_pred conjuncts);*)
        build_predicate_and conjuncts
        (*let sp1 = simplify_netcore_predicate p1 in
        let sp2 = simplify_netcore_predicate p2 in
          if sp1 = Nothing || sp2 = Nothing then Nothing
          else if sp1 = Everything then sp2
          else if sp2 = Everything then sp1
          else And(sp1, sp2)     *)
    | Hdr(pat) -> pr
    | OnSwitch(sw) -> pr;;

(* TODO: using mem here prevents from descending >1 layer of alternation
   since it compares by structural equality *)
let smart_compare_preds_int (p1: pred) (p2: pred): int =
  match (p1, p2) with
    | (And(_,_), And(_,_)) ->
      let set1 = gather_predicate_and p1 in
      let set2 = gather_predicate_and p2 in
        (* PredSet.equal set1 set2*)
        if      exists (fun e -> not (mem e set2)) set1 then 1
        else if exists (fun e -> not (mem e set1)) set2 then -1
      else 0
    | (Or(_,_), Or(_,_)) ->
      let set1 = gather_predicate_or p1 in
      let set2 = gather_predicate_or p2 in
      if      exists (fun e -> not (mem e set2)) set1 then 1
      else if exists (fun e -> not (mem e set1)) set2 then -1
      else 0
    | _ -> Pervasives.compare p1 p2;;

(* TODO: oh the inefficiency! SETS!! *)
let smart_compare_preds (p1: pred) (p2: pred): bool =
  (smart_compare_preds_int p1 p2) = 0;;

(* if want to totally order policies, need to do something better for actions *)
let rec safe_compare_pols (p1: pol) (p2: pol): bool =
  match p1, p2 with
    | Action(acts1), Action(acts2) ->
      safe_compare_actions acts1 acts2

      (* Special case for partial evaluation: prevents loss of duplicate (except for timeout) pols *)
    | ActionWithMeta(acts1, [NetCore_Types.IdleTimeout (OpenFlow0x01_Core.ExpiresAfter n1)]),
      ActionWithMeta(acts2, [NetCore_Types.IdleTimeout (OpenFlow0x01_Core.ExpiresAfter n2)]) ->
      safe_compare_actions acts1 acts2 && n1 = n2

    | ActionWithMeta(acts1, meta1), ActionWithMeta(acts2, meta2) ->
      (* TODO(adf): only compare metadata if we teach NetCore to compare it *)
      safe_compare_actions acts1 acts2
    (* assume: only one switch event handler *)
    | HandleSwitchEvent(_), HandleSwitchEvent(_) -> true
    | Filter(apred1), Filter(apred2) -> smart_compare_preds apred1 apred2
    | Union(subp11, subp12), Union(subp21, subp22)
    | Seq(subp11, subp12), Seq(subp21, subp22) ->
      let comp1 = safe_compare_pols subp11 subp21 in
      if comp1 then safe_compare_pols subp12 subp22
      else comp1
    | ITE(apred1, subp11, subp12), ITE(apred2, subp21, subp22) ->
      let comppred = smart_compare_preds apred1 apred2 in
      if not comppred then comppred
      else let comp1 = safe_compare_pols subp11 subp21 in
        if comp1 then safe_compare_pols subp12 subp22
        else comp1
    (* Not same policy structure, then different policy. *)
    | _ -> false;;

(* this won't intelligently compare within the pred. e.g. (p and q) != (q and p) here. *)
(* module PredSet  = Set.Make( struct type t = pred let compare = compare end );; *)

module PredSet  = Set.Make( struct type t = pred let compare = smart_compare_preds_int end );;

(* PredSet.add Nothing PredSet.empty ;;*)


(* If verbose flag is not set, prepare for XSB. Otherwise, add extra info for debug. *)

  let xsb_of_term ?(mode:xsbmode = Xsb) (t: term): string =
    match t with
      | TConst(s) ->
        if (Str.string_match (Str.regexp "[0-9\\-]") s 0) then
          s
        else
          "'constesc"^(String.lowercase s)^"'"
      | TVar(s) ->
        (match mode with
          | XsbAddUnderscoreVars | XsbForcePositive -> "_"^(String.uppercase s)
          |  _ -> (String.uppercase s))
      | TField(varname, fname) ->
        (match mode with
          | XsbAddUnderscoreVars | XsbForcePositive -> "_"^(String.uppercase (varname^"__"^fname))
          | _ -> (String.uppercase (varname^"__"^fname)));;

  let action_string outrel argterms fmla: string =
    let argstring = (String.concat "," (map (string_of_term ~verbose:Verbose) argterms)) in
      outrel^"("^argstring^") WHERE "^(string_of_formula ~verbose:Verbose fmla);;

  let rec string_of_action (ac: Flowlog_Types.action): string =
    match ac with
      | ADelete(outrel, argterms, fmla) ->
        "DELETE "^(action_string outrel argterms fmla);
      | AInsert(outrel, argterms, fmla) ->
        "INSERT "^(action_string outrel argterms fmla);
      | ADo(outrel, argterms, fmla) ->
        "DO "^(action_string outrel argterms fmla)
      | AForward(p, fmla, tout) ->
        "FORWARD "^(action_string "forward" [p] fmla)^" TIMEOUT: "^(match tout with | None -> "none" | Some(x) -> string_of_int x)
      | AStash(p, where, until, thens) ->
        ("STASH "^(action_string "stash" [p] where)^
        " until " ^(string_of_formula ~verbose:Verbose until)^" then "^
        (String.concat ";" (map string_of_action thens)));;

  let string_of_rule (r: srule): string =
    "ON "^r.onrel^"("^r.onvar^"):"^(string_of_action r.action);;

  let string_of_field_decl (d : (string * typeid)): string =
    let s1, s2 = d in s1^":"^s2;;

  let string_of_outgoing_fields (ofld: outgoing_fields): string =
    match ofld with
      | SameAsOnFields -> " (same as on)"
      | AnyFields -> " (any)"
      | FixedEvent(tname) -> " (:"^tname^")";;

  let string_of_declaration (d: sdecl): string =
    match d with
      | DeclTable(tname, argtypes) -> "TABLE "^tname^" "^(String.concat "," argtypes);
      | DeclRemoteTable(tname, argtypes) -> "REMOTE TABLE "^tname^" "^(String.concat "," argtypes);
      | DeclInc(tname, argtype) -> "INCOMING "^tname^" "^argtype;
      | DeclOut(tname, arg) -> "OUTGOING "^tname^" "^(string_of_outgoing_fields arg);
      | DeclEvent(evname, argdecls) -> "EVENT "^evname^" "^(String.concat "," (map string_of_field_decl argdecls));;

let string_of_astdeclaration (d: astdecl): string =
    match d with
      | ASTDeclTable(tname, argtypes) -> "TABLE "^tname^" "^(String.concat "," argtypes);
      | ASTDeclVar(tname, argtype, None) -> "VAR "^tname^": "^ argtype^" [no default]";
      | ASTDeclVar(tname, argtype, Some(d)) -> "VAR "^tname^": "^argtype^" default="^(string_of_term d);
      | ASTDeclRemoteTable(tname, argtypes) -> "REMOTE TABLE "^tname^" "^(String.concat "," argtypes);
      | ASTDeclInc(tname, argtype) -> "INCOMING "^tname^" "^argtype;
      | ASTDeclOut(tname, arg) -> "OUTGOING "^tname^" "^(string_of_outgoing_fields arg);
      | ASTDeclEvent(evname, argdecls) -> "EVENT "^evname^" "^(String.concat "," (map string_of_field_decl argdecls));;

  (* used for Alloy conversion *)
  let outspec_type (spec:spec_out) (inrel: string) (useincrel: bool) =
    match spec with
      | OutForward -> if useincrel then inrel else "packet"
      | OutEmit(typ) -> typ
      | OutPrint -> "print"
      | OutLoopback -> "loopback"
      | OutSend(evtype, ip, pt) -> evtype;;

  let string_of_outspec (spec: spec_out) =
    match spec with
      | OutForward -> "forward"
      | OutEmit(typ) -> "emit["^typ^"]"
      | OutPrint -> "print"
      | OutLoopback -> "loopback"
      | OutSend(evtype, ip, pt) -> "event("^evtype^") to "^ip^":"^pt;;

  let string_of_eventsource (src: eventsource): string =
    match src with
      | IncDP -> "dataplane"
      | IncCP -> "control plane"
      | IncThrift -> "thrift event";;

  let string_of_reactive (r: sreactive): string =
    match r with
      | ReactRemote(tblname, argtypes, qname, ip, port, refresh) ->
        tblname^"TABLE (remote) = "^qname^"("^(String.concat "," argtypes)^") @ "^ip^" "^port;
      | ReactOut(outrel, outf, spec) ->
        outrel^"("^(string_of_outgoing_fields outf)^") (output rel) =  @ "^(string_of_outspec spec);
      | ReactInc(evtype, src, relname) ->
        relname^" (input rel) "^evtype^" when from "^(string_of_eventsource src);;

  let string_of_stmt (stmt: stmt): string =
    match stmt with
      | SReactive(rstmt) -> (string_of_reactive rstmt);
      | SDecl(dstmt) -> (string_of_declaration dstmt);
      | SRule(rstmt) -> (string_of_rule rstmt);;

  let string_of_aststmt (stmt: aststmt): string =
    match stmt with
      | ASTReactive(rstmt) -> (string_of_reactive rstmt);
      | ASTDecl(dstmt) -> (string_of_astdeclaration dstmt);
      | ASTRule(rstmt) -> (string_of_rule rstmt);;

  let pretty_print_ast (ast: flowlog_ast): unit =
    iter (fun inc -> printf "INCLUDE %s;\n%!" inc) ast.includes;
    iter (fun stmt -> printf "%s\n%!" (string_of_aststmt stmt)) ast.statements;;

  let string_of_clause ?(verbose: printmode = Brief) (cl: clause): string =
    "CLAUSE: "^(string_of_formula ~verbose:verbose cl.head)^" :- "^(string_of_formula ~verbose:verbose cl.body)^"\n"^
    (if verbose = Verbose then "FROM RULE: "^(string_of_rule cl.orig_rule) else "");;

  let pretty_print_program (p: flowlog_program): unit =
    Hashtbl.iter (fun k v -> (printf "table %s\n%!" k)) p.memos.tablemap;
    iter (fun cl -> printf "clause: %s\n%!" (string_of_clause ~verbose:Verbose cl)) p.clauses;;

  let string_of_triggered_clause ?(verbose: bool = false) (cl: triggered_clause): string =
    sprintf "TRIGGERED %s: (oldpkt=%s)\n%s\nDependencies: %s\n"
      cl.id
      cl.oldpkt
      (string_of_clause cl.clause)
      (string_of_list "," identity cl.dependencies);;

(******************************************************************************)
(* Infer types of terms. Used largely for rule-scope variables in the Alloy translator. *)

let get_trigger (p: flowlog_program) (cl: clause): (event_def * string * term list) option =
  fold_left (fun acc at ->
    match at with
      | FAtom(_, relname, args) when is_incoming_table p relname ->
        let evname = (input_rel_to_eventname p relname) in
        (Some ((get_event p evname), relname, args))
      | _ -> acc)
    None
    (get_atoms cl.body);;

let is_delete_clause (cl: clause): bool =
  match cl.orig_rule.action with
    | ADelete(_,_,_) -> true
    | _ -> false;;

module TypeIdSet = Set.Make(struct type t = string let compare = Pervasives.compare end);;
module TermMap = Map.Make(struct type t = term let compare = Pervasives.compare end);;

(* Returns: (existentials, universals)
   Not as simple as just looking for TVars, because plus_
   and minus_ clauses will have free TVars.
   Quantified vars are variables in a clause body that are not in the head.
    ^^^ OUTDATED
   This function excludes the trigger and its fields (e.g. "pkt")
     Constants will be included now, always.  *)
let get_non_trigger_quantified_terms (p: flowlog_program) ?(include_head_vars: bool = true) ?(include_consts: bool = false) (cl:clause): (term list * term list) =
  let triggerargs = (match get_trigger p cl with | Some(_,_,args) -> args | None -> []) in
  let to_exclude = if include_head_vars then triggerargs else (get_head_vars cl) @ triggerargs in
  let signed_free = get_terms_with_sign
      (function | TVar(x) as t -> not (mem t to_exclude)
                | TConst(x) as t -> include_consts && not (mem t to_exclude)
                | _ -> false) true
        (* FAnd constructor only for convenience here; just want terms for the head too. *)
        (FAnd(cl.head, cl.body)) in
    fold_left (fun (acce, acca) (term, sign) ->
      (* unnecessary extra: we're already extracting terms from the head positively above *)
(*        (* In a REMOVE clause, if a head-var appears negatively, still counts as existential!
            This is because we don't require safety in the core syntax for removes; there's an *implied*
            constraint that the tuple is already there. *)
        if is_delete_clause cl && not sign then (term::acce, term::acca)
        else *) if sign then (term::acce, acca)
        else (acce, term::acca))
      ([],[])
      signed_free;;

(* add(1, 2, x) should generate
   add(1, x, 3) should also
   add(1, 2, new.locSw) should not

   Non-field is a conservative appx. Because could have add(1,2,x) and x=p.dlSrc
   Note that any variable not in the head or trigger is implicitly existentially quantified
   As before: (positive, negative)
*)
let get_non_field_head_vars_generated_by_builtins (p: flowlog_program) (cl:clause): (term list * term list) =
  let ats = get_atoms_with_sign cl.body in
  let headvars = (get_head_vars cl) in
    fold_left (fun acc (sign, atom) -> match atom with
        | FAtom(_, "add", args) -> (* kludge: should be more centralized list of generating builtins *)
          fold_left (fun acc2 t -> match t with | TVar(x) when mem t headvars ->
              let acc2e, acc2u = acc2 in (* insert the term into either the existential or universal list *)
              if sign then (t::acc2e, acc2u)
              else (acc2e, t::acc2u)
            | _ -> acc2) acc args
        | _ -> acc) ([],[]) ats;;

let get_plus_or_minus_relname (relname: string): string =
  if starts_with relname plus_prefix then
    (String.sub relname ((String.length plus_prefix)+1) ((String.length relname) - (String.length plus_prefix) -1))
  else if starts_with relname minus_prefix then
    (String.sub relname ((String.length minus_prefix)+1) ((String.length relname) - (String.length minus_prefix) -1))
  else failwith "get_plus_or_minus_relname";;


(* Could be extended to be more fine grained. For now, just say how many variables there are in the program
   per type, and separated by "action" and "transition" clauses. *)
type vars_count_report = {action_qvars_ext: int StringMap.t;
                          transition_qvars_ext: int StringMap.t;
                          action_qvars_uni: int StringMap.t;
                          transition_qvars_uni: int StringMap.t};;

let string_of_varcount (c: vars_count_report): string =
  let process k v acc = acc^(sprintf "%s: %d " k v) in
    (sprintf "Actions E:\n%s\nTransitions E:\n%s\nActions U:\n%s\nTransitions U:\n%s\n"
      (StringMap.fold process c.action_qvars_ext "")
      (StringMap.fold process c.transition_qvars_ext "")
      (StringMap.fold process c.action_qvars_uni "")
      (StringMap.fold process c.transition_qvars_uni ""));;

exception TypeInference of term;;
let type_inference_of_vars (p: flowlog_program) (start_inferences: TypeIdSet.t TermMap.t) (cl: clause): TypeIdSet.t TermMap.t =
  (* Keep the sign for our purposes; conflicts will result in an exception anyway *)
  (*let atoms = filter_map (fun (s,b) -> if s then Some(b) else None) (get_atoms_with_sign cl.body) in*)
  let atoms = (get_atoms cl.body) in

  (* Inner fold operator to add an inferred term->type pair*)
  let add_infer (acc2: TypeIdSet.t TermMap.t) (arg,aritytype): TypeIdSet.t TermMap.t =
    if TermMap.mem arg acc2 then
      TermMap.add arg (TypeIdSet.add aritytype (TermMap.find arg acc2)) acc2
    else
      TermMap.add arg (TypeIdSet.singleton aritytype) acc2 in

  (* expand out fields of a variable that occurs in an incoming-event-table *)
  let expand_fields (t: term) (fieldnames: string list): term list =
    match t with
      | TVar(vname) -> map (fun fn -> TField(vname, fn)) fieldnames
      | _ -> failwith "expand_fields" in

  (* Gather type assertions from table declarations
     whenever a term "arg" appears in an atomic formula. *)
  let base_body = fold_left (fun (acc: TypeIdSet.t TermMap.t) (at: formula) ->
    match at with
      | FAtom(_, relname, args) when is_local_table p relname ->
        let tblarity = (get_table p relname).tablearity in
          (* for every type in the table's arity, assert that type for the arg in that position *)
          fold_left add_infer acc (combine args tblarity)
      | FAtom(_, relname, args) when is_incoming_table p relname ->
        let eventfieldnames, eventfieldtypes = split ((get_event p (input_rel_to_eventname p relname)).evfields) in
          fold_left add_infer acc (combine (expand_fields (hd args) eventfieldnames) eventfieldtypes)
      | FAtom(_, relname, args)  -> acc
      | _ -> failwith "infer_type_of_vars") start_inferences atoms in

  (* Also need to include head variables *)
  let base =
    (match cl.head with
      | FAtom(_, relname, headargs) when is_outgoing_table p relname ->
        (* Get the outgoing def for this relation name, and turn it into an event decl*)
        let outdef = (get_outgoing p relname) in
        let outevent = (match outdef.outarity with
          | FixedEvent(evid) ->
            (get_event p evid)
          | SameAsOnFields ->
            (match (get_trigger p cl) with
              | Some(e,_,_) -> e
              | None ->
                write_log_and_print ("infer_type_of_vars head no trigger event in clause: "^(string_of_clause ~verbose:Verbose cl));
                failwith ("infer_type_of_vars head no trigger event in clause: "^(string_of_clause ~verbose:Verbose cl)))
          | _ -> failwith "infer_type_of_vars head AnyFields") in
        let headfieldnames, headfieldtypes = split (outevent.evfields) in
          fold_left add_infer base_body (combine (expand_fields (hd headargs) headfieldnames) headfieldtypes)

      (* STRINGS ARE A GREAT WAY OF HIDING USEFUL INFORMATION :-( "_" is extra, not included in defines. *)
      | FAtom(_, relname, args) when (starts_with relname plus_prefix) || (starts_with relname minus_prefix) ->
        let tblname = get_plus_or_minus_relname relname in
        let tblarity = (get_table p tblname).tablearity in
          fold_left add_infer base_body  (combine args tblarity)

      | FAtom(_, relname, args) -> base_body
      | _ -> failwith "infer_type_of_vars head") in

  (* Iterate knowledge to fixpoint along equalities *)
  (* ASSUMPTION: Don't have to do this more than once, since excess variables have been removed *)
  let equalities = (map (fun (sn,eqf) -> match eqf with FEquals(t1, t2) -> (t1, t2) | _ -> failwith "get_equalities")
    (get_equalities cl.body)) in
  (*let equalities = gather_nonneg_equalities cl.body false in*)

  (* Debug printing, please leave commented-out in function for quick diagnosis. *)

  (*TermMap.iter (fun k v ->
      printf "%s --> \n%!" (string_of_term k);
      TypeIdSet.iter (fun t -> printf "%s\n%!" t) v)
    base;*)
  (*printf "equalities: %s\natoms: %s\n%!"
    (string_of_list ";" (fun (t1,t2) -> (string_of_term t1)^"="^(string_of_term t2)) equalities)
    (string_of_list ";" string_of_formula atoms);*)

  let extend_by_equalities (curr: TypeIdSet.t TermMap.t): TypeIdSet.t TermMap.t =
    (* If t1->type then add type to t2's type set. (and the other way around, too) *)
    fold_left (fun acc (t1, t2) ->
        match TermMap.mem t1 acc, TermMap.mem t2 acc with
          | true,true ->
            let theunion = (TypeIdSet.union (TermMap.find t1 acc) (TermMap.find t2 acc)) in
              (TermMap.add t2 theunion (TermMap.add t1 theunion acc))
          | false,true ->
            TermMap.add t1 (TermMap.find t2 acc) acc
          | true,false ->
            TermMap.add t2 (TermMap.find t1 acc) acc
          | _ -> acc)
      curr equalities in

    extend_by_equalities base;;

(******************************************************)
(* Count the number of quantified vars in the program, indexed by type and quantifier
   Also detect term-generating builtins, like add, and count the implicit quantifier.

  For _existentials_, count only the ceiling over all clauses.
  For _universals_, accumulate over all clauses (without doublecounting across a single rule)

  Includes constants (accumulated for uni, maxed for exi) *)
let get_program_var_counts (p: flowlog_program) : vars_count_report =
  (* to avoid overcomplicating functional folds below; start with nothing seen *)
  let counted_already: (srule * term) list ref = ref [] in

  fold_left (fun acc cl ->
      (* count vars in head of clause if delete/insert; otherwise ignore (because head var for a DO is an event; will be counted separately) *)
      let include_head_vars = (match cl.orig_rule.action with | ADelete(_,_,_) | AInsert(_,_,_) -> true | _ -> false) in
      let ntqvs_ext, ntqvs_uni = get_non_trigger_quantified_terms p ~include_consts:true ~include_head_vars:include_head_vars cl in
      let infers = type_inference_of_vars p TermMap.empty cl in

      printf "\ngetting var counts for clause: %s%!" (string_of_clause cl);
      printf "ntqvs_ext: %s\nntqvs_uni: %s\n%!"
        (string_of_list "," string_of_term ntqvs_ext)
        (string_of_list "," string_of_term ntqvs_uni);


      (* Free, non-trigger variables that appear in builtin *)
      let builtin_generated_vars = get_non_field_head_vars_generated_by_builtins p cl in
      let builtin_generated_vars_e, builtin_generated_vars_u = builtin_generated_vars in
      if (length builtin_generated_vars_e) + (length builtin_generated_vars_u) > 0 then
        printf "vars generated by builtins in clause: %s\n%s\n%!"
          (string_of_list "," string_of_term builtin_generated_vars_e)
          (string_of_list "," string_of_term builtin_generated_vars_u);


      (* Don't doublecount a term or generated term for _u_ within same rule
           e.g. NOT r(x) and (a or b)  don't doublecount the x
             but in another rule if x is used, re-count it *)
      (* treat ANY terms like constants for purpose of recounting. This is because (a) we do renaming in the parser
         so that two distinct occurrences will have different ids and (b) rule-level remembering isn't good enough
         for ON packet(x) where NOT R(ANY):  do ... do ... do...   each do will be a sep. rule.*)
      let not_yet_counted t = (match t with | TVar(_) when is_ANY_term t -> for_all (fun (_, tm) -> t <> tm) !counted_already
                                            | TConst(_) -> for_all (fun (_, tm) -> t <> tm) !counted_already
                                            | TVar(_) -> not (mem (cl.orig_rule, t) !counted_already)
                                            | _ -> true) in
      let ntqvs_uni_filtered = filter not_yet_counted ntqvs_uni in
      let builtin_generated_vars_u_filtered = filter not_yet_counted builtin_generated_vars_u in
      (* remember what we just counted, along with rule context, for univ. *)
      let mark_counted t = (cl.orig_rule, t) in
        counted_already := (map mark_counted ntqvs_uni_filtered)@(map mark_counted builtin_generated_vars_u_filtered)@ (!counted_already);



      (* Adding the list of ordinary terms (e.g., existentially quantified variables, or ANYs) and
         generated terms (e.g., vars in the clause head that are bound by "add" *)
      let add_counts (m: int StringMap.t) (tlst: term list) (gentlst: term list): int StringMap.t =
          fold_left (fun acc t ->
            let typenames = if (TermMap.mem t infers) then (TermMap.find t infers) else (TypeIdSet.singleton "unknown") in
              printf "  handling: term=%s; types=%s\n%!" (string_of_term t) (string_of_list "," identity (TypeIdSet.elements typenames));

              (* add for each inferred type in case of inconsistency *)
              fold_left (fun acc2 typename ->
                let previous = if (StringMap.mem typename acc2) then (StringMap.find typename acc2) else 0 in
                  printf "add_counts incrementing %s to %d\n%!" typename (previous + 1);
                  (StringMap.add typename (previous+1) acc2))
                acc (TypeIdSet.elements typenames)

            ) m (unique (tlst@gentlst)) in

      (* Use to only keep largest count per-type over all clauses *)
      let ceiling_counts (m: int StringMap.t) (tlst: term list) (gentlst: term list): int StringMap.t =
        printf "ceiling_counts...\n%!";
        let local_ceiling = (add_counts StringMap.empty tlst gentlst) in (* build count for THIS clause only (start empty) *)
          fold_left (fun acc (typename,current) ->
            let previous = if (StringMap.mem typename acc) then (StringMap.find typename acc) else 0 in
              if previous < current then
              begin
                printf "ceiling_counts increasing %s to %d.\n%!" typename current;
                (StringMap.add typename current acc)
              end
              else acc)
            m (StringMap.bindings local_ceiling) in


        (* This clause needs to add such-and-such many variables to the counter*)
        match cl.head with
          | FAtom(_,relname,_) when (starts_with relname plus_prefix) || (starts_with relname minus_prefix) ->
            {acc with transition_qvars_ext=ceiling_counts acc.transition_qvars_ext ntqvs_ext builtin_generated_vars_e;
                      transition_qvars_uni=add_counts acc.transition_qvars_uni ntqvs_uni_filtered builtin_generated_vars_u_filtered;}
          | _ ->
            {acc with action_qvars_ext=ceiling_counts acc.action_qvars_ext ntqvs_ext builtin_generated_vars_e;
                      action_qvars_uni=add_counts acc.action_qvars_uni ntqvs_uni_filtered builtin_generated_vars_u_filtered})

    (* Folding the above over all the clauses, starting with no terms *)
    {action_qvars_ext=StringMap.empty; transition_qvars_ext=StringMap.empty;
     action_qvars_uni=StringMap.empty; transition_qvars_uni=StringMap.empty}
    p.clauses;;

let combine_inferences (i1: TypeIdSet.t TermMap.t) (i2: TypeIdSet.t TermMap.t): TypeIdSet.t TermMap.t =
  TermMap.merge (fun k i1o i2o ->
      match i1o, i2o with
        | Some(x1),Some(x2) -> Some(TypeIdSet.union x1 x2)
        | Some(x),None
        | None,Some(x) -> Some(x)
        | None,None -> None)
    i1 i2;;

(*****************************************************************************)
(* Almost the inverse of pretty printing. If a notification or a query reply
   includes, say, a dotted-quad IP, convert it to an intstring. *)
let ip_regexp = Str.regexp "[0-9]?[0-9]?[0-9]\\.[0-9]?[0-9]?[0-9]\\.[0-9]?[0-9]?[0-9]\\.[0-9]?[0-9]?[0-9]";;
let mac_regexp = Str.regexp "[0-9a-f]?[0-9a-f]:[0-9a-f]?[0-9a-f]:[0-9a-f]?[0-9a-f]:[0-9a-f]?[0-9a-f]:[0-9a-f]?[0-9a-f]:[0-9a-f]?[0-9a-f]";;
let flvalue_to_xsbable (s: string): string =
  if (Str.string_match ip_regexp s 0) then
    nwaddr_to_int_string (Packet.ip_of_string s)
  else if (Str.string_match mac_regexp s 0) then
    macaddr_to_int_string (Packet.mac_of_string s)
  else
    s;;

let is_mutex_locked (m: Mutex.t): bool =
  let fresh_lock = Mutex.try_lock m in
    match fresh_lock with
      | false -> true (* was already locked *)
      | true -> Mutex.unlock m; false;; (* was not already locked *)