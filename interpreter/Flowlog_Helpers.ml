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


exception UndeclaredIncomingRelation of string;;
exception UndeclaredOutgoingRelation of string;;
exception UndeclaredTable of string;;
exception BadBuiltInUse of string;;
exception BadArityOfTable of string;;
exception UndeclaredField of string * string;;
exception NonCondensedNoField of string;;
exception RelationHadMultipleReacts of string;;
exception RelationHadMultipleDecls of string;;
exception NoDefaultForField of string * string;;

(* True if string str1 ends with string str2 *)
let ends_with (str1 : string) (str2 : string) : bool =
	if String.length str2 > String.length str1 then false
    else (String.sub str1 ((String.length str1) - (String.length str2)) (String.length str2)) = str2;;

(* ends_with plus mirror-universe beard*)
let starts_with (str1 : string) (str2 : string) : bool =
  if String.length str2 > String.length str1 then false
    else (String.sub str1 0 (String.length str2)) = str2;;

let construct_map (bindings: (string * string) list): (string StringMap.t) =
  fold_left (fun acc (bx, by) -> StringMap.add bx by acc) StringMap.empty bindings

(* return list of terms that match pred *)
let rec get_terms (pred: term -> bool) (f: formula) : term list =
	match f with
		| FTrue -> []
		| FFalse -> []

		| FAtom(_, _, tlargs) ->
			filter pred tlargs
		| FEquals(t1, t2) ->
			filter pred [t1; t2]
    | FIn(t,_,_) -> [t]
		| FAnd(f1, f2) ->
			(unique (get_terms pred f1) @ (get_terms pred f2))
    | FOr(f1, f2) ->
      (unique (get_terms pred f1) @ (get_terms pred f2))
		| FNot(innerf) ->
			get_terms pred innerf;;

let rec get_terms_with_sign (pred: term -> bool) (startsign : bool) (f: formula) : (term*bool) list =
  match f with
    | FTrue -> []
    | FFalse -> []
    | FAtom(_, _, tlargs) ->
      filter_map (fun t -> if pred t then Some (t, startsign) else None) tlargs
    | FEquals(t1, t2) ->
      filter_map (fun t -> if pred t then Some (t, startsign) else None) [t1; t2]
    | FIn(t,_,_) -> [(t, startsign)]
    | FAnd(f1, f2) ->
      (unique (get_terms_with_sign pred startsign f1) @ (get_terms_with_sign pred startsign f2))
    | FOr(f1, f2) ->
      (unique (get_terms_with_sign pred startsign f1) @ (get_terms_with_sign pred startsign f2))
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

let rec build_and (fs: formula list): formula =
	if length fs > 1 then
		FAnd((hd fs), build_and (tl fs))
	else if length fs = 1 then
		(hd fs)
	else
		FTrue;;

let rec build_or (fs: formula list): formula =
	if length fs > 1 then
		FOr((hd fs), build_or (tl fs))
	else if length fs = 1 then
		(hd fs)
	else
		FFalse;;

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

let reassemble_xsb_equality (incpkt: string) (tlargs: term list) (tuple: term list) : formula list =
    map2 (fun origterm xsbterm ->
		  match xsbterm,origterm with
        | TVar(_), _ -> failwith "reassemble_xsb_equality: unconstrained variable"
        | TField(_, _), _ -> failwith "field def in xsb term returned"
        (* COMPILATION: free variable. Keep this assertion around in case it's needed. *)
        | TConst(c), TVar(vname) -> FEquals(TVar(vname), TConst(c)) (*FTrue *)
		    | _ -> FEquals(origterm, xsbterm))
    	 tlargs tuple;;

let reassemble_xsb_atom (modname:string) (relname: string) (tuple: string list): formula =
    FAtom(modname, relname, map reassemble_xsb_term tuple);;

let subtract (biglst: 'a list) (toremove: 'a list): 'a list =
  (filter (fun ele -> not (mem ele toremove)) biglst);;

let list_intersection (l1: 'a list) (l2: 'a list): 'a list =
  filter (fun ele1 -> (mem ele1 l2)) l1;;

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
  List.concat (List.map (fun e1 -> List.map (fun e2 -> (e1,e2)) lst2) lst1);;

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
            | FEquals(_,_) -> f
            | _  -> failwith ("disj_to_top: expected nnf fmla"))
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

  let get_event (prgm: flowlog_program) (goalrel: string) : event_def =
    Hashtbl.find prgm.memos.eventmap goalrel;;
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

  let is_incoming_table (prgm: flowlog_program) (relname: string): bool =
    try ignore (get_event prgm relname); true with | Not_found -> false;;
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

  let get_valid_fields_for_input_rel (p: flowlog_program) (rname: string): (string list) =
    try
      map (fun (fname, _) -> fname) (get_event p rname).evfields
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
  let n = int_of_string s in
  if n <= Int32.to_int Int32.max_int
  then Int32.of_int n
  else Int32.of_int (n - 0xFFFFFFFF - 1)

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



(*************************************************************)
 (* improve this when we have more than strings running around
    We should also use option rather than empty string to indicate "use the default" *)
 let pretty_print_value (typename: string) (strval: string): string =
  if strval = "" then "<DEFAULT>"
  else (match typename with
      | "ipaddr" -> Packet.string_of_ip (nwaddr_of_int_string strval)
      | "macaddr" -> Packet.string_of_mac (macaddr_of_int_string strval)
      | "portid" -> strval
      | "switchid" -> OpenFlow0x01.string_of_switchId (Int64.of_string strval)
      | "ethtyp" -> Packet.string_of_dlTyp (int_of_string strval)
      | _ -> strval);;

 let pretty_print_constant (typename: string) (c: term): string =
      pretty_print_value typename
                         (match c with | TConst(s) -> s
                                       | _ -> failwith ("pretty_print_constant: non constant"));;

  (* This function needs to know the program context, because that is where the type of each field is stored.
     Even if we stored values as ints, etc. that might not be enough either, since there is a
     difference between how we'd display an "int" and how we'd display an "ipaddr". *)
  let string_of_event (p: flowlog_program) (notif: event): string =
    notif.typeid^": ["^(String.concat ";"
      (map (fun (k, v) -> k^":"^(pretty_print_value (get_type_for_field p notif k) v))
           (StringMap.bindings notif.values)))^"]";;






let atom_to_relname (f: formula): string =
  match f with
    | FAtom(_, r, _) -> r
    | _ -> failwith "atom_to_relname";;

let rec get_atoms (f: formula): formula list =
	match f with
		| FTrue -> []
		| FFalse -> []
    | FIn(_,_,_) -> []
		| FAtom(modname, relname, tlargs) -> [f]
		| FEquals(t1, t2) -> []
		| FAnd(f1, f2) ->
			(unique (get_atoms f1) @ (get_atoms f2))
    | FOr(f1, f2) ->
      (unique (get_atoms f1) @ (get_atoms f2))
		| FNot(innerf) ->
			get_atoms innerf;;

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

let get_atoms_used_in_bodies (p: flowlog_program): formula list =
	let fmlas = map (fun cl -> cl.body) p.clauses in
		fold_left (fun acc f -> unique ((get_atoms f) @ acc)) [] fmlas;;


let out_log = ref None;;

let write_log (ln: string): unit =
  match !out_log with
  | None -> printf "Unable to write to log file.\n%!"
  | Some(out) -> fprintf out "%s\n%!" ln;;

let close_log (): unit =
  match !out_log with
  | Some(out) -> close_out out
  | _ -> ();;

let appendall (lsts: 'a list list): 'a list =
  fold_left (fun acc l -> acc @ l) [] lsts;;

let safe_compare_action_atoms (a1: action_atom) (a2: action_atom): bool =
    match (a1, a2) with
      | (SwitchAction(sa1), SwitchAction(sa2)) -> sa1 = sa2
          (* VITAL ASSUMPTION: only one callback used here *)
      | (ControllerAction(_), ControllerAction(_)) -> true
          (* Same assumption --- separate switch event callback *)
      | _ -> false;;

let safe_compare_actions (al1: action) (al2: action): bool =
  (* same ordering? TODO probably not intended *)
  (length al1 = length al2) && for_all2 safe_compare_action_atoms al1 al2;;

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

      | Not(p) as np ->
        if exists (fun apred -> apred = p) complex then raise UnsatisfiableFlag
        else (sws, hdrs, np :: complex)

        (* could be smarter TODO *)
      | Or(p1, p2) ->
        (sws, hdrs, p :: complex)

      | _ -> failwith ("remove_contradiction: expected only atomic preds") in
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
        let conjuncts = unique( map simplify_netcore_predicate (unique (gather_predicate_and p1) @ (gather_predicate_and p2))) in
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

  let string_of_outspec (spec: spec_out) =
    match spec with
      | OutForward -> "forward"
      | OutEmit(typ) -> "emit["^typ^"]"
      | OutPrint -> "print"
      | OutLoopback -> "loopback"
      | OutSend(evtype, ip, pt) -> "event("^evtype^") to "^ip^":"^pt;;

  let string_of_reactive (r: sreactive): string =
    match r with
      | ReactRemote(tblname, argtypes, qname, ip, port, refresh) ->
        tblname^"TABLE (remote) = "^qname^"("^(String.concat "," argtypes)^") @ "^ip^" "^port;
      | ReactOut(outrel, outf, spec) ->
        outrel^"("^(string_of_outgoing_fields outf)^") (output rel) =  @ "^(string_of_outspec spec);
      | ReactInc(evtype, relname) ->
        relname^" (input rel) "^evtype;;

  let string_of_stmt (stmt: stmt): string =
    match stmt with
      | SReactive(rstmt) -> (string_of_reactive rstmt);
      | SDecl(dstmt) -> (string_of_declaration dstmt);
      | SRule(rstmt) -> (string_of_rule rstmt);;

  let pretty_print_ast (ast: flowlog_ast): unit =
    iter (fun inc -> printf "INCLUDE %s;\n%!" inc) ast.includes;
    iter (fun stmt -> printf "%s\n%!" (string_of_stmt stmt)) ast.statements;;

  let string_of_clause ?(verbose: printmode = Brief) (cl: clause): string =
    "CLAUSE: "^(string_of_formula ~verbose:verbose cl.head)^" :- "^(string_of_formula ~verbose:verbose cl.body)^"\n"^
    (if verbose = Verbose then "FROM RULE: "^(string_of_rule cl.orig_rule) else "");;

  let string_of_triggered_clause ?(verbose: bool = false) (cl: triggered_clause): string =
    "TRIGGER: "^cl.oldpkt^" "^(string_of_clause cl.clause);;
