(****************************************************************)
(* Flowlog's core evaluation and compilation code               *)
(****************************************************************)

open Flowlog_Types
open Flowlog_Packets
open Flowlog_Helpers
open NetCore_Types
open ExtList.List
open Printf
open Xsb_Communication
open Flowlog_Thrift_Out
open Partial_Eval_Validation

let policy_recreation_thunk: (unit -> unit) option ref = ref None;;

(* Map query formula to results and time obtained (floating pt in seconds) *)
let remote_cache: (term list list* float) FmlaMap.t ref = ref FmlaMap.empty;;

(* action_atom list = atom
   Used internally to ease respond_to_notification; should not be accessed outside mutex! *)
let fwd_actions = ref [];;

(* Now that we added the suppress_new_policy parameter of respond_to_notification,
   we need to make sure that if there are changes waiting, a new policy will be issued,
   even if those changes were caused by an event that suppressed new policy generation. *)
let modifications_since_last_policy = ref [];;

exception ContradictionInPE;;

(* Push function for packet stream. Used to emit rather than forward. *)
(* Not sure why the input can be option. *)
let emit_push: ((NetCore_Types.switchId * NetCore_Types.portId * OpenFlow0x01_Core.payload) option -> unit) option ref = ref None;;

(* The stream in NetCore is not safe to call here; we may be in a separate thread.
   Lwt_stream push func will call the wakener for the stream immediately.

   We cannot work entirely within LWT to correct this (i.e., wrap the Thread mutex in an Lwt_mutex)
   as NetCore's packet callback returns actions, not actions in the Lwt.t monad.
   If respond_to_notification returned an Lwt.t that is *sleeping* (possible if something blocks)
   then we have no way to get the value out of the monad because it's not yet been computed.

   Therefore: emits that originate from a Thrift thread (eventsource = IncThrift) need to be QUEUED
   rather than immediately placed into the push function. To avoid polling on the Lwt side, we use
   a Unix FD to flag the queue is ready for reading.



    *)

let safe_queue = new SafeEmitQueue.cl_SafeEmitQueue;;

let guarded_emit_push (from:eventsource) (swid: switchId) (pt: portId) (payload: OpenFlow0x01_Core.payload): unit =
  match from with
    | IncThrift ->
      (* Queue the emit (see above)
         Do not actually place in the LWT stream. *)
      safe_queue#enqueue_emit (Some (swid, pt, payload));
    | _ ->
    (match !emit_push with
      | None -> printf "Packet stream has not been created yet. Error!\n%!"
      | Some f -> f (Some (swid,pt,payload)));;

let ms_on_packet_processing = ref 0.;;
let counter_inc_pkt = ref 0;;
let counter_inc_all = ref 0;;
let counter_pols_pushed = ref 0;;
let longest_used_packet_ms = ref 0.;;
let last_policy_pushed = ref (Action([]));;

exception ContradictoryActions of (string * string);;

let make_last_packet sw ncpt pkt buf =
  match ncpt with
    | NetCore_Pattern.Physical(x) -> Some (sw, x, pkt, buf)
    | _ -> failwith "set_last_packet_received";;

(***************************************************************************************)

(***************************************************************************************)

let rec refresh_remote_relation (p: flowlog_program) (f: formula): unit =
  match f with
    | FAtom(modname, relname, args) when (is_remote_table p relname) ->
      (* Is this query still in the cache? Then just use it. *)
      if not (FmlaMap.mem f !remote_cache) then
      begin
        let remtbl = get_remote_table p relname in
        match remtbl.source with
          | RemoteTable(qryname, (ip, port), refresh) ->
            (* qryname, not relname, when querying *)
            printf "REMOTE STATE --- REFRESHING: %s\n%!" (string_of_formula f);
            let bb_results = Flowlog_Thrift_Out.doBBquery qryname ip port args remtbl.tablearity in
            let bb_termlists = (map (fun tlist -> map reassemble_xsb_term tlist) bb_results) in
            let bb_formulas = (map (fun tlist -> FAtom(modname, relname, tlist)) bb_termlists) in
              remote_cache := FmlaMap.add f (bb_termlists, Unix.time()) !remote_cache;
              iter Communication.assert_formula bb_formulas;
          | _ -> failwith "refresh_remote_relation"
      end
    | _ -> ();;

(* get_state_maybe_remote calls out to BB and updates the cache IF UNCACHED. *)
let pre_load_all_remote_queries (p: flowlog_program): unit =
  (*printf ">>> %s\n%!" (String.concat "," (map string_of_formula (get_atoms_used_in_bodies p)));*)
  let remote_fmlas =
    filter (function | FAtom(modname, relname, args) when (is_remote_table p relname) -> true
                     | _-> false)
           p.memos.atoms_used_in_bodies in
    iter (fun f -> ignore (refresh_remote_relation p f)) remote_fmlas;;


(* Handle substitution of values in cases like (pkt.dlSrc = x, x = 5) and (pkt.dlSrc=pkt.dlDst, pkt.dlDst=5 *)
(* Also, if there is an FIn(_,_,_) in the clause, it may need substitution with equalities produced in PE. *)
let rec substitute_for_join (eqs: formula list) (othsubfs: formula list): formula list option =
  try
  (* After PE, the formula will be a conjunction of equalities, INs, and negated PE fmlas. *)
  if !global_verbose > 5 then
    write_log (sprintf "substitute_for_join: %s; %s\n%!"
      (String.concat "," (map string_of_formula eqs))
      (String.concat "," (map string_of_formula othsubfs)));

  (* gather assignments from the equality formulas
     detect duplicate or bad assignments *)
  let assignments = fold_left (fun acc subf -> match subf with

                                  (* DIRECT equality assignment. this is post PE, so don't need an FNot condition *)
                                  | FEquals((TVar(_) as v), (TConst(_) as c))
                                  | FEquals((TConst(_) as c), (TVar(_) as v))
                                  | FEquals((TField(_, _) as v), (TConst(_) as c))
                                  | FEquals(TConst(_) as c, (TField(_, _) as v)) ->

                                    (* we DO NOT need to check *EQUALITIES* under negated subformulas:
                                       a contradiction like (x=5 and y=6 and not (x=5 or y=6))
                                       will be resolved after substitution:
                                       (5=5 and 6=6 and not (5=5 or 6=6)
                                       which is a contradiction.

                                       *INs* buried under negation are handled in the same way. we are guaranteed that the addr and mask
                                       of an IN are strongly-safe, so will always arrive at <X> in <const>/<const>, which can be handled
                                       by NetCore. *)

                                    (* search any positive equalities for a contradiction to this assignment *)
                                    if (mem_assoc v acc) && (assoc v acc) <> c then
                                    begin
                                      (*write_log (sprintf "contradiction in PE (subfmla=%s):\n v=%s had=%s got=%s\n%!"
                                        (string_of_formula subf) (string_of_term v) (string_of_term (assoc v acc)) (string_of_term c));*)
                                      (* Without this exception, we will try to substitute below and end up with, e.g. FEquals(5,7) *)
                                      raise ContradictionInPE
                                      (* failwith ("contradictory assignment to "^(string_of_term v)^": "^(string_of_term (assoc v acc))^" versus"^(string_of_term c))*)
                                    end
                                    else
                                      (v, c) :: acc

                                  | FEquals(TField(_,_), TField(_,_)) -> acc
                                  | FTrue -> acc
                                  | _ -> failwith ("substitute_for_join: unexpected non-eq fmla:"^(string_of_formula subf))
                                ) [] eqs in

  (* substitute according to assignments. but don't freak out if result contains a 5=7, since may be part of negated subfmla
     also check for contradictory INs.
     + we will replace field-assignments at end of this function *)
  let substituted = (map (fun sf -> substitute_terms ~report_inconsistency:false sf assignments) othsubfs) in

  (* Need to iterate this substitution process. Consider: R(x), x=y, ~P(y).*)
  let neweqs = fold_left (fun acc subf -> match subf with
                              | FEquals((TVar(_)), (TConst(_)))
                              | FEquals((TConst(_)), (TVar(_)))
                              | FEquals((TField(_, _)), (TConst(_)))
                              | FEquals(TConst(_), (TField(_, _))) -> subf::acc
                              | _ -> acc) [] substituted in
  let remainder =
    (if (length neweqs) > 0 then
    begin
      (* need to recurse *)
      if !global_verbose >= 5 then
      begin
        write_log (sprintf "substitute_for_join recursing. neweqs: %s" (string_of_list "," string_of_formula neweqs));
        write_log (sprintf "former eqs: %s" (string_of_list "," string_of_formula eqs));
        write_log (sprintf "former othersubfs: %s" (string_of_list "," string_of_formula othsubfs));
      end;
      substitute_for_join neweqs (subtract substituted neweqs)
    end
    else Some(substituted)) in

  if !global_verbose >= 5 then
  begin
    iter (fun (v, c) -> write_log (sprintf "ASSN: %s -> %s" (string_of_term v) (string_of_term c))) assignments;
    write_log (sprintf "REMAINDER: %s" (String.concat "," (map string_of_formula (Option.default [FFalse] remainder))));
  end;

  if Option.is_none remainder then
    raise ContradictionInPE;

  (* Re-add field assignments that we substituted out for safety: *)
  let field_value_conj = filter_map (fun (v,c) -> match v with | TField(_, _) -> Some(FEquals(v, c)) | _ -> None) assignments in
  if !global_verbose > 3 then write_log (sprintf "FIELD VALUE CONJ: %s\n%!" (string_of_formula (build_and field_value_conj)));

  (* ~~~ASSUMPTION~~~
     ON blocks guard all field references within their scope: if a tpSrc field is used (even only within a negated subfmla),
     the on block must positively guard with dlTyp and nwProto in the same clause. This fact allows us to merely sort below,
     rather than checking and inserting into every partially-evaluated subformula inside negations. *)

  (* If there are still variables left in INs, they are free to vary arbitrarily, and so the compiler will ignore that IN. *)
  (* Also we need to make sure that DlTyp comes first, then NwProto, then other fields *)
  let result = sort ~cmp:order_clause_conjunct (field_value_conj @ (Option.get remainder)) in
    Some(result) (* none = empty clause; some [] = empty conjunction. used by filter_map in parent *)
  with | ContradictionInPE -> if !global_verbose >= 5 then printf "ContradictionInPE: \n%!"; None;;

(*************************************************************************)



(* ~~~ Store the PE result broken into a list of lists representing DISJ of CONJS of atoms.
	   Stage 1, since it's going to do substitution, needs this broken out, not as the final fmla.
	   Stage 2, on the other hand, passes over negative formulas, which have already been maximally
	   substituted. So using the full fmla here will save on rebuilding lists/fmlas.
*)
let pe_helper_cache: (formula list list) FmlaMap.t ref = ref FmlaMap.empty;;
let pe_neg_cache: formula FmlaMap.t ref = ref FmlaMap.empty;;

(* Don't re-PE clauses that have no altered dependencies.
   Alterations can take place in local tables via RTN
    but also in remote tables via expire_remote_state_in_xsb.

   This would be much cleaner as a field of triggered_clause, but
   then the PE functions would need to return a new flowlog_program, and
   so would respond_to_notification; worse, the program has already been
   passed to the Thrift thread... *)
let pe_clause_cache: (pred * action * srule) list StringMap.t ref = ref StringMap.empty;;

(* For profiling and debugging info *)
let count_pe_cache_hits = ref 0;;
let count_pe_cache_misses = ref 0;;
let count_pe_neg_cache_hits = ref 0;;
let count_pe_neg_cache_misses = ref 0;;
let count_clauses_pe = ref 0;;
let count_disjuncts_pe = ref 0;;
let count_unique_disjuncts_pe = ref 0;;

(* Assumes only positive subformulas!
    Returns list of lists, each inner list represents a conjunction
    and the formula that is the disj of the conjs of that list *)
let partial_evaluation_helper (positive_f: formula): (formula list list) =
  (* Within a single PE cycle, the state won't change. So avoid repetitive calls to get_state.
     ^ This optimization is more important than it seems, since clauses are split before negative fmlas get PE'd now. *)
  if FmlaMap.mem positive_f !pe_helper_cache then
  begin
    if !global_verbose >= 6 then write_log (sprintf "Formula was cached for PE: %s" (string_of_formula positive_f));
    if !global_verbose >= 1 then count_pe_cache_hits :=  !count_pe_cache_hits + 1;
    FmlaMap.find positive_f !pe_helper_cache
  end
  else
  begin
    if !global_verbose >= 6 then write_log (sprintf "Formula was a cache MISS for PE: %s" (string_of_formula positive_f));
    if !global_verbose >= 1 then count_pe_cache_misses :=  !count_pe_cache_misses + 1;

    (* Consider formulas like: seqpt(PUBLICIP,0x11,X) -- need to remove constants from tlargs before reassembling equalities *)
    let terms_used_no_constants_or_any = get_terms (fun t -> not (is_ANY_term t) && match t with | TConst(_) -> false | _ -> true) positive_f in

    let xsbresults = Communication.get_state ~compiler:true positive_f in
      let conjuncts = map
        (fun tl ->
             if !global_verbose >= 4 then
              printf "Reassembling an XSB equality for formula %s. tl=%s; terms_used_no_constants_or_any=%s\n%!"
                     (string_of_formula positive_f) (String.concat "," (map string_of_term tl)) (String.concat "," (map string_of_term terms_used_no_constants_or_any));
            (reassemble_xsb_equality terms_used_no_constants_or_any tl))
        xsbresults in

        if !global_verbose >= 5 then
          printf "<< POSITIVE partial evaluation result (converted from xsb) for %s\n    was: %s\n%!"
                 (string_of_formula positive_f)
                 (String.concat "\n" (map (fun fl -> (String.concat "," (map string_of_formula fl))) conjuncts));

        pe_helper_cache := FmlaMap.add positive_f conjuncts !pe_helper_cache;
        conjuncts
  end;;

let stage_2_partial_eval (incpkt: string) (atoms_or_neg: formula list): formula list =
  (map (fun subf -> match subf with
    (* If a negated atom, leave the disjunction *)
    | FNot(FTrue) -> FFalse
    | FNot(FFalse) -> FTrue

    (* Kludge... TN *)
    | FNot(FAtom(_,"haslongerprefixmatch",[sw;dst;pre;mask])) ->
      (* partial_evaluation_helper will use the cache here; we should too *)
      let specialfmla = (FAtom("", "getPossibleLongerPrefixMasks", [sw;pre;mask;TVar("P2");TVar("M2")])) in
      let result =
        if FmlaMap.mem specialfmla !pe_neg_cache then
        begin
          if !global_verbose >= 1 then count_pe_neg_cache_hits := !count_pe_neg_cache_hits + 1;
          FmlaMap.find specialfmla !pe_neg_cache
        end
        else
        begin
          if !global_verbose >= 1 then count_pe_neg_cache_misses :=  !count_pe_neg_cache_misses + 1;
          let xsbresults = Communication.get_state ~compiler:true specialfmla in
          let to_negate = map (fun tl -> match tl with
            | [p2;m2] -> FIn(dst, p2, m2)
            | _ -> failwith (sprintf "bad haslongerprefixmatch: %s\n%!" (string_of_list "," string_of_term tl)))
            xsbresults in
            let negated_disjunction = FNot(build_or to_negate) in
              pe_neg_cache := FmlaMap.add specialfmla negated_disjunction !pe_neg_cache;
            negated_disjunction
        end in
      if !global_verbose > 9 then printf "KLUDGE: haslongerprefixmatch: %s\n%!" (string_of_formula result);
      result

    (* Kludge 2... TN *)
    (* extra bad: so much duplicated code! *)
    | FNot(FAtom(_,"inlocalsubnet",[sw;dst])) ->
      (* partial_evaluation_helper will use the cache here; we should too *)
      (* note lowercase any *)
      let specialfmla = (FAtom("", "subnets", [TVar("pre");TVar("mask");TVar("any1");TVar("any2");sw;TVar("any3")])) in
      let result =
        if FmlaMap.mem specialfmla !pe_neg_cache then
        begin
          if !global_verbose >= 1 then count_pe_neg_cache_hits := !count_pe_neg_cache_hits + 1;
          FmlaMap.find specialfmla !pe_neg_cache
        end
        else
        begin
          if !global_verbose >= 1 then count_pe_neg_cache_misses :=  !count_pe_neg_cache_misses + 1;
          let xsbresults = Communication.get_state ~compiler:true specialfmla in
          write_log (sprintf "ils for %s %s xsb: %s\n" (string_of_term sw) (string_of_term dst) (string_of_list_list "," ";" string_of_term xsbresults));
          let to_negate = map (fun tl -> match tl with
            | [p2;m2] -> FIn(dst, p2, m2)
            | _ -> failwith (sprintf "bad islocalsubnet: %s\n%!" (string_of_list "," string_of_term tl)))
            xsbresults in
            let negated_disjunction = FNot(build_or to_negate) in
              pe_neg_cache := FmlaMap.add specialfmla negated_disjunction !pe_neg_cache;
            write_log (sprintf "ils for %s %s negated disj: %s\n" (string_of_term sw) (string_of_term dst) (string_of_formula negated_disjunction));
            negated_disjunction
        end in
      if !global_verbose > 9 then printf "KLUDGE: islocalsubnet: %s\n%!" (string_of_formula result);
      result

    | FNot(FAtom(_,_,_) as inner) ->
      (* Note second cache here: negatives can afford to cache the whole result formula *)
      if FmlaMap.mem inner !pe_neg_cache then
      begin
        if !global_verbose >= 1 then count_pe_neg_cache_hits :=  !count_pe_neg_cache_hits + 1;
        FmlaMap.find inner !pe_neg_cache
      end
      else
      begin
      	if !global_verbose >= 1 then count_pe_neg_cache_misses :=  !count_pe_neg_cache_misses + 1;
        let peresults = partial_evaluation_helper inner in
        (*write_log (sprintf "s2pe: %s\n%!" (String.concat ", " (map string_of_formula peresults)));*)
        let result = if (length peresults) < 1 then FTrue
                     else FNot((build_or (map build_and peresults))) in
        pe_neg_cache := FmlaMap.add inner result !pe_neg_cache;
        result
      end
    | _ -> subf) atoms_or_neg);;


(*
let debug_contains_equal_dlsrc_dldst (p:flowlog_program) (orig_fmla: formula) (positive_conjuncts: formula list list) (incpkt: string) (fl: formula list): unit =
  let dlsrc_asns = (filter_map (fun f ->
    match f with
      | FEquals(TField(v, "dlsrc"), TConst(x))
      | FEquals(TConst(x), TField(v, "dlsrc")) when v <> incpkt ->
        Some x
      | _ -> None)
    fl) in
  let dldst_asns = (filter_map (fun f ->
    match f with
      | FEquals(TField(v, "dldst"), TConst(x))
      | FEquals(TConst(x), TField(v, "dldst")) when v <> incpkt ->
        Some x
      | _ -> None)
    fl) in
  let inters = list_intersection dlsrc_asns dldst_asns in
    if length inters > 0 then
    begin
      printf "@@@ debug_contains_equal_dlsrc_dldst: %s;\n%s\n%!"
        (String.concat "," inters)
        (string_of_list ";" string_of_formula fl);
      printf "orig formula: %s\n%!" (string_of_formula orig_fmla);
      printf "  positive_conjuncts: %s\n%!" (string_of_list_list ";" "," string_of_formula positive_conjuncts) ;
      Communication.get_and_print_xsb_state p;
      exit(100);
    end;;
*)

(* deal with e.g.: nwdst = 10.1.1.1 and not(nwdst in 192.168.0.0/16 ...)
   avoid burdening NetCore's compiler.
   TODO (this will be run for every fully substituted clause, so super inefficient...)

   Context: NetCore's optimizer doesn't always catch situations like the above, and in Exodus
     those negated disjunctions can get pretty big. So we pass over the substituted clauses before
     converting them to policies, saving us from an O(n^2) blowup in number of OF rules in the worst case.
 *)
let remove_contradictory_nots (fs: formula list): formula list =
  let ins = fold_left (fun acc eqf -> match eqf with | FIn(tm, TConst(addr), TConst(mask)) -> (tm, (addr, mask))::acc
                                                     | _ -> acc) [] fs in
    let rec remove_helper innerf =
      (match innerf with | FOr(f1, f2) -> FOr(remove_helper f1, remove_helper f2)
                         | FEquals(tm, TConst(vx))
                         | FEquals(TConst(vx), tm) when mem_assoc tm ins ->
                           let a,m = assoc tm ins in
                             if is_in_ip_range (Int32.of_string vx) (Int32.of_string a) (int_of_string m) then innerf
                             else
                             begin
                               if !global_verbose > 4 then
                                 write_log (sprintf "remove_contradictory_nots: %s = %s vs. %s/%s" (string_of_term tm) vx a m);
                               FFalse
                             end
                         | _ -> innerf) in
      map (fun f -> match f with | FNot(innerf) -> FNot(remove_helper innerf) | _ -> f) fs;;


(* Replace state references with constant matrices.
   ASSUMPTION: f is a conjunction of atoms. *)
(* list of lists: outer list: new clauses; inner list: conjunctions of atoms in the clauses *)
let partial_evaluation (p: flowlog_program) (incpkt: string) (f: formula): formula list list =
  (* we have may R(X) and P(y) and Q(z) and ...
      If these relations are heavily populated, we could wind up with many, many clauses as a PE result.
      Instead, use XSB as much as possible to avoid contradictions (and thus junk clauses)
      (1) ask XSB for values for all positive atoms together
      (2) substitute
      (3) ask XSB for values for (substituted) negative atoms one at a time
       [no more substitution necessary] *)
  (* ASSUMPTION: this is called from within respond_to_notification, and thus is protected by mutex. *)
  let atoms = conj_to_list f in
  let positive_atoms = filter is_positive_atom atoms in
  let other_formulas = subtract atoms positive_atoms in

  (* Include Eqs and Ins that have terms constrained by positive_atoms.
     This gives XSB additional purchase and lowers the number of silly answers we get back.
     ^ This optimization is vital on large databases on rules with join.
     ***VITAL*** that these supplemental formulas come after the main positive atoms, due to XSB eval ordering *)
  let vars_defined_in_positive_atoms = unique (fold_left (fun acc atom -> (get_vars_and_fieldvars atom) @ acc) [] positive_atoms) in
  let additional_positive_subfmlas = filter (fun othfmla -> let used = get_vars_and_fieldvars othfmla in list_contains vars_defined_in_positive_atoms used) other_formulas in

    (* force refresh remote relations
      TODO: is this needed? aren't they already refreshed by this point? *)
    iter (refresh_remote_relation p) atoms;

    let positive_conj = build_and (positive_atoms @ additional_positive_subfmlas) in
      let positive_conjuncts = partial_evaluation_helper positive_conj in
        (* Hook each positive disj together with the other_formulas. We now have possibly many different clauses.
           ^ Note that this substitution also applies to the additional_positive_subfmlas.  *)
        let set_of_split_conjunctions = filter_map (fun listconj -> substitute_for_join listconj other_formulas) positive_conjuncts in

          let result_clauses = map (stage_2_partial_eval incpkt) set_of_split_conjunctions in

          let unique_result_clauses = unique (map remove_contradictory_nots result_clauses) in

            if !global_verbose >= 1 then
            begin
              count_disjuncts_pe := !count_disjuncts_pe + (length result_clauses);
              count_unique_disjuncts_pe := !count_unique_disjuncts_pe + (length unique_result_clauses);
            end;

            if !global_verbose >= 4 then
            begin
              if (length result_clauses) <> (length unique_result_clauses) then
              begin
                write_log (sprintf "PE of clause completed with %d disjuncts; only %d were unique after removing contradictory NOTs.\n%!" (length result_clauses) (length unique_result_clauses));
              end
              else
              begin
                write_log (sprintf "PE of clause completed with %d disjuncts.\n%!" (length unique_result_clauses));
              end
            end;
            (* TODO: Any should just not be included in the aggregation. Filtering dupes after the fact is inefficient.
               No reason why this should result in 41 separate disjuncts:
               aclalias(ANY27,PKT__LOCSW,ANY28,ANY29), aclalias(Rtr-loopback1-acl,PKT__LOCSW,PKT__LOCPT,NEW__LOCPT)
              ??? Is this TODO still in effect? ^^^*)

            (*iter (debug_contains_equal_dlsrc_dldst p f positive_conjuncts incpkt) unique_result_clauses;*)

            unique_result_clauses;;


(***************************************************************************************)

(** Add NetCore action to set a packet's header fields during forwarding
      TODO(adf): add support for VLAN, VLAN PCP, IP ToS, and transport srcPort &
                 dstPort once Flowlog includes in the packet types *)
  let enhance_action_atom (afld: string) (aval: string) (anact: action_atom): action_atom =
  match anact with
    | SwitchAction(oldout) ->
      (match afld with
        | "locpt" -> anact
        | "dlsrc" -> SwitchAction({oldout with outDlSrc = Some (None, macaddr_of_int_string aval) })
        | "dldst" -> SwitchAction({oldout with outDlDst = Some (None, macaddr_of_int_string aval) })
        | "dltyp" -> failwith ("OpenFlow 1.0 does not allow this field to be updated")
        | "nwsrc" -> SwitchAction({oldout with outNwSrc = Some (None, (nwaddr_of_int_string aval)) })
        | "nwdst" -> SwitchAction({oldout with outNwDst = Some (None, (nwaddr_of_int_string aval)) })
        | "tpsrc" -> SwitchAction({oldout with outTpSrc = Some (None, (tpport_of_int_string aval)) })
        | "tpdst" -> SwitchAction({oldout with outTpDst = Some (None, (tpport_of_int_string aval)) })
        | "nwproto" -> failwith ("OpenFlow 1.0 does not allow this field to be updated")

        | "dlvlan" ->
          let vlanval = if aval = "-1" then None else Some(int_of_string aval) in
            SwitchAction({oldout with outDlVlan = Some (None, vlanval) })

        | _ -> failwith ("enhance_action_atom unknown afld: "^afld^" -> "^aval))
    | _ -> failwith ("enhance_action_atom non SwitchAction: "^afld^" -> "^aval);;

open NetCore_Pattern
open NetCore_Wildcard

(* "unsafe" because the caller is responsible for resolving ALL vs. fwd(x) actions
   Without that resolution, the action set could produce duplicate packets. *)
let rec build_unsafe_switch_actions (oldpkt: string) (atoms: formula list): action =

  (* port actions: focus on new.locPt *)
  let create_port_actions (actlist: action) (lit: formula): action =

    (* Prevent actlist from accumulating duplicates or contradictions.
       (Since this set of atoms has been partially-evaluated, there should never be >1 outport value) *)
    let no_contradiction_or_repetition (aval: string): bool =
      for_all
        (function
          | SwitchAction(pat) ->
            (match pat.outPort with
              | NetCore_Pattern.Physical(aval2) ->
                if pat.outPort <> NetCore_Pattern.Physical(nwport_of_string aval) then
                  raise (ContradictoryActions(aval, nwport_to_string aval2))
                else
                  false (* repetition, don't repeat! *)
              | _ -> true)
          | _ -> true)
        actlist
    in

    match lit with
    | FFalse
    | FNot(FTrue) -> raise UnsatisfiableFlag
    | FTrue
    | FNot(FFalse) -> actlist

    (* old.locpt != new.locpt ---> allports (meaning: all but incoming) *)
    | FNot(FEquals(TField(var1, fld1), TField(var2, fld2))) ->
      if var1 = oldpkt && fld1 = "locpt" && var2 <> oldpkt && fld2 = "locpt" then
        [allportsatom] @ actlist
      else if var2 = oldpkt && fld2 = "locpt" && var1 <> oldpkt && fld1 = "locpt" then
        [allportsatom] @ actlist
      else failwith ("create_port_actions: bad negation: "^(string_of_formula lit))

    (* Substitition may give us a new.locPt != c.
       CONCERN: This smells a bit bad. How do we know that this always comes from a flood+substitution?
       *)
    | FNot(FEquals(TField(var1, fld1), TConst(avalue))) when var1 <> oldpkt && fld1 = "locpt" ->
      [allportsatom] @ actlist

    | FEquals(TField(var1, fld1), TField(var2, fld2)) ->
      if fld1 <> fld2 then
        failwith ("create_port_actions: invalid fields: "^fld1^" "^fld2)
      else
        actlist

    | FEquals(TField(avar, afld), TConst(aval))
    | FEquals(TConst(aval), TField(avar, afld)) ->
      (* CHECK FOR CONTRADICTION WITH PRIOR ACTIONS! *)
      if avar <> oldpkt && afld = "locpt" && (no_contradiction_or_repetition aval) then
        [SwitchAction({id with outPort = NetCore_Pattern.Physical(nwport_of_string aval)})]
        @ actlist
      else
        actlist

    (* IP range forbidden for new packet *)
    | FIn(_,_,_) -> actlist
    | FNot(FIn(_,_,_)) -> actlist
    (* Only negated variable equalities can be left-over. Ignore. *)
    | FNot(FEquals(TVar(_), TConst(_))) -> actlist
    | FNot(FEquals(TConst(_), TVar(_))) -> actlist

    (* If PE has left a disjunction or negated disjunction (or a standalone negated tuple),
        then it doesn't involve newpkt so ignore this conjunct. *)
    | FOr(_, _) -> actlist
    | FNot(FOr(_, _)) -> actlist
    | FNot(FAnd(_,_)) -> actlist

      (* remember: only called for FORWARD/EMIT rules. so safe to do this: *)
    | FNot(FEquals(TField(avar, afld), TConst(aval)))
    | FNot(FEquals(TConst(aval), TField(avar, afld))) ->
        actlist


    | _ -> failwith ("create_port_actions: bad lit: "^(string_of_formula lit)) in


  let create_mod_actions (actlist: action) (lit: formula): action =
    match lit with
    | FFalse -> actlist
    | FTrue -> actlist
    | FNot(_) -> actlist
    | FIn(_, _, _) -> actlist

    | FEquals(TField(var1, fld1), TField(var2, fld2)) ->
      failwith ("create_mod_actions: invalid equality "^(string_of_formula lit))

    | FEquals(TField(avar, afld), TConst(aval))
    | FEquals(TConst(aval), TField(avar, afld)) ->
      if avar <> oldpkt then
        (* since this is a FORWARD, must be over whatever we named newpkt *)
        map (enhance_action_atom afld aval) actlist
      else
        actlist (* ignore involving newpkt *)

    | _ -> failwith ("create_mod_actions: "^(string_of_formula lit)) in

    (* printf "  >> build_switch_actions: %s\n%!" (String.concat " ; " (map (string_of_formula ~verbose:true) atoms));*)

    (* Get a list of SwitchAction(output) by first folding over the atoms to get fwding actions, and then
       augmenting those with header modifications. So two iterations over atoms in all. *)
    (*  ASSUME: This is only called for FORWARDING rules, so only look at newpkt-related atoms *)
    (* if any actions are false, folding is invalidated *)
    try
      let port_actions = fold_left create_port_actions [] atoms in

      if !global_verbose > 4 then
      begin
        write_log "Actions: ";
        write_log (sprintf "%s\n" (NetCore_Pretty.string_of_action port_actions));
        write_log "\n";
      end;

      (* If no action has been produced, there were no new.locPt terms.
         Turn into a backflow to satisfy "all unmentioned equal" semantics.
         (PE won't remove all mention of new.locPt, because it always keeps a copy of the assertion it substitutes.) *)
      let port_actions_checked =
        if (length port_actions) = 0 then [SwitchAction({id with outPort = Here})]
        (* then pass *)
        (*if (length port_actions) = 0 then [SwitchAction(id)]*)
        else port_actions in

      let complete_actions = fold_left create_mod_actions port_actions_checked atoms in

      complete_actions
    with UnsatisfiableFlag -> [];;

(*********************************************************************************)

(* worst ocaml error ever: used "val" for varname. *)

let build_unsafe_switch_pred (oldpkt: string) (eqlist: formula list): pred =
let field_to_pattern (fld: string) (aval:string): NetCore_Pattern.t =
  match fld with (* switch handled via different pred type *)
    | "locpt" -> {all with ptrnInPort = WildcardExact (Physical(nwport_of_string aval)) }
    | "dlsrc" -> {all with ptrnDlSrc = WildcardExact (macaddr_of_int_string aval) }
    | "dldst" -> {all with ptrnDlDst = WildcardExact (macaddr_of_int_string aval) }
    | "dltyp" -> {all with ptrnDlTyp = WildcardExact (int_of_string aval) }
    | "nwsrc" -> {all with ptrnNwSrc = WildcardExact (nwaddr_of_int_string aval) }
    | "nwdst" ->  {all with ptrnNwDst = WildcardExact (nwaddr_of_int_string aval) }
    | "nwproto" -> {all with ptrnNwProto = WildcardExact (int_of_string aval) }
    | "tpsrc" -> {all with ptrnTpSrc = WildcardExact (int_of_string aval) }
    | "tpdst" ->  {all with ptrnTpDst = WildcardExact (int_of_string aval) }
    | "dlvlan" -> {all with ptrnDlVlan = if aval = "-1" then WildcardExact None else WildcardExact(Some (int_of_string aval)) }
    | _ -> failwith ("field_to_pattern: "^fld^" -> "^aval) in
    (* TODO: dlVLan, dlVLanPCP *)

let field_to_masked_pattern (fld: string) (aval:string) (maskstr:string): NetCore_Pattern.t =
  let mask_val = Int32.of_int (32 - (int_of_string maskstr)) in
  match fld with
    | "nwsrc" -> {all with ptrnNwSrc = WildcardPartial (nwaddr_of_int_string aval, mask_val) }
    | "nwdst" -> {all with ptrnNwDst = WildcardPartial (nwaddr_of_int_string aval, mask_val) }
    | _ -> failwith ("field_to_maskeD_pattern: "^fld^" -> "^aval^" / "^maskstr) in

  let rec eq_to_pred (eqf: formula): pred option =
  (*printf "eq to pred: %s; oldpkt=%s\n%!" (string_of_formula eqf) oldpkt;*)
    match eqf with
      | FNot(innerf) ->
        (match eq_to_pred innerf with
        | None -> None
        | Some(p) ->
          if p = Everything then Some Nothing
          else if p = Nothing then Some Everything
          else Some(Not(p)))

      (* only match oldpkt.<field> here*)
      | FEquals(TConst(aval), TField(varname, fld)) when varname = oldpkt ->
        if fld = "locsw" then Some(OnSwitch(Int64.of_string aval))
        else Some(Hdr(field_to_pattern fld aval))
      | FEquals(TField(varname, fld),TConst(aval)) when varname = oldpkt ->
        if fld = "locsw" then Some(OnSwitch(Int64.of_string aval))
        else Some(Hdr(field_to_pattern fld aval))

      | FTrue -> Some(Everything)
      | FFalse -> Some(Nothing)

      | FIn(TField(varname, fld), TConst(addr), TConst(mask)) when varname = oldpkt ->
        Some(Hdr(field_to_masked_pattern fld addr mask))
      | FIn(_,_,_) ->
        (*failwith ("Uncompilable IN formula:"^(string_of_formula eqf))*)
        (* Substituion did not remove vars. *)
        None

      (* PE may leave a disjunction or negated disjunction if it doesn't involve newpkt
         (meaning it needs processing here) and inside the disj are conjunctions over tuples!

         Thus, this function is now terribly named... :-)  *)

      | FOr(f1, f2) ->
          let p1 = eq_to_pred f1 in
          let p2 = eq_to_pred f2 in
          (match (p1, p2) with
            | (None, None) -> None
            | (None, Some(apred))
            | (Some(apred), None) -> Some(apred)
            | (Some(apred1), Some(apred2)) -> Some(Or(apred1, apred2)))
      | FAnd(f1, f2) ->
          let p1 = eq_to_pred f1 in
          let p2 = eq_to_pred f2 in
         (match (p1, p2) with
            | (None, None) -> None
            | (None, Some(apred))
            | (Some(apred), None) -> Some(apred)
            | (Some(apred1), Some(apred2)) -> Some(And(apred1, apred2)))

      | _ -> None (* something for action, not pred *) in
      (*| _  -> failwith ("build_switch_pred: "^(string_of_formula ~verbose:true eqf)) in*)

      (* SLOW! But check for NetCore accepted ordering of atoms if odd behavior. *)
      if !global_verbose >= 5 then
        ignore (validate_ordering eqlist);

    (* After PE, should be only equalities and negated equalities. Should be just a conjunction *)
    let predlist = unique (filter_map eq_to_pred eqlist) in

    (* MUST be fold_right, to preserve ordering! *)
      fold_right (fun pred acc -> match pred with
              | Nothing -> Nothing
              | Everything -> acc
              | _ when acc = Everything -> pred
              | _ when acc = Nothing -> acc
              | _ -> And(pred, acc)) predlist Everything;;

let is_all_ports_atom (a: action_atom): bool =
  a = allportsatom;;

(* Does this action policy involve the allports action? Error out if not an action policy *)
let involves_allports_atom (apol: pol): bool =
  match apol with
    | Action(a)
    | ActionWithMeta(a, _) ->
      exists (fun aatom -> is_all_ports_atom aatom) a
    | _ -> failwith ("involves_allports: "^NetCore_Pretty.string_of_pol apol);;

let get_physical_port_atom (a: action_atom): Int32.t option =
  match a with
    | SwitchAction(swa) ->
      (match swa.outPort with
        | Physical(aval) -> Some(aval)
        | _ -> None)
    | _ -> None;;

let is_physical_port_atom (a: action_atom): bool =
  match get_physical_port_atom a with
    | Some(_) -> true
    | _ -> false;;

let handle_all_and_port_together (oldpkt: string) (apred: pred) (acts: action_atom list): (pred * action) =
  (* If both allportsatom and physical(x) appear in acts,
     (1) remove allportsatom from acts
     (2) add oldpt != x to pred
     ASSUMPTION: an allportsatom only comes from new.locPt != old.locPt. *)

  if exists is_all_ports_atom acts &&
     exists is_physical_port_atom acts then
     let avalopt = get_physical_port_atom (find is_physical_port_atom acts) in
     match avalopt with
      | None -> failwith "handle_all_and_port_together"
      | Some(aval) ->
        let newpred = And(apred, Not(Hdr({all with ptrnInPort = WildcardExact (Physical(aval))}))) in
        let newacts = remove acts allportsatom in
          (*printf "Safe pred/act pair: %s THEN %s \n%!" (NetCore_Pretty.string_of_pred newpred) (NetCore_Pretty.string_of_action newacts);*)
          (newpred, newacts)
  else
  begin
    (*printf "(Was already) safe pred/act pair: %s THEN %s \n%!" (NetCore_Pretty.string_of_pred apred) (NetCore_Pretty.string_of_action acts);*)
    (apred, acts)
  end;;

(* Side effect: reads current state in XSB *)
(* Note: if given a non-packet-triggered clause, this function will happily compile it, but the trigger relation will be empty in current state
   and this reduce the clause to <false>. If the caller wants efficiency, it should pass only packet-triggered clauses. *)
let pkt_triggered_clause_to_netcore (p: flowlog_program) (callback: get_packet_handler option) (tcl: triggered_clause): (pred * action * srule) list =
    if !global_verbose > 4 then (match callback with
      | None -> write_log_and_print (sprintf "\n--- Packet triggered clause to netcore (FULL COMPILE) on: \n%s\nCached: %b\n%!" (string_of_triggered_clause tcl) (StringMap.mem tcl.id !pe_clause_cache))
      | Some(_) -> write_log_and_print (sprintf "\n--- Packet triggered clause to netcore (~CONTROLLER~) on: \n%sCached: %b\n%!" (string_of_triggered_clause tcl) (StringMap.mem tcl.id !pe_clause_cache)));

    (* If cached and still valid, used cached version *)
    if StringMap.mem tcl.id !pe_clause_cache then
      StringMap.find tcl.id !pe_clause_cache
    else
    match tcl.clause.head with
      | FAtom(_, _, headargs) ->
        (* Do partial evaluation. Need to know which terms are of the incoming packet.
          All others can be factored out. ASSUMPTION: the body is a conjunction of atoms. *)

        let startt = Unix.gettimeofday() in

        let bodies = partial_evaluation p tcl.oldpkt tcl.clause.body in

        if !global_verbose >= 1 then count_clauses_pe := !count_clauses_pe + 1;

        if !global_verbose > 2 then
          write_log (sprintf "Time to PE: %fs" (Unix.gettimeofday() -. startt));


        if !global_verbose > 5 then
          write_log (sprintf "bodies = \n    %s\n%!"
            (String.concat "\n    " (map (fun l -> (String.concat " & " (map string_of_formula l))) bodies)));


        (*printf "BODIES: %s" (String.concat ",\n" (map string_of_formula bodies));*)
         (*printf "BODIES from PE of single clause: %d\n%!" (length bodies);*)

        let unsafe_result =
          match callback with
            | None -> map (fun abodylist ->
                let unsafe_pred = build_unsafe_switch_pred tcl.oldpkt abodylist in
                let unsafe_acts = build_unsafe_switch_actions tcl.oldpkt abodylist in

                  (* Need to deal with cases where all-ports and physical(x)
                     coexist. Remember that all-ports FORBIDS input port! *)

                  handle_all_and_port_together tcl.oldpkt unsafe_pred unsafe_acts)
                          bodies
            | Some(f) ->
              (* Action is always sending to controller. So don't extract action *)
              let bigpred = fold_left (fun acc abodylist -> Or(acc, build_unsafe_switch_pred tcl.oldpkt abodylist)) Nothing bodies in
                [(bigpred, [ControllerAction(f)])] in


          (* add context: the rule for this clause*)
          let result = (map (fun (apred, anact) -> (apred, anact, tcl.clause.orig_rule)) unsafe_result) in

          if !global_verbose > 2 then
            write_log (sprintf "Time to PE+build policy for clause: %fs" (Unix.gettimeofday() -. startt));

          if !global_verbose > 4 then
          begin
            write_log "PREDS AND ACTIONS: \n";
            iter (fun res ->
              let result_pred, result_act, orig_rule = res in
                write_log (sprintf "%s -> %s\n"
                             (NetCore_Pretty.string_of_pred result_pred)
                             (NetCore_Pretty.string_of_action result_act))) result;
            write_log "\n\n";
          end;
		  pe_clause_cache := StringMap.add tcl.id result !pe_clause_cache;
          result
      | _ -> failwith "pkt_triggered_clause_to_netcore";;

(* call simplify after extracting. but only once. *)
let when_policy_applies_nodrop (startpol: pol): pred =
    let rec applies_helper (apol: pol): pred =
      match apol with
        (* Drop actions count as the policy not applying *)
        | Action([])
        | ActionWithMeta([], _) -> Nothing
        (* All other actions count as the policy applying *)
        | Action(_)
        | ActionWithMeta(_,_) -> Everything

        (* A union B -> applies when A does or B does*)
        | Union(p1, p2) -> build_predicate_or [applies_helper p1; applies_helper p2]
        (* Sequential composition with a filter first: filter must hold *)
        | Seq(Filter(apred), p2) -> And(apred, applies_helper p2)
        (* Disallow other kinds of sequences. *)
        | Filter(apred) -> apred
        | ITE(apred, p1, p2) -> build_predicate_or [And(apred, applies_helper p1);
                                                    And(Not(apred), applies_helper p2)]

        | _ -> failwith (sprintf "when_policy_applies_nodrop: could not handle %s\n" (NetCore_Pretty.string_of_pol apol)) in
    simplify_netcore_predicate (applies_helper startpol);;


(* Only timeout metadata for now *)
let build_metadata_action_pol (ac: NetCore_Types.action) (ru: srule): NetCore_Types.pol =
  match ru.action with
    | AForward(_, _, Some(n)) ->
      let metadata = [NetCore_Types.IdleTimeout (OpenFlow0x01_Core.ExpiresAfter n)] in
        NetCore_Types.ActionWithMeta(ac, metadata)
    | AForward(_, _, None) ->
      Action(ac) (* no metadata attached to policy *)
    | _ ->
      (* no metadata attached to policy: separate rule to reflect different logical intent between fwd and, say, insert *)
      Action(ac);;


(* Rule out duplicates with sets (logtime membership test) *)
 let triplecompcompare tup1 tup2 : int =
  let (p1, a1, r1) = tup1 in
  let (p2, a2, r2) = tup2 in
  match smart_compare_preds_int p1 p2 with
    | i when i > 0 -> 1
    | i when i < 0 -> -1
    | _ -> match safe_compare_actions_int a1 a2 with
             | j when j > 0 -> 1
             | j when j < 0 -> -1
             | _ -> Pervasives.compare r1 r2;;

module TripleCompSet  = Set.Make( struct type t = (pred * action * srule) let compare = triplecompcompare end );;

(* return the union of policies for each clause *)
(* Side effect: reads current state in XSB *)
let pkt_triggered_clauses_to_netcore (p: flowlog_program) (clauses: triggered_clause list) (callback: get_packet_handler option): pol =
  let startt = Unix.gettimeofday() in

  if !global_verbose > 2 then
    write_log (sprintf "Converting clauses to NetCore. %d clauses.\n%!" (length clauses));

  let pre_metadata = fold_left (fun acc cl ->
    let triples = pkt_triggered_clause_to_netcore p callback cl in
      (fold_left (fun acc2 t -> TripleCompSet.add t acc2) acc triples)) TripleCompSet.empty clauses in
  (* let pre_unique_pas = appendall (map (pkt_triggered_clause_to_netcore p callback) clauses) in*)

  if !global_verbose >= 3 then
    write_log (sprintf "[pkt_triggered_clauses_to_netcore] Time after building NetCore for all these %d clauses: %fs\n%!" (length clauses) (Unix.gettimeofday() -. startt));

  (* First, group these pas into (action, metadata) -> pred list
     Second: unique the pred list.
     (Don't try to unique first; would be a danger of losing the timeout if there is overlap with a non-timeout rule.
   *)

  (*iter
    (fun (ap, ac, r) -> write_log (sprintf "%s %s %s\n" (NetCore_Pretty.string_of_action ac) (NetCore_Pretty.string_of_pred ap) (string_of_rule r)))
    pre_unique_pas;*)

    (* Using a list here and then calling unique afterward would be terribly slow; convert back to list here *)

   (* CONCERN: intermediate set will destroy previous ordering; did we depend on that in any way? *)

    let clause_aps = map (fun (pr, ac, ru) -> (ac, pr, build_metadata_action_pol ac ru)) (TripleCompSet.elements pre_metadata) in


 (* if !global_verbose >= 3 then
    write_log (sprintf "[pkt_triggered_clauses_to_netcore] Time after pre_unique_with_metadata: %fs\n%!"  (Unix.gettimeofday() -. startt));
*)

  (*iter
    (fun (ac, ap, aa) -> write_log (sprintf "%s %s %s\n" (NetCore_Pretty.string_of_action ac) (NetCore_Pretty.string_of_pred ap) (NetCore_Pretty.string_of_pol aa)))
    pre_unique_with_metadata;*)

    (*let clause_aps = unique ~cmp:(fun tup1 tup2 ->
                                  let (ac1, pp1, actpol1) = tup1 in
                                  let (ac2, pp2, actpol2) = tup2 in
                                    (safe_compare_pols actpol1 actpol2) && (smart_compare_preds pp1 pp2)) pre_unique_with_metadata in
*)

  if !global_verbose >= 3 then
    write_log (sprintf "[pkt_triggered_clauses_to_netcore] Time after clause_aps: %fs\n%!"  (Unix.gettimeofday() -. startt));



  (*printf "Done creating clause_pas! %d members.\n%!" (length clause_pas);*)

  (*iter (fun (ac, ap, aa) -> write_log (sprintf "!!! %s ...  %s ... %s \n%!"
                        (NetCore_Pretty.string_of_pred ap)
                        (NetCore_Pretty.string_of_action ac)
                        (NetCore_Pretty.string_of_pol aa)) ) clause_aps;
  (*iter (fun (ap, aa) -> write_log (sprintf "--- %s %s\n%!"*)
                        (NetCore_Pretty.string_of_pred ap)
                        (NetCore_Pretty.string_of_action aa)) ) disj_pas;*)
  let or_of_preds_for_action_pol (apol: pol): pred =
    fold_left (fun acc (act, smallpred, actpol) ->
              if not (safe_compare_pols apol actpol) then acc
              else if acc = Nothing then smallpred
              else if smallpred = Nothing then acc
              else Or(acc, smallpred))
            Nothing
            clause_aps in

    if length clause_aps = 0 then
      (Action([]))
    else if length clause_aps = 1 then
      let (act, pred, actpol) = (hd clause_aps) in
        (Seq(Filter(pred), actpol))
    else
    begin
      let non_all_actionsused = unique ~cmp:safe_compare_actions
                        (filter_map (fun (ac, ap, aa) -> if involves_allports_atom aa then None else Some(ac)) clause_aps) in
      let actionpolswithallports = unique ~cmp:safe_compare_pols
                        (filter_map (fun (ac, ap, aa) -> if (involves_allports_atom aa) then Some(aa) else None) clause_aps) in

  if !global_verbose >= 3 then
    write_log (sprintf "[pkt_triggered_clauses_to_netcore] Time after non_all_actionsused and actionpolswithallports: %fs\n%!" (Unix.gettimeofday() -. startt));


      (*printf "actionsued = %s\nactionswithphysicalports = %s\n%!"
       (String.concat ";" (map NetCore_Pretty.string_of_action actionsused))
       (String.concat ";" (map NetCore_Pretty.string_of_action actionswithphysicalports));*)

    (* When folded over, will produce an IF statement prioritizing HIGHEST timeout.
       This enforces the invariant that if a packet is sent out port N by 2 flowlog rules, the highest of the two timeouts takes effect.
       Note that if the rules send the packet out different ports, their two separate timeouts will be respected *)
    let sort_by_decreasing_timeout (p1: pol) (p2: pol): int =
    (* WARNING: this sort function only works for the limited, single-timeout-only metadata we use as of this writing *)
      match p1, p2 with
        | Action(_), Action(_) -> 0
        | Action(_), ActionWithMeta(_,  [NetCore_Types.IdleTimeout (OpenFlow0x01_Core.ExpiresAfter n)]) -> 1
        | ActionWithMeta(_,  [NetCore_Types.IdleTimeout (OpenFlow0x01_Core.ExpiresAfter n)]), Action(_) -> -1
        | ActionWithMeta(_,  [NetCore_Types.IdleTimeout (OpenFlow0x01_Core.ExpiresAfter n1)]),
          ActionWithMeta(_,  [NetCore_Types.IdleTimeout (OpenFlow0x01_Core.ExpiresAfter n2)]) -> Pervasives.compare n1 n2
        | _ -> failwith ("sort_by_increasing_timeout: "^(NetCore_Pretty.string_of_pol p1)^", "^(NetCore_Pretty.string_of_pol p2)) in

    let get_action_pols_for_action (a: action): pol list =
      let raws = (fold_left (fun acc (ac, ap, aa) ->
          if (safe_compare_actions a ac) then aa::acc else acc) [] clause_aps) in
      let unique_raws = unique ~cmp:safe_compare_pols raws in
        (sort ~cmp:sort_by_decreasing_timeout unique_raws) in

    let per_action_helper (actionpols: pol list): pol =
      fold_left (fun (actionacc: pol) (acpol: pol) ->
          let newpred = simplify_netcore_predicate (or_of_preds_for_action_pol acpol) in
            if newpred = Nothing then actionacc
            else ITE(newpred, acpol, actionacc))
         (Action([]))
         actionpols in

      (* ***************************************** *)
      (* SINGLE PORTS *)

      (* Build a single union over policies for each distinct action *)
      (* if we get dup packets, make certain || isn't getting compiled to bag union in netcore *)
      let (union_over_ports: pol) = fold_left
                (fun (acc: pol) (aportaction: action) ->
                  (* which metadata combos do we have for this action? *)
                  let actionpols = get_action_pols_for_action aportaction in
                    Union(acc, per_action_helper actionpols))
                (Action([]))
                non_all_actionsused in

      write_log (sprintf "action pols with all ports: %s" (string_of_list "," NetCore_Pretty.string_of_pol actionpolswithallports));

      (* ***************************************** *)
      (* ALLPORTS *)
      (* the "allports" actions (note, may have multiple unique allports acts,
         due to metadata) must always be checked first*)
      (* If not all-ports, can safely union without overlap *)
      fold_left (fun (accpol: pol) (apactpol: pol) ->
          let (thepred: pred) = or_of_preds_for_action_pol apactpol in
            ITE(thepred, apactpol, accpol))
        union_over_ports
        (sort ~cmp:sort_by_decreasing_timeout actionpolswithallports)

        (* return not just the policy, but the predicate that triggers it*)
    end;;

(* Side effect: reads current state in XSB *)
(* Set up policies for all packet-triggered clauses *)
(* RETURN VALUES: fwd pol, notif pol, notif pred (for safety; see caller) *)
let program_to_netcore (p: flowlog_program) (callback: get_packet_handler): (pol * pol * pred) =
  (* posn 1: fully compilable packet-triggered.
     posn 2: pre-weakened, non-fully-compilable packet-triggered *)

    (* Clear out the PE caches. This is vital! *)
    pe_helper_cache := FmlaMap.empty;
    pe_neg_cache := FmlaMap.empty;

    (* The clause cache remains across PE iterations;
       respond_to_notification (local tables) and
       expire_remote_state_in_xsb (remote tables) clears it as needed *)

    (* reset debug counters*)
    count_pe_cache_hits := 0;
    count_pe_cache_misses := 0;
    count_pe_neg_cache_hits := 0;
    count_pe_neg_cache_misses := 0;
    count_clauses_pe := 0;
    count_disjuncts_pe := 0;
    count_unique_disjuncts_pe := 0;

    (* profile how much time is spent in PE vs. applicability etc. *)
    let startt = Unix.gettimeofday() in

    let fwd_pol = pkt_triggered_clauses_to_netcore p p.can_fully_compile_to_fwd_clauses None in
    if !global_verbose >= 3 then
      write_log (sprintf "[program_to_netcore] Time after building NetCore for fully compilable forward clauses: %fs\n%!" (Unix.gettimeofday() -. startt));

    (*let fwd_pred = when_policy_applies_nodrop fwd_pol in*)
    let notif_pol = pkt_triggered_clauses_to_netcore p p.weakened_cannot_compile_pt_clauses (Some callback) in
    if !global_verbose >= 3 then
      write_log (sprintf "[program_to_netcore] Time after building NetCore for weakened clauses: %fs\n%!" (Unix.gettimeofday() -. startt));

    let notif_pred = when_policy_applies_nodrop notif_pol in
    if !global_verbose >= 3 then
      write_log (sprintf "[program_to_netcore] Time after building notif_pred: %fs\n%!" (Unix.gettimeofday() -. startt));

    let result = (fwd_pol, notif_pol, notif_pred) in

    if !global_verbose >= 1 then
    begin
      write_log ("--------------------------- program_to_netcore statistics ---------------------------\n%!");
      write_log (sprintf "pe_helper_cache size = %d\n%!" (FmlaMap.cardinal !pe_helper_cache));
      write_log (sprintf "pe_neg_cache size = %d\n%!" (FmlaMap.cardinal !pe_neg_cache));
      write_log (sprintf "count_pe_cache_hits = %d\n%!" !count_pe_cache_hits);
      write_log (sprintf "count_pe_cache_misses = %d\n%!" !count_pe_cache_misses);
      write_log (sprintf "count_pe_neg_cache_hits = %d\n%!" !count_pe_neg_cache_hits);
      write_log (sprintf "count_pe_neg_cache_misses = %d\n%!" !count_pe_neg_cache_misses);
      write_log (sprintf "count_clauses_pe = %d\n%!" !count_clauses_pe);
      write_log (sprintf "count_disjuncts_pe = %d\n%!" !count_disjuncts_pe);
      write_log (sprintf "count_unique_disjuncts_pe = %d\n%!" !count_unique_disjuncts_pe);
    end;

    result;;

let forward_packet (p: flowlog_program) (ev: event) (full_packet: int64 * int32 * Packet.packet * int32 option): unit =
  let incsw, incpt, incpkt, incbuffid = full_packet in

  printf "forwarding: %s from switch %Ld\n%!" (string_of_event p ev) incsw;
  write_log (sprintf ">>> queueing forward action from XSB-constructed event: %s from switch %Ld\n%!" (string_of_event p ev) incsw);
  (* TODO use allpackets here. compilation uses it, but XSB returns every port individually. *)
  let base_action = SwitchAction({id with outPort = Physical(nwport_of_string (get_field ev "locpt"))}) in
    (* add modifications to fields as prescribed in the event *)
    fwd_actions :=
      (fold_left (fun acc afld ->
        if field_is_defined ev afld then
        begin
          let aval = get_field_helper ev afld in
            (*printf "%s %s %s\n%!" (NetCore_Pretty.string_of_action [acc]) afld aval; *)
            enhance_action_atom afld aval acc
        end
        else acc)
      base_action (remove legal_to_modify_packet_fields "locpt"))
      :: !fwd_actions;;

(*let doSendPacketIn_ref: (flowlog_program -> event -> string -> string -> (int64 * int32 * Packet.packet * int32 option) -> unit Lwt.t) option ref = ref None;;*)

let emit_packet (p: flowlog_program) (from: eventsource) (ev: event): unit =
  printf "emitting: %s\n%!" (string_of_event p ev);
  write_log (sprintf ">>> emitting: %s\n%!" (string_of_event p ev));
  let swid = (Int64.of_string (get_field ev "locsw")) in
  let pt = (nwport_of_string (get_field ev "locpt")) in

  (* TODO: confirm dltyp/nwProto etc. are consistent with whatever type of packet we're producing
     At the moment, someone can emit_arp with dlTyp = 0x000 or something dumb like that. *)
  guarded_emit_push from swid pt (OpenFlow0x01_Core.NotBuffered (marshal_packet ev));;

(*let forward_packet_from_cp (p: flowlog_program) (ev: event) (full_packet: int64 * int32 * Packet.packet * int32 option): unit =
  printf "forwarding packet from CP: %s\n%!" (string_of_event p ev);
  write_log (sprintf ">>> CP-forward (forward -> emit): %s\n%!" (string_of_event p ev));
  let pt = (nwport_of_string (get_field ev "locpt")) in
   let incsw, incpt, incpkt, incbuffid = full_packet in

   (*TODO: change fields besides locsw, locpt *)
   (* OpenFlow0x01_Core.payload *)
    guarded_emit_push incsw pt (OpenFlow0x01_Core.NotBuffered (Packet.marshal incpkt));;
*)

let send_event (p: flowlog_program) (ev: event) (ip: string) (pt: string) (full_packet: (int64 * int32 * Packet.packet * int32 option) option): unit =
  printf "sending: %s\n%!" (string_of_event p ev);
  write_log (sprintf ">>> sending: %s\n%!" (string_of_event p ev));
  if is_built_in_packet_typename ev.typeid then
   (* (match !doSendPacketIn_ref with
      | Some(f) ->
        (match full_packet with
          | None -> failwith "Error: send_event as packet_in failed because no orig packet was recorded"
          | Some(fp) -> ignore (f p ev ip pt fp))
      | None -> failwith "Error: couldn't send packet_in because function not set. Sending on CP not supported at present.")*)
	failwith "sending packets is currently unsupported"
  else
  begin
    let arity = (get_event p ev.typeid).evfields in
      doBBnotify ev ip pt arity
  end;;

let event_with_field (p: flowlog_program) (ev_so_far : event) (fieldn: string) (avalue: term) : event =
  match avalue with
    | TConst(x) -> {ev_so_far with values=(StringMap.add fieldn x ev_so_far.values)}
    (* If returned a TVar, then need to flag to use the default value *)
    | TVar(x) -> {ev_so_far with values=(StringMap.add fieldn "" ev_so_far.values)}
    | _ -> failwith ("event_with_field:"^(string_of_term avalue));;

let prepare_output (p: flowlog_program) (incoming_event: event) (from: eventsource) (defn: outgoing_def): (event * eventsource * spec_out) list =
    let arities = (match defn.outarity,defn.react with
                          | FixedEvent(evname),_ -> [get_fields_for_type p evname]
                          (*| AnyFields,OutPrint -> init (length tup) (fun i -> "x"^(string_of_int i))  *)

                          (* Not necessarily only ONE arity! Get them all.
                             automatic dlTyp/nwProto checks should prevent overlaps. *)
                          | SameAsOnFields,OutForward -> (map (get_valid_fields_for_input_rel p) (built_in_supertypes incoming_event.typeid))

                          | _ -> failwith "prepare_tuple") in

    let prepare_tuple (fieldnames: string list) (tup: term list): (event * eventsource * spec_out) =
      (*printf "PREPARING OUTPUT... tuple: %s\n%!" (String.concat ";" (map string_of_term tup));*)
      (* arglist orders the xsb results. assigns says how to use them, spec how to send them. *)
      let initev = (match defn.react with
                | OutForward -> {typeid = incoming_event.typeid; values=StringMap.empty}
                | OutEmit(typ) -> {typeid = typ; values=StringMap.empty}
                | OutLoopback -> failwith "loopback unsupported currently"
                | OutPrint -> failwith "print unsupported currently"
                | OutSend(outtype, _, _) -> {typeid=outtype; values=StringMap.empty}) in
      let ev = fold_left2 (event_with_field p) initev fieldnames tup in
        (ev, from, defn.react) in

      let prepare_tuples (tupswithfields: string list * term list list): (event * eventsource * spec_out) list =
        let fieldnames, tups = tupswithfields in
          fold_left (fun acc tup -> (prepare_tuple fieldnames tup) :: acc) [] tups in

      (* query xsb for this output relation *)
      (* May have multiple queries to make in the case of forward: Suppose an IP packet arrives, but the program
         has rules for ON packet(p) and ON ip_packet(p). These have different arities. We must reduce the event and
         query for each subtype. *)
      let xsb_results_with_fieldnames =
        fold_left (fun acc fieldnames ->
                    (fieldnames, Communication.get_state (FAtom("", defn.outname, map (fun s -> TVar(s)) fieldnames))) :: acc) [] arities in
      (* return the results to be executed later *)
      fold_left (fun acc tupswf -> (prepare_tuples tupswf) @ acc) [] xsb_results_with_fieldnames;;

let execute_output (p: flowlog_program)
                   (full_packet: (int64 * int32 * Packet.packet * int32 option) option)
                   ((ev, from, spec): event * eventsource * spec_out) : unit =
  match spec with
   | OutForward ->
     (* If "forwarding" a CP packet, we need to provide a new packet out, not a new forwarding action *)
     (match from,full_packet with
        | IncCP,Some(fp) -> failwith "unsupported: packet from CP" (*forward_packet_from_cp p ev fp*)
        | IncCP,_ -> failwith "forward_packet_from_cp: None given as last packet seen."

        | IncDP,Some(fp) -> (forward_packet p ev fp)
        | _ -> failwith "forward_packet: None given as last packet seen.")
   | OutEmit(_) -> emit_packet p from ev
   | OutPrint -> printf "PRINT RULE FIRED: %s\n%!" (string_of_event p ev)
   | OutLoopback -> failwith "loopback unsupported currently"
   | OutSend(_, ip, pt) -> send_event p ev ip pt full_packet;;

(* XSB query on plus or minus for table *)
let change_table_how (p: flowlog_program) (toadd: bool) (tbldecl: table_def): formula list =
  let relname,argtypes = tbldecl.tablename, tbldecl.tablearity in
    let modrelname = if toadd then (plus_prefix^"_"^relname) else (minus_prefix^"_"^relname) in
    let varlist = init (length argtypes) (fun i -> TVar("X"^string_of_int i)) in
    let xsb_results = Communication.get_state (FAtom("", modrelname, varlist)) in
    map (fun tup -> FAtom("", relname, tup)) xsb_results;;

let invalidate_policy_caches (p: flowlog_program) (modifications: string list): unit =
  (* Invalidate any clause that depends on something we changed.
     quick and <1000 clauses; don't prematurely optimize *)
  let invalidate (count_invalidated: int) (tcl: triggered_clause): int =
    if length (list_intersection tcl.dependencies modifications) > 0 then
    begin
      pe_clause_cache := StringMap.remove tcl.id !pe_clause_cache;
      count_invalidated+1
    end
    else count_invalidated in

  let invalidated =
  (fold_left invalidate 0 p.weakened_cannot_compile_pt_clauses) +
  (fold_left invalidate 0 p.can_fully_compile_to_fwd_clauses) in
  if !global_verbose > 1 then
      write_log (sprintf "pe_clause_cache had %d entries invalidated. %d entries remain. %d total compilable clauses.\n%!"
        invalidated
        (StringMap.cardinal !pe_clause_cache)
        ((length p.weakened_cannot_compile_pt_clauses) + (length p.can_fully_compile_to_fwd_clauses)));;

let expire_remote_state_in_xsb (p: flowlog_program) : unit =

  (* The cache is keyed by rel/tuple. So R(X, 1) is a DIFFERENT entry from R(1, X). *)
  let expire_remote_if_time (p:flowlog_program) (keyfmla: formula) (values: ((term list) list * float)): unit =
    if !global_verbose > 1 then printf "expire_remote_if_time %s\n%!" (string_of_formula keyfmla);
    let (xsb_results, timestamp) = values in
    match keyfmla with
      | FAtom(modname, relname, args) ->
      begin
        let remtbl = get_remote_table p relname in
         match remtbl.source with
          | RemoteTable(qryname, (ip, port), refresh) ->
          begin
            match refresh with
              | RefreshTimeout(num, units) when units = "seconds" ->
                (* expire every num units. TODO: suppt more than seconds *)
                if Unix.time() > ((float_of_int num) +. timestamp) then begin
                  if !global_verbose > 1 then printf "REMOTE STATE --- Expiring remote for formula (duration expired): %s\n%!"
                        (string_of_formula keyfmla);
                  remote_cache := FmlaMap.remove keyfmla !remote_cache;
                  invalidate_policy_caches p [relname]; (* *)
                  iter (fun tup -> Communication.retract_formula (FAtom(modname, relname, tup))) xsb_results
                end else
                  if !global_verbose > 1 then printf "REMOTE STATE --- Allowing relation to remain: %s %s %s\n%!"
                    (string_of_formula keyfmla) (string_of_int num) (string_of_float timestamp);
                  ();
              | RefreshNever ->
                (* never expire pure tables *)
                ();
              | RefreshEvery ->
                (* expire everything under this table, every evaluation cycle *)
                if !global_verbose > 1 then printf "REMOTE STATE --- Expiring remote for formula: %s\n%!" (string_of_formula keyfmla);
                remote_cache := FmlaMap.remove keyfmla !remote_cache;
                invalidate_policy_caches p [relname]; (* *)
                iter (fun tup -> Communication.retract_formula (FAtom(modname, relname, tup))) xsb_results;
              | RefreshTimeout(_,_) -> failwith "expire_remote_state_in_xsb: bad timeout"
          end
        | _ -> failwith "expire_remote_state_in_xsb: bad defn_decl"
      end
      | _ -> failwith "expire_remote_state_in_xsb: bad key formula" in

    FmlaMap.iter (expire_remote_if_time p) !remote_cache;;

(* Which definitions need triggering by this notification? *)
let get_output_defns_triggered (p: flowlog_program) (notif: event) (src: eventsource): outgoing_def list =
  let inrelnames = inc_event_to_relnames p notif src in
  let outrelnames = fold_left (fun acc inrel -> (Hashtbl.find_all p.memos.out_triggers inrel) @ acc) [] inrelnames in
  let possibly_triggered = filter (fun def -> mem def.outname outrelnames) p.outgoings in
    (*printf "possibly triggered: %s\n%!" (String.concat ",\n" (map string_of_reactive possibly_triggered));*)
    possibly_triggered;;

let get_local_tables_triggered (p: flowlog_program) (sign: bool) (notif: event) (src: eventsource): table_def list =
  let inrelnames = inc_event_to_relnames p notif src in
  let outrelnames = fold_left
    (fun acc inrel -> (Hashtbl.find_all
                        (if sign then p.memos.insert_triggers else p.memos.delete_triggers) inrel) @ acc) [] inrelnames in
  let possibly_triggered = filter (fun def -> mem def.tablename outrelnames) p.tables in
    (*printf "possibly triggered: %s\n%!" (String.concat ",\n" (map string_of_declaration possibly_triggered));*)
    possibly_triggered;;

(*let lwt_respond_lock = Lwt_mutex.create ();;*)

(* Returns list of names of tables that have been modified, plus forwarding actions if any *)
(* DO NOT PASS suppress_new_policy = true unless it is safe to do so! It was added to prevent a single switch registration from
   rebuilding the policy once for each new port. *)
let respond_to_notification
  (p: flowlog_program) ?(suppress_new_policy: bool = false)
  ?(full_packet: (int64 * int32 * Packet.packet * int32 option) option = None)
  (notif: event) (from: eventsource): (string list * 'a list) =
  try (* IF THIS FUNCTION RETURNS LWT.T, needs to be try_lwt, not try, otherwise a failure will freeze the program *)
      write_log (sprintf "<<< respond_to_notification... (outside mutex) for event: %s" (string_of_event p notif));

      write_log (sprintf "xsb mutex status: %b\n%!" (is_mutex_locked xsbmutex));
      printf "xsb mutex status: %b\n%!" (is_mutex_locked xsbmutex);
      printf "current thread ID: %d\n%!" (Thread.id (Thread.self ()));
      (* Yes, we need two layers of mutexes to account for the fact that the codebase uses two KINDS of thread-management:
         NetCore (and the general program) uses Lwt. But Thrift uses standard OCaml Threads.

         Suppose Lwt A is dealing with a dataplane packet, but meanwhile Lwt B, a switch-proxy listener, receives a packet_out.
         If B hits the "real" mutex while A has it locked, the program deadlocks (the "real" thread is running B, and can never
         switch to A). So we give Lwts a chance to yield here. *)

    (*  Lwt_mutex.with_lock lwt_respond_lock
    (fun () ->*)
      Mutex.lock xsbmutex;

      (* Timer etc. need to be protected by the mutex. *)
      let startt = Unix.gettimeofday() in
      counter_inc_all := !counter_inc_all + 1;


      (* announce nature of event being processed inside the mutex, for maintaing sanity when reading the log*)
      write_log "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<";
      write_log (sprintf "<<< incoming: %s" (string_of_event p notif));


      (* TN: Not sure if Lwt_mutex locks work for *all* Lwt threads in the process, or merely all Lwt threads in the OCaml thread.
         If the latter, this double-lock isn't enough to protect us if more than one OC thread splits via Lwt. If A1 locks, B1 and B2
         will both get stopped at the OC mutex, and nothing will prevent them from executing simultaneously once A1 unlocks.

         At the moment, only the main network thread splits via Lwt, so the above scenario shouldn't occur. *)


  (*printf "~~~~ RESPONDING TO NOTIFICATION ABOVE ~~~~~~~~~~~~~~~~~~~\n%!";*)

  if !global_verbose >= 3 then
      write_log (sprintf "Time after locking mutexes: %fs\n%!" (Unix.gettimeofday() -. startt));

  (* populate the EDB with event *)
    Communication.assert_event_and_subevents p notif from;

    if !global_verbose >= 3 then
      write_log (sprintf "Time after asserting event: %fs\n%!" (Unix.gettimeofday() -. startt));

    (* Expire remote state if needed*)
    expire_remote_state_in_xsb p;

    if !global_verbose >= 3 then
      write_log (sprintf "Time after asserting event + expiring remote state: %fs\n%!" (Unix.gettimeofday() -. startt));


    (* Since we can't hook XSB's access to these relations,
       over-generalize and ask for all the fmlas that can possibly be needed.
       For instance, if foo(X, pkt.dlSrc) is used, ask for foo(X,Y) *)
    pre_load_all_remote_queries p;

    if !global_verbose >= 3 then
      write_log (sprintf "Time after preload: %fs\n%!" (Unix.gettimeofday() -. startt));

    (* for all declared tables +/- *)
    let triggered_insert_table_decls = get_local_tables_triggered p true notif from in
    let triggered_delete_table_decls = get_local_tables_triggered p false notif from in
    let to_assert = flatten (map (change_table_how p true) triggered_insert_table_decls) in
    let to_retract = flatten (map (change_table_how p false) triggered_delete_table_decls) in
    if !global_verbose >= 2 && (length to_assert > 0 || length to_retract > 0) then
    begin
      printf "  *** WILL ADD: %s\n%!" (String.concat " ; " (map (Communication.pretty_print_formula p) to_assert));
      printf "  *** WILL DELETE: %s\n%!" (String.concat " ; " (map (Communication.pretty_print_formula p) to_retract));
    end;
    write_log (sprintf "  *** WILL ADD: %s\n%!" (String.concat " ; " (map (Communication.pretty_print_formula p) to_assert)));
    write_log (sprintf "  *** WILL DELETE: %s\n%!" (String.concat " ; " (map (Communication.pretty_print_formula p) to_retract)));

    if !global_verbose >= 3 then
    begin
      write_log (sprintf "Time after calculating to_assert and to_retract: %fs\n%!" (Unix.gettimeofday() -. startt));
      printf "Time after calculating to_assert and to_retract: %fs\n%!" (Unix.gettimeofday() -. startt);
    end;

   (**********************************************************)
   (* Prepare packets/events to be sent. *)
   (* This must be done BEFORE xsb is updated (output + updates must
      be computed from same EDB, and BEFORE we retract the event *)
   (* for all potentially-triggered outgoing events ...*)
    let outgoing_defns = (get_output_defns_triggered p notif from) in
    let prepared_output = flatten (map (prepare_output p notif from) outgoing_defns) in
    if !global_verbose >= 3 then
      printf "There were %d prepared output actions.\n%!" (length prepared_output);
   (**********************************************************)

    if !global_verbose >= 3 then
      write_log (sprintf "Time after preparing output: %fs\n%!" (Unix.gettimeofday() -. startt));

    (* depopulate event EDB *)
    Communication.retract_event_and_subevents p notif from;

    if !global_verbose >= 3 then
      write_log (sprintf "Time after retracting event and subevents: %fs\n%!" (Unix.gettimeofday() -. startt));

    (* Return the tables that have actually being modified.
       TODO: not as smart as it could be: we aren't checking whether this stuff actually *changes the state*,
             just whether facts are being asserted/retracted. *)
    let modifications = map atom_to_relname (to_assert @ (subtract to_retract to_assert)) in
    modifications_since_last_policy := modifications @ !modifications_since_last_policy;
   (* Now that all the queries are completed, actually do stuff. *)

   (* invalidate policy caches that depend on relations this update changes *)
   invalidate_policy_caches p modifications;

   (**********************************************************)
   (* UPDATE STATE IN XSB as dictated by +/- results stored before
      Semantics demand that retraction happens before assertion here! *)
   (* THIS MUST HAPPEN BEFORE POLICY IS UPDATED *)
    iter Communication.retract_formula to_retract;
    iter Communication.assert_formula to_assert;

    if !global_verbose >= 3 then
      write_log (sprintf "Time after asserting and retracting: %fs\n%!" (Unix.gettimeofday() -. startt));

    if !global_verbose >= 2 then
    begin
      Communication.get_and_print_xsb_state p;

      if !global_verbose >= 3 then
        write_log (sprintf "Time after printing state: %fs\n%!" (Unix.gettimeofday() -. startt));

    end;
   (**********************************************************)

   (**********************************************************)
   (* UPDATE POLICY ON SWITCHES  (use the NEW state)         *)
   (* Don't recreate a policy if there are no state changes! *)
   (* Don't recreate a policy if suppress_new_policy is set. *)
   if not suppress_new_policy then
   begin
    (match !policy_recreation_thunk with
      | Some(t) when (length !modifications_since_last_policy) > 0 -> t();
      | Some(t) -> (); (* we have a thunk, but no updates are necessary *)
      | None -> ());
    modifications_since_last_policy := [];
   end
   else if !global_verbose >= 1 then
     printf "~~ Suppressing new policy generation. ~~\n%!";
   (**********************************************************)

   (**********************************************************)
   (* Finally actually send output (except forward actions; those just get queued) *)
   printf "Queued %d output actions...\n%!" (length prepared_output);
   iter(execute_output p full_packet) prepared_output;
   printf "Output complete.\n%!";

    let forward_actions_todo = !fwd_actions in
   (**********************************************************)

    printf "~~~~~~~~~~~~~~~~~~~FINISHED EVENT (%d total, %d packets) ~~~~~~~~~~~~~~~\n%!"
          !counter_inc_all !counter_inc_pkt;

    (* Unlock the mutex. Make sure nothing uses XSB outside of this.*)
    Mutex.unlock xsbmutex;

    if !global_verbose >= 3 then
      write_log (sprintf "Time to process event completely: %fs\n%!" (Unix.gettimeofday() -. startt));

    (modifications, forward_actions_todo)
(* ) *)
(* return tables that may have changed *)

  with
   | Not_found ->
       Communication.retract_event_and_subevents p notif from;
       Mutex.unlock xsbmutex;
       if !global_verbose > 0 then printf "Nothing to do for this event.\n%!";
       ([],[])
   | exn ->
      begin
        Format.printf "Unexpected exception on event. Event was: %s\n Exception: %s\n----------\n%s\n%!"
          (string_of_event p notif)
          (Printexc.to_string exn)
          (Printexc.get_backtrace ());
        Xsb.halt_xsb();
        Mutex.unlock xsbmutex;
        exit(101);
      end;;




(* Ignore the switch's non-physical ports (pp. 18-19 of OpenFlow 1.0 spec). *)
let ofpp_max_port = nwport_of_string "0xff00";;

(* If not unsafe, packets that will update the switch must be handled entirely by controller *)
let build_total_pol (notifypred: pred) (fwd: pol) (notif: pol) (internal: pol): pol =
  if !global_unsafe then Union(Union(fwd, notif), internal)
  else Union(ITE(notifypred, notif, fwd), internal);;

(* If notables is true, send everything to controller *)
let make_policy_stream (p: flowlog_program)
                       (notables: bool)
                       (reportallpackets: bool) =
  (* stream of policies, with function to push new policies on *)
  let (policies, push) = Lwt_stream.create () in

    let rec switch_event_handler (swev: switchEvent): unit =
      let startt = Unix.gettimeofday() in
      match swev with
      | SwitchUp(sw, feats) ->
        let sw_string = Int64.to_string sw in
        let notifs =
          filter_map (fun portid ->
            if portid < ofpp_max_port then
              Some {typeid="switch_port";
                    values=construct_map [("sw", sw_string);
                                          ("pt", (nwport_to_string portid))]}
            else None)
            feats.ports in
        if (length notifs) < 1 then
        begin
          printf "SWITCH 0x%Lx connected, but did not report any ports.\n%!" sw;
        end
        else
        begin
          printf "SWITCH 0x%Lx connected. Flowlog events triggered: %s\n%!" sw (String.concat ", " (map (string_of_event p) notifs));
          (* Avoid re-computing policy for every port on the switch: *)
          List.iter (fun notif -> ignore (respond_to_notification p ~suppress_new_policy:true notif IncDP)) (tl notifs);
          (* Only recompute policy on the last (first) port: *)
          ignore (respond_to_notification p (hd notifs) IncDP);
          if !global_verbose >= 1 then
          begin
            printf "Total time to process all switch_port events for switch 0x%Lx: %fs.\n%!" sw (Unix.gettimeofday() -. startt);
            write_log (sprintf "Total time to process all switch-up events: %fs.\n%!" (Unix.gettimeofday() -. startt));
          end;
          if !global_verbose > 2 then
          begin
            printf "Function counters:\n%!";
            printf "build_and: %d\n%!" !build_and_count;
          end

        end;
      | SwitchDown(swid) ->
        let sw_string = Int64.to_string swid in
        let notif = {typeid="switch_down"; values=construct_map [("sw", sw_string)]} in
          printf "SWITCH %Lx went down. Triggered: %s\n%!" swid (string_of_event p notif);
          ignore(respond_to_notification p notif IncDP);
      | FlowRemoved(swid, frm) ->

      (* Because Flowlog has no option type in events, we use zero to indicate no constraint.*)
      let build_flow_removed_event (frm: OpenFlow0x01_Core.flowRemoved): (string*string) list =
        printf "Flow Removed on switch %Lx: %s\n%!" swid (OpenFlow0x01.FlowRemoved.to_string frm);
        let open OpenFlow0x01_Core in
        let dlsrcval = (match frm.pattern.dlSrc with | None -> "0" | Some(x) -> macaddr_to_int_string x) in
        let dldstval = (match frm.pattern.dlSrc with | None -> "0" | Some(x) -> macaddr_to_int_string x) in
        let dltypval = (match frm.pattern.dlTyp with | None -> "0" | Some(x) -> string_of_int x) in
        let nwprotoval = (match frm.pattern.nwProto with | None -> "0" | Some(x) -> string_of_int x) in
        let inportval = (match frm.pattern.inPort with | None -> "0" | Some(x) -> string_of_int x) in
        let tpsrcval = (match frm.pattern.tpSrc with | None -> "0" | Some(x) -> tpport_to_int_string x) in
        let tpdstval = (match frm.pattern.tpDst with | None -> "0" | Some(x) -> tpport_to_int_string x) in

        let nwsrcaddrval = (match frm.pattern.nwSrc with | None -> "0" | Some(x) -> nwaddr_to_int_string x.m_value) in
        let nwsrcmaskval = (match frm.pattern.nwSrc with | None -> "32"
                                                         | Some(addrmsk) -> (match addrmsk.m_mask with
                                                                  | None -> "32"
                                                                  | Some(msk) -> nwaddr_to_int_string msk)) in
        let nwdstaddrval = (match frm.pattern.nwDst with | None -> "0" | Some(x) -> nwaddr_to_int_string x.m_value) in
        let nwdstmaskval = (match frm.pattern.nwDst with | None -> "32"
                                                         | Some(addrmsk) -> (match addrmsk.m_mask with
                                                                  | None -> "32"
                                                                  | Some(msk) -> nwaddr_to_int_string msk)) in
        let reasonstr = (match frm.reason with | IdleTimeout -> "idletimeout"
                                               | HardTimeout -> "hardtimeout"
                                               | Delete -> "delete") in
        [("reason",reasonstr);("inport",inportval);
         ("dlsrc",dlsrcval);("dldst",dldstval);
         ("dltyp",dltypval);("nwproto",nwprotoval);
         ("tpsrc",tpsrcval);("tpdst",tpdstval);
         ("nwsrcaddr",nwsrcaddrval);("nwdstaddr",nwdstaddrval);
         ("nwsrcmask",nwsrcmaskval);("nwdstmask",nwdstmaskval);
         ] in

        let sw_string = Int64.to_string swid in
        let notif = {typeid="flow_removed"; values=construct_map ([("sw", sw_string)]@(build_flow_removed_event frm))} in
          printf "Triggered event: %s\n%!" (string_of_event p notif);
          ignore(respond_to_notification p notif IncDP);
    and

    reportPacketCallback (sw: switchId) (pt: port) (pkt: Packet.packet) (buf: int32 option) : NetCore_Types.action =
      printf "[REPORT ONLY] Packet arrived on switch %Ld, port %s.\n%s\n%!"
        sw (NetCore_Pretty.string_of_port pt) (Packet.to_string pkt);
      []

    and
    switch_event_handler_policy = HandleSwitchEvent(switch_event_handler)
    and
    report_all_packets_policy = Action[ControllerAction(reportPacketCallback)]
    and
    internal_policy () = (if reportallpackets then Union(switch_event_handler_policy, report_all_packets_policy)
                          else switch_event_handler_policy)
    and
    (* the thunk needs to know the pkt callback, the pkt callback invokes the thunk. so need "and" *)
    trigger_policy_recreation_thunk (): unit =
      write_log (sprintf "** policy recreation thunk triggered.\n");
      if not notables then
      begin
        let startt = Unix.gettimeofday() in
        (* Update the policy *)
        let (newfwdpol, newnotifpol, newnotifpred) = program_to_netcore p updateFromPacket in

        if !global_verbose > 2 then
          write_log (sprintf "Time after program_to_netcore: %fs" (Unix.gettimeofday() -. startt));


   (*     printf "NEW FWD policy: %s\n%!" (NetCore_Pretty.string_of_pol newfwdpol);
        printf "NEW NOTIF policy: %s\n%!" (NetCore_Pretty.string_of_pol newnotifpol);
     *)
        let newpol = build_total_pol newnotifpred newfwdpol newnotifpol (internal_policy()) in (*Union(Union(newfwdpol, newnotifpol), internal_policy()) in*)
          (* Since can't compare functions, need to use custom comparison *)

          if !global_verbose > 2 then
            write_log (sprintf "Time after total pol: %fs" (Unix.gettimeofday() -. startt));


          let no_update_needed = (safe_compare_pols newpol !last_policy_pushed) in
          if !global_verbose > 2 then
            write_log (sprintf "Time after compare new and old policy: %fs" (Unix.gettimeofday() -. startt));

          if not no_update_needed then
          begin
            counter_pols_pushed := !counter_pols_pushed + 1;

            printf "PUSHING NEW POLICY (number %d)!\n%!" !counter_pols_pushed;
            push (Some newpol);
            (*safe_queue#enqueue_pol newpol;*)
            last_policy_pushed := newpol;
            printf "PUSHED NEW POLICY!\n%!";
            write_log (sprintf "Pushed new policy (number %d).\n%!" !counter_pols_pushed);
            write_log (sprintf "NEW FWD policy: %s\n%!" (NetCore_Pretty.string_of_pol newfwdpol));
            write_log (sprintf "NEW NOTIF policy: %s\n%!" (NetCore_Pretty.string_of_pol newnotifpol));
            write_log (sprintf "NEW NOTIF pred: %s\n%!" (NetCore_Pretty.string_of_pred newnotifpred));
            if !global_verbose > 3 then write_log (sprintf "NEW POLICY: %s\n%!" (NetCore_Pretty.string_of_pol newpol));
          end
          else
          begin
            write_log (sprintf "NEW POLICY was the same. Did not push (last was number %d).\n" !counter_pols_pushed);
            printf "NEW POLICY was the same. Did not push (last was number %d).\n%!" !counter_pols_pushed;
          end;

          if !global_verbose > 2 then
            write_log (sprintf "Time to complete trigger_policy_recreation_thunk: %fs" (Unix.gettimeofday() -. startt));
      end
      else
        if notables then printf "\n*** FLOW TABLE COMPILATION DISABLED! ***\n%!";
        (*DO NOT CALL THIS: push None*)
    and

    (* The callback to be invoked when the policy says to send pkt to controller *)
    (* callback here. *)
    updateFromPacket (sw: switchId) (pt: port) (pkt: Packet.packet) (buf: int32 option): NetCore_Types.action =
      (* Update the policy via the push function *)
      let startt = Unix.gettimeofday() in
      let buf_id = match buf with
                    | Some id -> id
                    | None -> Int32.of_int (-1) in
      printf "[Outside mutex] Packet in on switch %Ld in buffer %ld.\n%s\n%!" sw buf_id (Packet.to_string pkt);
      counter_inc_pkt := !counter_inc_pkt + 1;
      fwd_actions := []; (* populated by things respond_to_notification calls *)

      (* Hack to speed up NIB: don't push a new policy for every probe packet that
         arrives. (This will be replaced by proper LLDP and a built-in NIB in Flowlog 2.0.) *)
      let is_custom_nib_probe = ((Packet.dlTyp pkt) = 0x1001) in

      (* Parse the packet and send it to XSB. Deal with the results *)
      let notif = (pkt_to_event sw pt pkt) in
        printf "~~~ [Outside mutex] Incoming Notif:\n %s\n%!" (string_of_event p notif);

        (* respond_to_notification needs to return actions in a thread-safe way.
           So no more using a !fwd_actions ref. ;-) *)
        (* TODO: concern: this needs to be lwt, not let, in order to force full evaluation before LWT switch.
           But NetCore requires a function of this type, not a thread!

           TODO: WORSE---can't get the fwd_actions_todo without being inside Lwt.t.
              and this callback is fixed by type/netcore reqs to be not inside Lwt.t *)

        let _, fwd_actions_todo = respond_to_notification p
                                                          ~suppress_new_policy:is_custom_nib_probe
                                                          ~full_packet:(make_last_packet sw pt pkt buf) notif IncDP in

        if !global_verbose >= 1 then
        begin
          let used = (Unix.gettimeofday() -. startt) in

            ms_on_packet_processing := !ms_on_packet_processing +. used;

            if !longest_used_packet_ms < used then
              longest_used_packet_ms := used;

            (* Keep an eye on XSB's status *)
            (*if (!counter_inc_pkt mod 200) = 199 then
              Xsb.print_statistics();*)

            printf "Time used: %fs. Average: %fs. Longest: %fs.\n%!"
               used (!ms_on_packet_processing /. (float_of_int !counter_inc_pkt)) !longest_used_packet_ms;
            printf "Asserts: %d. Retracts: %d. Send_asserts: %d. Send_queries: %d\n%!"
              !count_assert_formula !count_retract_formula !count_send_assert !count_send_query;

            if !global_verbose > 2 then
            begin
              printf "Function counters:\n%!";
              printf "build_and: %d\n%!" !build_and_count;
            end
        end;
        if !global_verbose >= 2 then
        begin
          printf "actions will be = %s\n%!" (NetCore_Pretty.string_of_action !fwd_actions);
          write_log (sprintf "actions will be = %s\n%!" (NetCore_Pretty.string_of_action !fwd_actions));
        end;
        (* This callback returns an action set to Frenetic. *)
        fwd_actions_todo in

    (* For use elsewhere: call this to trigger policy update on switches. *)
    policy_recreation_thunk := Some trigger_policy_recreation_thunk;

    if not notables then
    begin
      let (initfwdpol, initnotifpol, initnotifpred) = program_to_netcore p updateFromPacket in
      printf "INITIAL FWD policy is:\n%s\n%!" (NetCore_Pretty.string_of_pol initfwdpol);
      printf "INITIAL NOTIF policy is:\n%s\n%!" (NetCore_Pretty.string_of_pol initnotifpol);
      printf "INITIAL NOTIF pred is:\n%s\n%!" (NetCore_Pretty.string_of_pred initnotifpred);
      let initpol = build_total_pol initnotifpred initfwdpol initnotifpol (internal_policy()) in
        (trigger_policy_recreation_thunk, NetCore_Stream.from_stream initpol policies, push)
    end else begin
      let initpol = Union(switch_event_handler_policy, Action([ControllerAction(updateFromPacket)])) in
        (trigger_policy_recreation_thunk, NetCore_Stream.from_stream initpol policies, push)
    end;;
