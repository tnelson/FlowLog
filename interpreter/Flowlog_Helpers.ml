open Flowlog_Types
open ExtList.List
open Printf
open NetCore_Types


(* True if string str1 ends with string str2 *)
let ends_with (str1 : string) (str2 : string) : bool = 
	if String.length str2 > String.length str1 then false
    else (String.sub str1 ((String.length str1) - (String.length str2)) (String.length str2)) = str2;;

(* return list of terms that match pred *)
let rec get_terms (pred: term -> bool) (f: formula) : term list = 
	match f with
		| FTrue -> []
		| FFalse -> []

		| FAtom(_, _, tlargs) ->
			filter pred tlargs
		| FEquals(t1, t2) ->
			filter pred [t1; t2]
		| FAnd(f1, f2) ->
			(unique (get_terms pred f1) @ (get_terms pred f2))
    | FOr(f1, f2) ->
      (unique (get_terms pred f1) @ (get_terms pred f2))
		| FNot(innerf) ->
			get_terms pred innerf;;	

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
let reassemble_xsb_equality (incpkt: string) (tlargs: term list) (tuple: string list) : formula list =  
    map2 (fun aterm astr -> 
    	if (String.get astr 0) = '_' then
		    failwith "reassemble_xsb_equality: unconstrained variable"
      else match aterm with 
        | TVar(vname) -> FTrue        
		    | _ -> FEquals(aterm, TConst(astr)))
    	 tlargs tuple;;

let reassemble_xsb_atom (modname:string) (relname: string) (tuple: string list): formula =
    FAtom(modname, relname, map (fun x -> TConst(x)) tuple);;

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
		| FAnd(f1, f2) -> (uses_relation goal_modname goal_relname f1) || (uses_relation goal_modname goal_relname f2)			
		| FOr(f1, f2) -> (uses_relation goal_modname goal_relname f1) || (uses_relation goal_modname goal_relname f2)			
		| FNot(innerf) -> uses_relation goal_modname goal_relname innerf			
		| FAtom(modname, relname, tlargs) ->
			relname = goal_relname && modname = goal_modname;;

let is_packet_triggered_clause (cl: clause): bool =
  uses_relation "" packet_in_relname cl.body;;

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
        | FOr(f1, f2) -> FOr(nnf f1, nnf f2)
        | FAnd(f1, f2) -> FAnd(nnf f1, nnf f2)
        | FNot(f2) -> 
          match f2 with
            | FTrue -> FFalse
            | FFalse -> FTrue
            | FEquals(_, _) -> f
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

  let get_local_tables (prgm: flowlog_program): sdecl list =
    filter (function | DeclTable(relname, argtypes) -> true | _ -> false ) 
        prgm.decls;;
(* table name, query name, ip, port, refresh settings *)
  let get_remote_tables (prgm: flowlog_program): (sreactive * sdecl) list =
    filter_map (function   
    	| ReactRemote(relname, qryname, ip, port, refresh) as x -> 
    		Some (x, find (function 
    					| DeclRemoteTable(drel, dargs) when drel = relname -> true 
    					| _ -> false) prgm.decls)
        | _ -> None) 
        prgm.reacts;;    

  let get_remote_table (prgm: flowlog_program) (goalrel: string) : (sreactive * sdecl) =
  	let the_react = find (function   
    	| ReactRemote(relname, qryname, ip, port, refresh) when relname = goalrel -> true    		
        | _ -> false) prgm.reacts in
    let the_decl  = find (function 
    	| DeclRemoteTable(relname, dargs) when relname = goalrel -> true 
    	| _ -> false) prgm.decls in
	(the_react, the_decl);;

  let is_remote_table (prgm: flowlog_program) (relname: string): bool =   
    exists (function      
        | DeclRemoteTable(rname, _) when rname = relname -> true         
        | _ -> false) 
        prgm.decls;;      

  let get_output_defns (prgm: flowlog_program): sreactive list =
    filter_map (function      
        | ReactOut(relname, arglist, outtype, assigns, spec) as x -> Some x       
        | _ -> None) 
        prgm.reacts;;      

  let relation_name_of_defn (r: sreactive): string =
    match r with
      | ReactOut(relname, arglist, outtype, assigns, spec) -> relname
      | ReactRemote(relname, qryname, ip, port, refresh) ->  relname
      | ReactInc(evname, relname) -> relname;;

  let get_input_defn_for_rel (prgm: flowlog_program) (goalrel: string): sreactive =
    find (function      
          | ReactInc(intype, relname) when goalrel = relname -> true 
          | _ -> false) 
        prgm.reacts;;      

  let is_io_rel (prgm: flowlog_program) (modname: string) (relname: string): bool =
    (* exists is ocaml's ormap *)
    exists (function      
        | DeclInc(rname, argtype) when rname = relname -> true 
        | DeclOut(rname, argtypelst) when rname = relname -> true
        | _ -> false) 
        prgm.decls;;      

   (* raises not_found on invalid field *)
  let get_field (ev: event) (fldname: string): string  = 
  	StringMap.find fldname ev.values;; 

  let get_fields_for_type (prgm: flowlog_program) (etype: string): string list =  
      let decl = find (function       
        | DeclEvent(evname, evtypelst) when evname = etype -> true 
        | _ -> false) prgm.decls in 
      match decl with 
        | DeclEvent(evname, evfieldlst) -> 
          evfieldlst
        | _ -> failwith "get_fields_for_type";;

  (* in this IO relation, at index idx, there should be something of type T. What are T's fields, in order? *)
  let get_io_fields_for_index (prgm: flowlog_program) (relname: string) (idx: int): (string list) option =
    let decl = find (function       
        | DeclInc(rname, argtype) when rname = relname -> true 
        | DeclOut(rname, argtypelst) when rname = relname -> true
        | _ -> false) prgm.decls in 
      match decl with 
        | DeclInc(rname, argtype) when rname = relname -> 
          Some (get_fields_for_type prgm argtype)
        | DeclOut(rname, argtypelst) when rname = relname ->
        	if mem relname built_in_condensed_outrels then
        		Some(get_fields_for_type prgm (nth argtypelst idx))
        	else
        		None
          (*get_fields_for_type prgm (nth argtypelst idx)*)
        | _ -> failwith "get_io_fields_for_index";;

  (* ASSUMED: only one in relation per event *)
  (* raises Not_found if nothing to do for this event *)
  let inc_event_to_formula (p: flowlog_program) (notif: event): formula =
    (* event contains k=v mappings and a type. convert to a formula via defns in program*)
    (*printf "Converting event to formula: %s\n%!" (string_of_event notif);*)
    let defn = find (function       
        | ReactInc(typename, relname) when notif.typeid = typename -> true
        | _ -> false ) p.reacts in 
      match defn with 
        | ReactInc(typename, relname) -> 
          FAtom("", relname, map (fun fld -> 
          			TConst(StringMap.find fld notif.values)) 
        			(get_fields_for_type p typename))	
        | _ -> failwith "inc_event_to_formula";;

   (* in modname.relname, the ith element has which fields? *)
  let decls_expand_fields (prgm: flowlog_program) (modname: string) (relname: string) (i: int) (t: term): term list =  	
    match t with 
      | TVar(vname) when is_io_rel prgm modname relname -> 
      	(match (get_io_fields_for_index prgm relname i) with
      		| Some fieldlist -> map (fun fldname -> TField(vname, fldname)) fieldlist
      		| None -> [t])
      | _ -> [t];;

let construct_map (bindings: (string * string) list): (string StringMap.t) =
  fold_left (fun acc (bx, by) -> StringMap.add bx by acc) StringMap.empty bindings

let rec get_atoms (f: formula): formula list = 
	match f with
		| FTrue -> []
		| FFalse -> []
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