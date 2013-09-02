open Flowlog_Types
open ExtList.List
open Printf

(* True if string str1 ends with string str2 *)
let ends_with (str1 : string) (str2 : string) : bool = 
	if String.length str2 > String.length str1 then false
    else (String.sub str1 ((String.length str1) - (String.length str2)) (String.length str2)) = str2;;

(* return list of terms that match pred *)
let rec get_terms (pred: term -> bool) (f: formula) : term list = 
	match f with
		| FTrue -> []
		| FFalse -> []

		| FAtom(modname, relname, tlargs) ->
			filter pred tlargs
		| FEquals(t1, t2) ->
			filter pred [t1; t2]
		| FAnd(f1, f2) ->
			(unique (get_terms pred f1) @ (get_terms pred f2))
		| FNot(innerf) ->
			get_terms pred innerf
		| _ -> failwith "get_terms";;	

let rec get_vars (f: formula) : term list = 	
	get_terms (function | TVar(_) -> true |  _ -> false) f;;
(* as get_vars, but includes fields as well *)
let rec get_vars_and_fieldvars (f: formula) : term list = 
	printf "gcaf: %s\n%!" (string_of_formula f);
	get_terms (function | TVar(_) -> true | TField(_,_) -> true | _ -> false) f;;

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
   Produce [FEquals(TVar(x), TConst("5")), ...] *)
let reassemble_xsb_equality (tlargs: term list) (tuple: string list) : formula list =  
    map2 (fun aterm astr -> 
    	  if (String.get astr 0) = '_' then
		    failwith "reassemble_xsb_equality"
		  else
    		FEquals(aterm, TConst(astr)))
    	 tlargs tuple;;

let subtract (biglst: 'a list) (toremove: 'a list): 'a list =
  (filter (fun ele -> not (mem ele toremove)) biglst);;

let list_intersection (l1: 'a list) (l2: 'a list): 'a list = 
  filter (fun ele1 -> (mem ele1 l2)) l1;;


let is_forward_clause (cl: clause): bool =    
	match cl.head with 
	| FAtom("", "do_forward", _) -> true    
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
  uses_relation "" "packet-in" cl.body;;

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
        | FEquals(t1, t2) -> f
        | FAtom(modstr, relstr, argterms) -> f
        | FOr(f1, f2) -> FOr(nnf f1, nnf f2)
        | FAnd(f1, f2) -> FAnd(nnf f1, nnf f2)
        | FNot(f2) -> 
          match f2 with
            | FTrue -> FFalse
            | FFalse -> FTrue
            | FEquals(t1, t2) -> f
            | FAtom(modstr, relstr, argterms) -> f            
            | FNot(f3) -> f3            
            | FOr(f1, f2) -> FAnd(nnf (FNot f1), nnf (FNot f2))
            | FAnd(f1, f2) -> FOr(nnf (FNot f1), nnf (FNot f2));;
            
(* Assume: NNF before calling this *)
let rec disj_to_top (f: formula): formula = 
    match f with 
        | FTrue -> f;
        | FFalse -> f;
        | FEquals(t1, t2) -> f;
        | FAtom(modstr, relstr, argterms) -> f;
        | FOr(f1, f2) -> f;
        | FNot(f2) -> f; (* since guaranteed to be in NNF *)            
        | FAnd(f1, f2) -> 
            (* Distributive law if necessary *)
            let f1ds = disj_to_list (disj_to_top f1) in
            let f2ds = disj_to_list (disj_to_top f2) in

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

  let get_output_defns (prgm: flowlog_program): sreactive list =
    filter_map (function      
        | ReactOut(relname, arglist, outtype, assigns, spec) as x -> Some x       
        | _ -> None) 
        prgm.reacts;;      

  let is_io_rel (prgm: flowlog_program) (modname: string) (relname: string): bool =
    (* exists is ocaml's ormap *)
    exists (function      
        | DeclInc(rname, argtype) when rname = relname -> true 
        | DeclOut(rname, argtypelst) when rname = relname -> true
        | _ -> false) 
        prgm.decls;;      

  let get_fields_for_type (prgm: flowlog_program) (etype: string): string list =
      let decl = find (function       
        | DeclEvent(evname, evtypelst) when evname = etype -> true 
        | _ -> false) prgm.decls in 
      match decl with 
        | DeclEvent(evname, evfieldlst) -> 
          evfieldlst
        | _ -> failwith "get_fields_for_type";;

  (* in this IO relation, at index idx, there should be something of type T. What are T's fields, in order? *)
  let get_io_fields_for_index (prgm: flowlog_program) (relname: string) (idx: int): string list =
    let decl = find (function       
        | DeclInc(rname, argtype) when rname = relname -> true 
        | DeclOut(rname, argtypelst) when rname = relname -> true
        | _ -> false) prgm.decls in 
      match decl with 
        | DeclInc(rname, argtype) when rname = relname -> 
          get_fields_for_type prgm argtype
        | DeclOut(rname, argtypelst) when rname = relname ->
          get_fields_for_type prgm (nth argtypelst idx)
        | _ -> failwith "get_io_fields_for_index";;

  (* ASSUMED: only one in relation per event *)
  let inc_event_to_formula (p: flowlog_program) (notif: event): formula =
    (* event contains k=v mappings and a type. convert to a formula via defns in program*)
    let defn = find (function       
        | ReactInc(typename, relname) when notif.typeid = typename -> true
        | _ -> false ) p.reacts in 
      match defn with 
        | ReactInc(typename, relname) -> 
          FAtom("", relname, map (fun str -> TConst(str)) (get_fields_for_type p typename))
        | _ -> failwith "inc_event_to_formula";;

  let decls_expand_fields (prgm: flowlog_program) (modname: string) (relname: string) (i: int) (t: term): term list =
    match t with 
      | TVar(vname) when is_io_rel prgm modname relname -> 
        map (fun fldname -> TField(vname, fldname)) (get_io_fields_for_index prgm relname i)
      | _ -> [t];;

let construct_map (bindings: (string * string) list): (string StringMap.t) =
  fold_left (fun acc (bx, by) -> StringMap.add bx by acc) StringMap.empty bindings