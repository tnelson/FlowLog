open Flowlog_Types
open ExtList.List
open Printf

(* True if string str1 ends with string str2 *)
let ends_with (str1 : string) (str2 : string) : bool = 
	if String.length str2 > String.length str1 then false
    else (String.sub str1 ((String.length str1) - (String.length str2)) (String.length str2)) = str2;;

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


let is_forward_clause (cl: clause): bool =
	match cl.head with 
	| FAtom("", "forward", _) -> true
	| _ -> false;;

let rec uses_relation (modname: string) (relname: string) (f: formula): bool =
	match f with
		| FTrue -> false
		| FFalse -> false
		| FEquals(t1, t2) -> false		
		| FAnd(f1, f2) -> (uses_relation modname relname f1) || (uses_relation modname relname f2)			
		| FOr(f1, f2) -> (uses_relation modname relname f1) || (uses_relation modname relname f2)			
		| FNot(innerf) -> uses_relation modname relname innerf			
		| FAtom(modname, relname, tlargs) ->
			relname = relname && modname = modname;;

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
