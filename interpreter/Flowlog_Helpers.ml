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
