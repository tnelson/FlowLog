open Flowlog_Types
open Flowlog_Parse_Helpers
open Flowlog_Helpers
open Printf
open ExtList.List
open Xsb_Communication

(**********************************************************************)
(* Experiments with change-impact in Flowlog *)
(**********************************************************************)

(* A "partial" model: set of atomic formulas *)
type pmodel = formula list;;

let string_of_pmodel (mdl: pmodel): string = 
  "{"^(String.concat ", " (map string_of_formula mdl))^"}";;

(* Guaranteed: no recursion in the clauses. *)
(* So _assert_ each piece of the db to XSB, then evaluate whether goal is true. *)
let query_containment (prgm: flowlog_program) (db: pmodel) (expected: formula): bool =  
  printf "\n---------- checking containment for %s...\n%!" (string_of_pmodel db);
  iter Communication.assert_formula db;
  
  (* Do we get the expected formula?*)
  let results = (Communication.get_state expected) in
  let found = (length results) > 0 in

    iter Communication.retract_formula db;
    printf "done checking containment. found=%b\n%!" found;  
    (*printf "results = %s\n%!" results;*)
    found;;

let var_to_const (t: term): term =
  match t with
    | TField(vid, fid) -> TConst(vid^"_"^fid)
    | TVar(vid) -> TConst(vid)
    | _ -> t;;

(* turning = into a relation means we have to check for contradictions later *)
let var_to_const_fmla (f: formula): formula option =
  match f with 
    | FTrue -> None
    | FFalse -> failwith "false formula in body: cannot execute this rule"
    | FAtom(m, r, args) -> Some(FAtom(m, r, map var_to_const args))
    | FEquals(t1, t2) -> Some(FAtom("", "equal", [var_to_const t1; var_to_const t2]))
    | FNot(FEquals(t1, t2)) -> Some(FAtom("", "not_equal", [var_to_const t1; var_to_const t2]))
    | FNot(FAtom(m, r, args)) -> Some(FAtom(m, "not_"^r, map var_to_const args))
    | _ -> failwith ("var_to_const: "^(string_of_formula f));;

let sod (x: 'a option): 'a =
  match x with
    | Some y -> y
    | None -> failwith "sod";;

let db_of_clause (prgm: flowlog_program) (cl: clause): pmodel*formula =
    (*iter (printf "b: %s\n%!") (map string_of_formula (map (Communication.subs_xsb_formula prgm) (conj_to_list cl.body)));
    printf "h: %s\n%!" (string_of_formula (Communication.subs_xsb_formula prgm cl.head));*)
    (filter_map var_to_const_fmla (map (Communication.subs_xsb_formula prgm) (conj_to_list cl.body))
    ,sod (var_to_const_fmla (Communication.subs_xsb_formula prgm cl.head))
    );;  

(* Convert cl to clause-database, then evaluate clauses2 on it 
   return the pmodel if containment fails for it *)
let test_rule_containment (prgm: flowlog_program) (cl: clause): pmodel option =
  let (db, expected) = db_of_clause prgm cl in
  if query_containment prgm db expected then None
  else Some db;;

let build_chase_containment (prgm1: flowlog_program) (prgm2: flowlog_program): pmodel list =
  Mutex.lock xsbmutex;
  (* reset XSB *)  
  Xsb.halt_xsb();
  (* Hand XSB the clauses of the program we're leaving intact *)
  Communication.start_program prgm2 ~forcepositive:true true;

  (* Hand XSB "xor" axioms for relations *)

  (* test each rule of the program we're breaking up *)
  let result = fold_left (fun (acc: pmodel list) (cl: clause) -> 
      match test_rule_containment prgm1 cl with
        | Some missed -> missed::acc
        | None -> acc)
    [] prgm1.clauses in 
    Mutex.unlock xsbmutex;
    result;;

let build_chase_equivalence (prgm1: flowlog_program) (prgm2: flowlog_program): pmodel list =
  build_chase_containment prgm1 prgm2 @ 
  build_chase_containment prgm2 prgm1;;

(*
let example_run(): unit = 
  let test1 = build_chase_equivalence
      (desugared_program_of_ast (read_ast "examples/Mac_Learning.flg") "examples/Mac_Learning.flg")
      (desugared_program_of_ast (read_ast "tests/Mac_Learning_Diffnames.flg") "tests/Mac_Learning_Diffnames.flg") in

  let test2 = build_chase_equivalence
      (desugared_program_of_ast (read_ast "examples/Mac_Learning.flg") "examples/Mac_Learning.flg")
      (desugared_program_of_ast (read_ast "tests/Mac_Learning_Missing_Flood.flg") "tests/Mac_Learning_Missing_Flood.flg") in
     
    printf "1 < 2 or 2 > 1LACKING: %s\n%!" (String.concat ";\n " (map string_of_pmodel test1));;    
*)

(* TODO: check examples. especially ones that involve equality. do we need to axiomatize to be sound? etc. *)

(* Concern 1: Equality. Unsound since do not produce symmetric/reflexive/transitive facts? *)
(* Concern 2: Negation. Need to express p xor np. Also: is this still sound/complete? *)
(* Concern 3: Coverage. Can't give planner _every_ model in the partial returned here, 
   because may be SOME overlap. Just know that there exists a model in the partial that 
   isn't reflected in other program.*)

(*  example_run();;    *)