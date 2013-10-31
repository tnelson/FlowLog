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

(* Guaranteed: no recursion in the clauses. *)
(* So _assert_ each piece of the db to XSB, then evaluate whether goal is true. *)
let query_containment (clauses2: clause list) (db: pmodel) (expected: formula): bool =  
  printf "checking containment...\n%!";
  iter Communication.assert_formula db;

  iter Communication.retract_formula db;
  printf "done checking containment...\n%!";
  (*Xsb.halt_xsb ();*)
  false;;

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

let db_of_clause (cl: clause): pmodel*formula =
  (filter_map var_to_const_fmla (conj_to_list cl.body), sod (var_to_const_fmla cl.head));;

(* Convert cl to clause-database, then evaluate clauses2 on it *)
let test_rule_containment (cl: clause) (clauses2: clause list): pmodel option =
  let (db, expected) = db_of_clause cl in
  if query_containment clauses2 db expected then None
  else Some db;;

let build_chase_containment (prgm1: flowlog_program) (prgm2: flowlog_program): pmodel list =
  fold_left (fun (acc: pmodel list) (cl: clause) -> 
      match test_rule_containment cl prgm2.clauses with
        | Some missed -> missed::acc
        | None -> acc)
    [] prgm1.clauses;;

let string_of_pmodel (mdl: pmodel): string = 
  String.concat ", " (map string_of_formula mdl);;

let example_run(): unit = 
  let test1 = build_chase_containment
      (desugared_program_of_ast (read_ast "examples/NIB.flg"))
      (desugared_program_of_ast (read_ast "examples/NIB.flg")) in

    printf "%s\n%!" (String.concat ";\n " (map string_of_pmodel test1));;

(*example_run();;    *)