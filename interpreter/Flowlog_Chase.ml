open Flowlog_Types
open Flowlog_Parse_Helpers
open Flowlog_Helpers
open Printf
open ExtList.List

(**********************************************************************)
(* Experiments with change-impact in Flowlog *)
(**********************************************************************)

(* A "partial" model: set of atomic formulas *)
type pmodel = formula list;;

(* Guaranteed: no recursion in the clauses. *)
let evaluate_clauses (clauses2: clause list) (db: pmodel): pmodel =
  [];;

let var_to_const (t: term): term =
  match t with
    | TField(vid, fid) -> TConst(vid^"_"^fid)
    | TVar(vid) -> TConst(vid)
    | _ -> t;;

(* turning = into a relation means we have to check for contradictions later *)
let var_to_const_fmla (f: formula): formula =
  match f with 
    | FTrue | FFalse -> f
    | FAtom(m, r, args) -> FAtom(m, r, map var_to_const args)
    | FEquals(t1, t2) -> FAtom("", "EQ", [var_to_const t1; var_to_const t2])
    | FNot(FEquals(t1, t2)) -> FAtom("", "NOT_EQ", [var_to_const t1; var_to_const t2])
    | FNot(FAtom(m, r, args)) -> FAtom(m, "NOT_"^r, map var_to_const args)
    | _ -> failwith ("var_to_const: "^(string_of_formula f));;

let db_of_clause (cl: clause): pmodel*formula =
  (map var_to_const_fmla (conj_to_list cl.body), var_to_const_fmla cl.head);;

(* Convert cl to clause-database, then evaluate clauses2 on it *)
let test_rule_containment (cl: clause) (clauses2: clause list): pmodel option =
  let (db, expected) = db_of_clause cl in
  let result = evaluate_clauses clauses2 db in
  if mem expected result then None
  else Some db;;

let build_chase_containment (prgm1: flowlog_program) (prgm2: flowlog_program): pmodel list =
  fold_left (fun (acc: pmodel list) (cl: clause) -> 
      match test_rule_containment cl prgm2.clauses with
        | Some missed -> missed::acc
        | None -> acc)
    [] prgm1.clauses;;

let test1 = build_chase_containment
    (desugared_program_of_ast (read_ast "examples/NIB.flg"))
    (desugared_program_of_ast (read_ast "examples/NIB.flg"));;

let string_of_pmodel (mdl: pmodel): string = 
  String.concat ", " (map string_of_formula mdl);;

printf "%s\n%!" (String.concat ";\n " (map string_of_pmodel test1));;