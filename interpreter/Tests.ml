open OUnit
open Flowlog_Types
open Flowlog_Helpers
open Partial_Eval
open Partial_Eval_Validation
open Printf
open ExtList.List
open NetCore_Types
open NetCore_Pattern
open Flowlog_Parse_Helpers

(**********************************************************************)
(* Formula Wrangling: NNF, disjunction lifting, substitution ...*)
(**********************************************************************)

let rx =  (FAtom("", "R", [(TVar "x")]));;
let ry =  (FAtom("", "R", [(TVar "y")]));;
let pxy = (FAtom("", "P", [(TVar "x"); (TVar "y")]));;
let pxx = (FAtom("", "P", [(TVar "x"); (TVar "x")]));;
let pxz = (FAtom("", "P", [(TVar "x"); (TVar "z")]));;
let pyy = (FAtom("", "P", [(TVar "y"); (TVar "y")]));;
let px7 = (FAtom("", "P", [(TVar "x"); (TConst "7")]));;
let nrx = (FNot rx);;
let nry = (FNot ry);;

(* Dummy rule used to populate clauses' orig_rule field *)
let dummy_rule = {onrel="foo"; onvar="p"; action=ADo("R", [], FTrue)};;

let newpkt = TVar("newpkt");;
let xvar = TVar("x");;
let yvar = TVar("y");;
let zvar = TVar("z");;
let const5 = TConst("5");;
let const7 = TConst("7");;
let constfoo = TConst("foo");;
let newpktdlsrc = TField("newpkt", "dlsrc");;
let newpktdldst = TField("newpkt", "dldst");;
let oldpktdlsrc = TField("pkt", "dlsrc");;
let oldpktdldst = TField("pkt", "dldst");;
let newpktlocpt = TField("newpkt", "locpt");;
let oldpktlocpt = TField("pkt", "locpt");;


let split_disj1 = (FOr (FAnd(FAtom("", "p1", []), FAtom("", "p2", [])),
                       (FAnd(FAtom("", "p3", []), FAtom("", "p4", [])))));;


             (*	 printf "%s\n%!" (string_of_formula (nnf (FOr((FNot (FOr(rx, ry))), pxy))));;
             	 printf "%s\n%!" (string_of_formula (disj_to_top (nnf (FOr((FNot (FOr(rx, ry))), pxy)))));;*)

let test_nnf () =
	assert_equal ~printer:string_of_formula
				 (nnf (FNot(FNot(rx))))
				 rx;
	assert_equal ~printer:string_of_formula
				 (nnf (FNot(FOr(rx, ry))))
				 (FAnd(nrx, nry));
	assert_equal ~printer:string_of_formula
				 (nnf (FNot(FAnd(rx, ry))))
				 (FOr(nrx, nry));
	assert_equal ~printer:string_of_formula
				 (nnf (FNot(FOr(nrx, nry))))
				 (FAnd(rx, ry));;

let test_disj_to_top () =
    assert_equal ~printer:string_of_formula
                 (disj_to_top (nnf (FOr(FFalse, FTrue))))
                 (FOr(FFalse, FTrue));
    assert_equal ~printer:string_of_formula
                 (disj_to_top (nnf (FAnd(FFalse, FTrue))))
                 (FAnd(FFalse, FTrue));
    assert_equal ~printer:string_of_formula
                 (disj_to_top (nnf (FAnd(FOr(FFalse, FTrue), FFalse))))
                 (FOr(FAnd(FFalse, FFalse), FAnd(FTrue, FFalse)));
    assert_equal ~printer:string_of_formula
             	   (disj_to_top (nnf (FOr(FNot(FOr(rx, ry)), pxy))))
             	   (FOr((FAnd(nrx, nry)), pxy));
    assert_equal ~printer:string_of_formula
                 (disj_to_top split_disj1)
                split_disj1;;

let gather_printer (lst: (term * term) list): string =
  String.concat ";" (map (fun apair -> let (t1, t2) = apair in
    (string_of_term t1)^","^(string_of_term t1)) lst);;

let test_minimize_variables () =
    assert_equal ~printer:gather_printer
                 ~msg:"gather1"
                [(xvar, yvar)]
                (gather_nonneg_equalities_involving_vars (FEquals(xvar, yvar)) false);

    (* This process does not guarantee logical equivalence.
       It guarantees logical equivalence in sig with fewer variables.
       Hence x=y --> true, because if x=y then y=y then true *)
    assert_equal ~printer:string_of_formula
                 ~msg:"minimize1"
                (minimize_variables (FEquals(xvar, yvar)))
                FTrue;
    (* negated equality isn't used in substitution *)
    assert_equal ~printer:string_of_formula
                 ~msg:"minimize2"
                (minimize_variables (FAnd(pxy, (FNot (FEquals(xvar, yvar))))))
                (FAnd(pxy, (FNot (FEquals(xvar, yvar)))));
    (* But P(x, y) & x=y produces P(x, x) *)
    assert_equal ~printer:string_of_formula
                 ~msg:"minimize3"
                (minimize_variables (FAnd(pxy, (FEquals(xvar, yvar)))))
                pyy;
    (* follow chain of equalities *)
    assert_equal ~printer:string_of_formula
                 ~msg:"minimize4"
                (minimize_variables (FAnd(FEquals(xvar, zvar), (FAnd(pxy, (FEquals(zvar, yvar)))))))
                pyy;
    (* ...for constants *)
    assert_equal ~printer:string_of_formula
                 ~msg:"minimize5"
                (minimize_variables (FAnd(FEquals(yvar, zvar), (FAnd(pxy, (FEquals(zvar, const7)))))))
                px7;
    (* x->7, y->x needs to fully reduce. don't "lose" the target for the y (i.e. each step of subs must be global) *)
    assert_equal ~printer:string_of_formula
                 ~msg:"minimize9"
                (minimize_variables ~exempt:[yvar] (FAnd(FEquals(xvar, const7), (FEquals(yvar, xvar)))))
                (FEquals(yvar, const7));
    (* but don't get rid of exempts *)
    assert_equal ~printer:string_of_formula
                 ~msg:"minimize6"
                (minimize_variables ~exempt:[yvar] (FAnd(FEquals(yvar, zvar), (FAnd(pxz, (FEquals(zvar, const7)))))))
                (FAnd(FEquals(yvar, const7), px7));
    (* make sure exempt on only one side doesn't prevent subsing out other side *)
    assert_equal ~printer:string_of_formula
                 ~msg:"minimize7"
                (minimize_variables ~exempt:[yvar] (FAnd(FEquals(zvar, yvar), (FAnd(pxz, (FEquals(const7, zvar)))))))
                (FAnd(FEquals(const7, yvar), px7));
    (* If conflicting constants? UNSAT! *)
    assert_equal ~printer:string_of_formula
                 ~msg:"minimize8"
                (minimize_variables (FAnd(FEquals(zvar, const5), (FEquals(const7, zvar)))))
                FFalse;


              ;;

(**********************************************************************)
(* Partial Evaluation *)
(**********************************************************************)

let cl1 = {orig_rule = dummy_rule;
           head = FAtom("", "forward", [newpkt]);
           body = FEquals(newpktdlsrc, oldpktdldst)};;
let cl2 = {orig_rule = dummy_rule;
           head = FAtom("", "forward", [newpkt]);
           body = FEquals(newpktdlsrc, oldpktdlsrc)};;
let cl3 = {orig_rule = dummy_rule;
           head = FAtom("", "forward", [newpkt]);
           body = FAnd((FAtom("", "R", [newpktdlsrc; xvar])), (FEquals(newpktdlsrc, oldpktdlsrc)))};;
let cl4 = {orig_rule = dummy_rule;
           head = FAtom("", "forward", [newpkt]);
           body = FAnd(FAtom("", "R", [newpktdlsrc; xvar]), FEquals(xvar, oldpktdlsrc))};;
let body5 = FAnd(FAtom("", "R", [oldpktdlsrc; xvar]), FAtom("", "R", [xvar; oldpktdldst]));;
let cl5 = {orig_rule = dummy_rule;
           head = FAtom("", "forward", [newpkt]);
           body = body5};;
let cl6 = {orig_rule = dummy_rule;
           head = FAtom("", "forward", [newpkt]);
           body = FAnd(FAtom("", "R", [oldpktdlsrc; xvar]), FEquals(oldpktdldst, oldpktdlsrc))};;
let cl7 = {orig_rule = dummy_rule;
           head = FAtom("", "forward", [newpkt]);
           body = FAnd(FAtom("", "R", [oldpktdlsrc; xvar]), FNot(FEquals(newpktdldst,oldpktdldst)))};;
let cl8 = {orig_rule = dummy_rule;
           head = FAtom("", "forward", [newpkt]);
           body = FAnd(FAtom("", "R", [oldpktdlsrc; xvar]), FNot(FEquals(newpktlocpt,oldpktlocpt)))};;

let test_pe_valid () =

    (* outdated *)
    (*assert_raises  ~msg:"cl1" (IllegalAssignmentViaEquals (FEquals(newpktdlsrc, oldpktdldst))) (fun _ -> validate_fwd_clause cl1);*)

    ignore (validate_and_process_pkt_triggered_clause cl2);

    (*assert_equal   ~msg:"cl2" (validate_fwd_clause cl2) ();
    (*assert_equal  ~msg:"cl3" (validate_clause cl3) ();*)
    assert_raises  ~msg:"cl3" (IllegalModToNewpkt(newpktdlsrc, newpktdlsrc)) (fun _ -> (validate_fwd_clause cl3));
    (*assert_equal   ~msg:"cl4" (validate_clause cl4) ();*)
    assert_raises  ~msg:"cl4" (IllegalModToNewpkt(newpktdlsrc, newpktdlsrc)) (fun _ -> (validate_fwd_clause cl4));
    assert_raises  ~msg:"cl5" (IllegalExistentialUse (FAtom("", "R", [xvar; oldpktdldst]))) (fun _ -> validate_fwd_clause cl5);
    assert_raises  ~msg:"cl6" (IllegalEquality(oldpktdldst,oldpktdlsrc)) (fun _ -> validate_fwd_clause cl6);
    assert_raises  ~msg:"cl7" (IllegalEquality(newpktdldst,oldpktdldst)) (fun _ -> validate_fwd_clause cl7);
    assert_equal   ~msg:"cl8" (validate_fwd_clause cl8) ();;
*)
    ();;

let cl9 = {orig_rule = dummy_rule;
           head = FAtom("", "plus_foo", [xvar]);
           body = FAnd(FAtom("", "R", [yvar; xvar]), (FNot(FAtom("", "R", [yvar; const7]))))};;
let cl9b = {orig_rule = dummy_rule;
            head = FAtom("", "plus_foo", [xvar]);
            body = FAtom("", "R", [yvar; xvar])};;
let cl10 = {orig_rule = dummy_rule;
           head = FAtom("", "plus_foo", [xvar]);
           body = FAnd((FNot(FEquals(oldpktdldst, oldpktdlsrc))), FAnd(FAtom("", "P", [oldpktdldst]), FAtom("", "R", [yvar; xvar; oldpktdldst])))};;
let cl10b = {orig_rule = dummy_rule;
            head = FAtom("", "plus_foo", [xvar]);
            body = FAnd(FAtom("", "P", [oldpktdldst]), FAtom("", "R", [yvar; xvar; oldpktdldst]))};;


(*
let test_strip_to_valid () =
    (* invalid because of bad newpkt assignment. now we only care about the predicate. so OK as is. *)
    assert_equal ~msg:"cl1" ~printer:string_of_clause cl1 (strip_to_valid "pkt" cl1);;
    assert_equal ~msg:"cl9" ~printer:string_of_clause cl9b (strip_to_valid "pkt" cl9);;
    (* remove bad equality, don't count FIELDS of oldpkt var*)
    assert_equal ~msg:"cl10" ~printer:string_of_clause cl10b (strip_to_valid "pkt" cl10);;
let allhdrs = Hdr(all);;
*)

(*let test_build_switch_pred () =
    let (old_cl1, trimmed_cl1) = trim_packet_from_body cl1.body in
      assert_equal ~printer:NetCore_Pretty.string_of_pred ~msg:"cl1"
        (build_switch_pred old_cl1 trimmed_cl1)
        allhdrs;;
*)

let try_to_open filename =
  (fun _ ->
    ignore (desugared_program_of_ast (read_ast filename) filename));;

let test_parse_errors () =
  assert_raises ~msg:"bad input field in head" (UndeclaredField("swpt","notafieldomg"))
                (try_to_open "./tests/bad_input_field_in_head.flg");
  assert_raises ~msg:"bad input field in body" (UndeclaredField("swpt","omgwtfbbq"))
                (try_to_open "./tests/bad_input_field_in_body.flg");
  assert_raises ~msg:"bad DO output field in WHERE" (UndeclaredField("new","omgwtfbbq"))
                (try_to_open "./tests/bad_do_field.flg");
  assert_raises ~msg:"bad DO output field in WHERE" (UndeclaredField("swpt","swx"))
                (try_to_open "./tests/bad_insert_field.flg");
  assert_raises ~msg:"INSERT/DELETE bad field" (NonCondensedNoField("swpt"))
                (try_to_open "./tests/bad_insert_not_field.flg");
  assert_raises ~msg:"multiple io-reacts for one relation" (RelationHadMultipleReacts("emit"))
                (try_to_open "./tests/multiple_reacts_on_emit.flg");
  assert_raises ~msg:"multiple io-decls for one relation" (RelationHadMultipleDecls("emit"))
                (try_to_open "./tests/multiple_decls_of_emit.flg");
  assert_raises ~msg:"DO args mixed field and nonfield of inc var" (NonCondensedNoField("pkt"))
                (try_to_open "./tests/bad_do_mixed_field_nonfield.flg");
  assert_raises ~msg:"Bad arity of output relation (INSERT)" (BadArityOfTable("switch_has_port"))
                (try_to_open "./tests/bad_insert_arity.flg");

  ();;

let test_ip_conversion () =
  (* Test intstr (not dotted) -> Int32 *)
  assert_equal ~msg:"conv1" ~printer:Int32.to_string (nwaddr_of_int_string "12345") (Int32.of_int 12345);
  assert_equal ~msg:"conv2" ~printer:Int32.to_string (nwaddr_of_int_string "3000000000") (Int32.of_string "-1294967296");
  assert_equal ~msg:"conv3" (nwaddr_of_int_string (nwaddr_to_int_string (Packet.ip_of_string "192.168.0.1"))) (Packet.ip_of_string "192.168.0.1");

  (* Test Int32 -> pretty print *)
  assert_equal ~msg:"conv4" (pretty_print_value "ipaddr" (nwaddr_to_int_string (Packet.ip_of_string "192.168.0.1"))) "192.168.0.1";

  (* Test containment check (this tests in OCaml, not XSB!) *)
  let h1 = (Packet.ip_of_string "192.168.0.1") in
  let a1 = (Packet.ip_of_string "192.168.0.0") in
  let a2 = (Packet.ip_of_string "192.168.1.0") in
    assert_equal ~msg:"contain1" (is_in_ip_range h1 a1 24) true;
    assert_equal ~msg:"contain2" (is_in_ip_range h1 a2 24) false;
  ();;

(**********************************************************************)
(* SUITE DEFINITION *)
(**********************************************************************)

 let suite = "Flowlog tests" >::: [
                                   "test_parse_errors" >:: test_parse_errors;
                                   "test_disj_to_top" >:: test_disj_to_top;
                                   "test_nnf" >:: test_nnf;
                                   "test_minimize_variables" >:: test_minimize_variables;
                                   "test_pe_valid" >:: test_pe_valid;
                                   "test_ip_conversion" >:: test_ip_conversion;
                                   (*"test_strip_to_valid" >:: test_strip_to_valid;*)
                                   (*"test_build_switch_pred" >:: test_build_switch_pred;*)
                                  ];;
 let _ = run_test_tt ~verbose:true suite;;
