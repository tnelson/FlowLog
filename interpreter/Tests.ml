open OUnit
open Flowlog_Types
open Partial_Eval
open Printf

(**********************************************************************)
(* Formula Wrangling: NNF, disjunction lifting, ...*)
(**********************************************************************)

let rx =  (FAtom("", "R", [(TVar "x")]));;
let ry =  (FAtom("", "R", [(TVar "y")]));;
let pxy = (FAtom("", "P", [(TVar "x"); (TVar "y")]));;
let nrx = (FNot rx);;
let nry = (FNot ry);;

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
             	 (FOr((FAnd(nrx, nry)), pxy));;

(**********************************************************************)
(* Partial Evaluation *)
(**********************************************************************)

let dummy_rule = Rule("foo", "bar", ADo("R", [], FTrue));;
let newpkt = TVar("newpkt");;
let xvar = TVar("xvar");;
let newpktdlsrc = TField("newpkt", "dlSrc");;
let newpktdldst = TField("newpkt", "dlDst");;
let oldpktdlsrc = TField("pkt", "dlSrc");;
let oldpktdldst = TField("pkt", "dlDst");;
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
let body5 = FAnd(FAtom("", "R", [newpktdlsrc; xvar]), FAtom("", "R", [xvar; newpktdldst]));;           
let cl5 = {orig_rule = dummy_rule; 
           head = FAtom("", "forward", [newpkt]);
           body = body5};;


let test_pe_valid () =
    assert_raises ~msg:"cl1" (IllegalAssignmentViaEquals (FEquals(newpktdlsrc, oldpktdldst))) (fun _ -> validate_clause cl1);
    assert_equal  ~msg:"cl2" (validate_clause cl2) ();
    assert_equal  ~msg:"cl3" (validate_clause cl3) ();
    assert_equal  ~msg:"cl4" (validate_clause cl4) ();
    assert_raises ~msg:"cl5" (IllegalExistentialUse (FAtom("", "R", [xvar; newpktdldst]))) (fun _ -> validate_clause cl5);

;;

(**********************************************************************)
(* SUITE DEFINITION *)
(**********************************************************************)

 let suite = "Flowlog tests" >::: ["test_disj_to_top" >:: test_disj_to_top;
                                   "test_nnf" >:: test_nnf;
                                   "test_pe_valid" >:: test_pe_valid;
                                  ];;
 let _ = run_test_tt ~verbose:true suite;;
