open OUnit
open Flowlog_Types
open Flowlog_Helpers
open Partial_Eval
open Printf
open ExtList.List

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

let dummy_rule = Rule("foo", "bar", ADo("R", [], FTrue));;
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
           head = FAtom("", "do_forward", [newpkt]);
           body = FEquals(newpktdlsrc, oldpktdldst)};;
let cl2 = {orig_rule = dummy_rule; 
           head = FAtom("", "do_forward", [newpkt]);
           body = FEquals(newpktdlsrc, oldpktdlsrc)};;
let cl3 = {orig_rule = dummy_rule; 
           head = FAtom("", "do_forward", [newpkt]);
           body = FAnd((FAtom("", "R", [newpktdlsrc; xvar])), (FEquals(newpktdlsrc, oldpktdlsrc)))};;
let cl4 = {orig_rule = dummy_rule; 
           head = FAtom("", "do_forward", [newpkt]);
           body = FAnd(FAtom("", "R", [newpktdlsrc; xvar]), FEquals(xvar, oldpktdlsrc))};;
let body5 = FAnd(FAtom("", "R", [oldpktdlsrc; xvar]), FAtom("", "R", [xvar; oldpktdldst]));;           
let cl5 = {orig_rule = dummy_rule; 
           head = FAtom("", "do_forward", [newpkt]);
           body = body5};;
let cl6 = {orig_rule = dummy_rule; 
           head = FAtom("", "do_forward", [newpkt]);
           body = FAnd(FAtom("", "R", [oldpktdlsrc; xvar]), FEquals(oldpktdldst, oldpktdlsrc))};;
let cl7 = {orig_rule = dummy_rule; 
           head = FAtom("", "do_forward", [newpkt]);
           body = FAnd(FAtom("", "R", [oldpktdlsrc; xvar]), FNot(FEquals(newpktdldst,oldpktdldst)))};;
let cl8 = {orig_rule = dummy_rule; 
           head = FAtom("", "do_forward", [newpkt]);
           body = FAnd(FAtom("", "R", [oldpktdlsrc; xvar]), FNot(FEquals(newpktlocpt,oldpktlocpt)))};;

let test_pe_valid () =
    assert_raises  ~msg:"cl1" (IllegalAssignmentViaEquals (FEquals(newpktdlsrc, oldpktdldst))) (fun _ -> validate_clause cl1);
    assert_equal   ~msg:"cl2" (validate_clause cl2) ();
    (*assert_equal  ~msg:"cl3" (validate_clause cl3) ();*)  
    assert_raises  ~msg:"cl3" (IllegalModToNewpkt(newpktdlsrc, newpktdlsrc)) (fun _ -> (validate_clause cl3));
    (*assert_equal   ~msg:"cl4" (validate_clause cl4) ();*)
    assert_raises  ~msg:"cl4" (IllegalModToNewpkt(newpktdlsrc, newpktdlsrc)) (fun _ -> (validate_clause cl4));
    assert_raises  ~msg:"cl5" (IllegalExistentialUse (FAtom("", "R", [xvar; oldpktdldst]))) (fun _ -> validate_clause cl5);
    assert_raises  ~msg:"cl6" (IllegalEquality(oldpktdldst,oldpktdlsrc)) (fun _ -> validate_clause cl6);
    assert_raises  ~msg:"cl7" (IllegalEquality(newpktdldst,oldpktdldst)) (fun _ -> validate_clause cl7);
    assert_equal   ~msg:"cl8" (validate_clause cl8) ();;

let test_strip_to_valid () = 
    (* invalid because of bad newpkt assignment. now we only care about the predicate. so OK as is. *)
    assert_equal ~msg:"cl1" (strip_to_valid cl1) cl1;;


(**********************************************************************)
(* SUITE DEFINITION *)
(**********************************************************************)

 let suite = "Flowlog tests" >::: ["test_disj_to_top" >:: test_disj_to_top;
                                   "test_nnf" >:: test_nnf;
                                   "test_minimize_variables" >:: test_minimize_variables;
                                   "test_pe_valid" >:: test_pe_valid;
                                   "test_strip_to_valid" >:: test_strip_to_valid;
                                  ];;
 let _ = run_test_tt ~verbose:true suite;;
