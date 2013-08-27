open OUnit
open Flowlog_Types
open Printf

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
 
 let suite = "Flowlog tests" >::: ["test_disj_to_top" >:: test_disj_to_top;
                                   "test_nnf" >:: test_nnf ];;
 let _ = run_test_tt ~verbose:true suite;;
