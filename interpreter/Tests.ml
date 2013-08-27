open OUnit
open Flowlog_Types

let test_disj_to_top _ =
    assert_equal ~printer:string_of_formula 
                 (disj_to_top (FOr(FFalse, FTrue)))
                 (FOr(FFalse, FTrue));    
    assert_equal ~printer:string_of_formula 
                 (disj_to_top (FAnd(FFalse, FTrue))) 
                 (FAnd(FFalse, FTrue));	
    assert_equal ~printer:string_of_formula 
                 (disj_to_top (FAnd(FOr(FFalse, FTrue), FFalse)))
                 (FOr(FAnd(FFalse, FFalse), FAnd(FFalse, FTrue)));;
 
 let suite = "Flowlog tests" >::: ["test_disj_to_top" >:: test_disj_to_top ];;
 let _ = run_test_tt ~verbose:true suite;;