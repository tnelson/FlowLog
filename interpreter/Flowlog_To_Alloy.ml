open Printf
open Flowlog_Types

(* Which is the right target language: Alloy or Margrave? 

+Margrave: automatic OSEPL.
+Margrave: easier rule-blaming (Alloy can do w/ added preds or with evaluator. 
           We can probably build blaming into the compiled target?)

+Alloy: easier to express properties (e.g. correctness of TC)
+Alloy: support for TC (could add to Margrave, but would be added engineering)
+Alloy: smoother user experience, visualization, treeview, etc.

+Alloy: better known tool
+Margrave: needs use

*)

(* Some boilerplate (packets, etc.) *)
let alloy_boilerplate (out: out_channel): unit =
	();;

(* Every program's declared notifications need a sig... *)
(* ...and an extensional constraint *)
let alloy_declares (out: out_channel) (p: flowlog_program): unit =
	();;

(* Every RULE in the program gets a predicate *)
let alloy_rules (out: out_channel) (p: flowlog_program): unit =
	();;

(* Every +, every -, every DO gets a predicate IFFing disj of appropriate rules *)
let alloy_actions (out: out_channel) (p: flowlog_program): unit =
	();;

(* transition: st x ev x st 
   (note this is a slight deviation from the language: packet-in becomes an event) *)
let alloy_transition (out: out_channel) (p: flowlog_program): unit =
	();;


let write_as_alloy (p: flowlog_program) (fn: string): unit =
    let out = open_out fn in 
    	alloy_boilerplate out;
    	alloy_declares out p;
    	alloy_rules out p;
    	alloy_actions out p;
    	alloy_transition out p;    	
		close_out out;;
