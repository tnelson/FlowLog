open Printf
open Flowlog_Types
open Flowlog_Helpers
open ExtList.List
open Unix 

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

(**********************************************************)
(* Some boilerplate (packets, etc.) *)    

let alloy_boilerplate (out: out_channel): unit =
  let localtm = localtime (gettimeofday()) in 
  fprintf out "// Produced automatically by flowlog -alloy at %d:%d:%d on %d %d %d\n%!"
              localtm.tm_hour localtm.tm_min localtm.tm_sec 
              localtm.tm_mon localtm.tm_mday (localtm.tm_year + 1900); 
  fprintf out "%s\n%!" "
sig Event {}
sig Switch {}
sig MacAddr {}
sig IPAddr {}
sig EthTyp {}
sig PhysicalPort {} 
sig NwProtocol {}";;

(**********************************************************)
(* Every program's declared notifications need a sig... *)
(* ...and an extensional constraint *)
let alloy_declares (out: out_channel) (p: flowlog_program): unit =
  let declare_event (decl: sdecl) =
    match decl with 
      | DeclEvent(evname, evfields) ->  
        let ifislone = if length evfields > 0 then "" else "lone " in
          fprintf out "%ssig EV%s extends Event {\n%!" ifislone evname;
          let flddecls = map (sprintf "    %s: one X") evfields in 
            fprintf out "%s%!" (String.concat ",\n" flddecls);
            fprintf out "}\n\n%!";

        if length evfields > 0 then 
        begin
          fprintf out "fact EV%sExtensional { all ev1, ev2: EV%s | \n%!" evname evname;        
          let fieldequals = (map (fun fld -> sprintf "ev1.%s = ev2.%s" fld fld) evfields) in
          let fieldsequal = String.concat "&&" fieldequals in 
            fprintf out "(%s) implies ev1 = ev2}\n\n%!" fieldsequal;
        end;
      | _ -> failwith "declare_event"
  in
  	iter declare_event (filter (function | DeclEvent(_,_) -> true | _ -> false) p.decls);;

(**********************************************************)
(* Tables in state sig, and extensional fact for states (incl. all tables) *)

let get_tablename (tdecl: sdecl): string = 
  match tdecl with
    | DeclTable(tblname, fieldtypes) 
    | DeclRemoteTable(tblname, fieldtypes) -> 
      tblname
    | _ -> failwith "get_tablename";;

let alloy_state (out: out_channel) (p: flowlog_program): unit =
  let declare_state (decl: sdecl) = 
    match decl with 
      | DeclTable(tblname, fieldtypes) 
      | DeclRemoteTable(tblname, fieldtypes) -> 
        let typesproduct = String.concat " -> " fieldtypes in 
          fprintf out "    %s: %s\n%!" tblname typesproduct
      | _ -> failwith "declare_state"
  in

  let local_tables = get_local_tables p in 
  let remote_tables = (map (fun (react, decl) -> decl) (get_remote_tables p)) in
    fprintf out "sig State {\n%!";
    iter declare_state local_tables;
    iter declare_state remote_tables;
    fprintf out "}\n%!";
    fprintf out "fact StateExtensional { all st1, st2: State |\n%!";
    let stateequals = (map (fun tblname -> sprintf "st1.%s = st2.%s" tblname tblname)
                           (map get_tablename (local_tables @ remote_tables))) in
    let statesequal = String.concat " && " stateequals in 
            fprintf out "(%s) implies st1 = st2}\n\n%!" statesequal;;

(**********************************************************)
(* Every RULE in the program gets a predicate *)
let alloy_rules (out: out_channel) (p: flowlog_program): unit =
	();;

(**********************************************************)
(* Every +, every -, every DO gets a predicate IFFing disj of appropriate rules *)
let alloy_actions (out: out_channel) (p: flowlog_program): unit =
	();;

(**********************************************************)
(* transition: st x ev x st 
   (note this is a slight deviation from the language: packet-in becomes an event) *)
let alloy_transition (out: out_channel) (p: flowlog_program): unit =
	();;

(**********************************************************)
let write_as_alloy (p: flowlog_program) (fn: string): unit =
    let out = open_out fn in 
    	alloy_boilerplate out;      
    	alloy_declares out p;
      alloy_state out p;
    	alloy_rules out p;
    	alloy_actions out p;
    	alloy_transition out p;    	
		  close_out out;
      printf "~~~ Finished compiling to Alloy. ~~~\n%!";;
