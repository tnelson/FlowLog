(*
  Modified from ocaml tutorial by Tim

  This is the FlowLog server for catching notifications,
    sending queries, and sending notifications.
*)

open Arg
open Thrift
open Flowlog_types

(* "Die, Bart, Die!" is German for "The, Bart, The!". -- Sideshow Bob *)
exception Die;;
let sod = function
    Some v -> v
  | None -> raise Die;;

type connection = {
  trans : Transport.t ;
  proto : Thrift.Protocol.t;
  bb : BlackBox.client ;
}

(* The ~ denotes a keyword argument *)
let connect ~host port =
  let tx = new TSocket.t host port in
  let proto = new TBinaryProtocol.t tx in
  let bb = new BlackBox.client proto proto in
    tx#opn;
    { trans = tx ; proto = proto; bb = bb}
;;

let dofl () =
  let cli = connect ~host:"127.0.0.1" 9090 in
  try
    Printf.printf "sending a notification\n%!"; 
    let notif = new notification in
      notif#set_notificationType "test";
      notif#set_values (Hashtbl.create 1);
      cli.bb#notifyMe notif;
    
    Printf.printf "notification sent\n%!"; 

    Printf.printf "querying\n%!";
    let qry = new query in
      qry#set_relName "testRel";
      qry#set_arguments ["1";"2"]; 
      let qresult = cli.bb#doQuery qry in            
        (*Printf.printf "result=%s\n%!" qresult#get_result;  *)
        Printf.printf "result received\n%!";
    Printf.printf "query done\n%!";

(*
    (let w = new work in
       w#set_op Operation.DIVIDE ;
       w#set_num1 (Int32.of_int 1) ;
       w#set_num2 (Int32.of_int 0) ;
       try
	 let quotient = cli.calc#calculate (Int32.of_int 1) w in
	   Printf.printf "Whoa? We can divide by zero!\n" ; flush stdout
       with InvalidOperation io ->
	 Printf.printf "InvalidOperation: %s\n" io#grab_why ; flush stdout) ;
    (let w = new work in
       w#set_op Operation.SUBTRACT ;
       w#set_num1 (Int32.of_int 15) ;
       w#set_num2 (Int32.of_int 10) ;
       let diff = cli.calc#calculate (Int32.of_int 1) w in
	 Printf.printf "15-10=%ld\n" diff ; flush stdout) ;
    (let ss = cli.calc#getStruct (Int32.of_int 1) in
       Printf.printf "Check log: %s\n" ss#grab_value ; flush stdout) ;
    cli.trans#close

*)

  with Transport.E (_,what) ->
    Printf.printf "ERROR: %s\n" what ; flush stdout

;;

(* todo: receive notifications. this was adapted from client tutorial, so 
    it presently just sends notifs and queries. *)

(* todo: REGISTRATION? *)
dofl();;
