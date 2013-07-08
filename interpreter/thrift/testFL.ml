(*
  Modified from ocaml tutorial by Tim

  This is the FlowLog server for catching notifications,
    sending queries, and sending notifications.
*)

(* note to self: all open does is avoid the "module." 
   not quite like #include or require. *)
open Arg
open Thrift
open Flowlog_types
open Thread

(* "Die, Bart, Die!" is German for "The, Bart, The!". -- Sideshow Bob *)
exception Die;;
let sod = function
    Some v -> v
  | None -> raise Die;;


class fl_handler =
object (self)
  inherit FlowLogInterpreter.iface

  method notifyMe notif = 
    Printf.printf "received notification\n%!"
end

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
  let h = new fl_handler in
  let proc = new FlowLogInterpreter.processor h in
  let port = 9090 in (* FL listen on 9090 *)
  let pf = new TBinaryProtocol.factory in
  let server = new TThreadedServer.t
		 proc
		 (new TServerSocket.t port)
		 (new Transport.factory)
		 pf
		 pf
  in
    (* first thing: listen for notifications *)
    (Thread.create (fun x -> (server#serve)) 0);

  Printf.printf "Started to listen for notifications. Waiting 5 seconds before sending test queries.\n%!";
  Unix.sleep 10;
  Printf.printf "Beginning to test sending to BB code...";

  (* Now do other stuff: test sending notifications/queries to BB *)
  (* recall BB listens on 9091 *)
  let cli = connect ~host:"127.0.0.1" 9091 in 
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
        (* currying is fun. unfortunately the types are wrong here! *)          
        (* Hashtbl.iter (Printf.printf "result=%s\n%!") qresult#get_result; *)
        Hashtbl.iter (fun k v -> 
                       (Printf.printf "result contained: %s -> %s\n%!" 
                                   (String.concat " " k) 
                                   (string_of_bool v)))
                     (sod qresult#get_result);    
    Printf.printf "query done\n%!";

    (* close the connection *)
    cli.trans#close;

  with Transport.E (_,what) ->
    Printf.printf "ERROR: %s\n%!" what; 
;;

(* todo: REGISTRATION? *)
dofl();;

(* Tests done, but wait in case new messages from BB. *)
Unix.sleep 1000
