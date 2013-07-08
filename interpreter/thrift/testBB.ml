(*
  Modified from ocaml tutorial by Tim

  This is a Blackbox. 
*)

open Arg
open Thrift
open Flowlog_types
open Thread

(* "Die, Bart, Die!" is German for "The, Bart, The!". -- Sideshow Bob *)
exception Die;;
let sod = function
    Some v -> v
  | None -> raise Die;;

type connection = {
  trans : Transport.t ;
  proto : Thrift.Protocol.t;
  fl : FlowLogInterpreter.client ;
}

(* The ~ denotes a keyword argument *)
let connect ~host port =
  let tx = new TSocket.t host port in
  let proto = new TBinaryProtocol.t tx in
  let fl = new FlowLogInterpreter.client proto proto in
    tx#opn;
    { trans = tx ; proto = proto; fl = fl}
;;

class bb_handler =
object (self)
  inherit BlackBox.iface

  method notifyMe notif = 
    Printf.printf "notified()\n%!"

  method doQuery qry =
    Printf.printf"handlequery\n%!";
    (* need to return a QueryReply *)
    let rep = new queryReply in
      let tbl = (Hashtbl.create 1)  in
      Hashtbl.add tbl ["1"; "2"; "3"] true;
      rep#set_result tbl;
      rep
end

let dobb () =
  let h = new bb_handler in
  let proc = new BlackBox.processor h in
  let port = 9091 in (* BB listen on 9091 *)
  let pf = new TBinaryProtocol.factory in
  let server = new TThreadedServer.t
		 proc
		 (new TServerSocket.t port)
		 (new Transport.factory)
		 pf
		 pf
  in
    (* Listen in a separate thread. *)
    Thread.create (fun x -> (server#serve)) 0;

    (* do whatever here *)
    (* in this case: test FL's ability to receive notifications *)
  let cli = connect ~host:"127.0.0.1" 9090 in 
  try
    Printf.printf "sending a notification\n%!"; 
    let notif = new notification in
      notif#set_notificationType "test";
      notif#set_values (Hashtbl.create 1);
      cli.fl#notifyMe notif;    
    Printf.printf "notification sent\n%!"; 
    
    (* close the connection *)
    cli.trans#close;

  with Transport.E (_,what) ->
    Printf.printf "ERROR: %s\n" what ; flush stdout
;;

(* todo: SEND notifications---how? *)

dobb();;

(* Tests done, but wait in case new messages from FL. *)
Unix.sleep 1000
