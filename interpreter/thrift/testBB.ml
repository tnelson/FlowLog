(*
  Modified from ocaml tutorial by Tim

  This is a Blackbox. Because it's a test, it handles both
  dummy notifications and dummy queries. It sends a proper 
  registration notification to Flowlog. It follows that up with
  an Apple TV notification event.
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

(* TODO: safety check. *)
let numArgs = Array.length Sys.argv;;
let arg1 = Sys.argv.(1);;
let arg2 = Sys.argv.(2);;

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
    (* returns handle to new thread. ignore to avoid warning *)
    ignore (Thread.create (fun x -> (server#serve)) 0);

    (* do whatever here *)
    (* in this case: test FL's ability to receive notifications *)
  let cli = connect ~host:"127.0.0.1" 9090 in 
  try

    Printf.printf "sending a BB_register notification\n%!"; 
    let notif = new notification in
      notif#set_notificationType "BB_register";
      let tbl = (Hashtbl.create 3) in
        Hashtbl.add tbl "id" "test";
        Hashtbl.add tbl "ip" "127.0.0.1";
        Hashtbl.add tbl "port" "9091";
        notif#set_values tbl;
        cli.fl#notifyMe notif;
    Printf.printf "notification sent\n%!"; 


    Printf.printf "sending another notification (from cmd line)\n%!"; 
    let notif = new notification in
      notif#set_notificationType "test_fake_appletv";
      let tbl = (Hashtbl.create 2) in
      Hashtbl.add tbl "req_mac" arg1;
      Hashtbl.add tbl "tv_mac" arg2;
      notif#set_values tbl;
      cli.fl#notifyMe notif;    
    Printf.printf "notification sent\n%!"; 
    
    (* close the connection *)
    cli.trans#close;

  with Transport.E (_,what) ->
    Printf.printf "ERROR: %s\n" what ; flush stdout
;;

dobb();;

(* Tests done, but wait in case new messages from FL. *)
Unix.sleep 1000
