(*
  Service listens for Flowlog events and prints them out in stdout.
*)

open Arg
open Thrift
open Flowlog_rpc_types
open Thread
open Printf
open Unix

let listen_port = 20000;;

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
    { trans = tx ; proto = proto; fl = fl};;

let send_notif notif =
    let cli = connect ~host:"127.0.0.1" 9090 in
    try
      cli.fl#notifyMe notif;
      cli.trans#close;
    with Transport.E (_,what) ->
      Printf.printf "ERROR: %s\n" what ; flush Pervasives.stdout

class bb_handler =
object (self)
  inherit BlackBox.iface

  method notifyMe notif =
    let ntype = sod ((sod notif)#get_notificationType) in
    let values = sod ((sod notif)#get_values) in
      Printf.printf "received notification. type=%s\n%!" ntype;
      Hashtbl.iter (fun k v -> printf "%s => %s\n%!" k v) values

  (********************************************************************************)
  method doQuery qry =
    let relname = (sod (sod qry)#get_relName) in
    let rep = new queryReply in
      Printf.printf "invalid query relation %s\n%!" relname;
      rep#set_exception_code "2";
      rep#set_exception_message "No query support in placeholder listener.";
      rep#set_result [];
      rep
end

let dobb () =
  let h = new bb_handler in
  let proc = new BlackBox.processor h in
  let port = listen_port in
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
    Printf.printf "Starting listener (in main thread; this should block)...\n%!";
    server#serve;
;;

dobb();;

