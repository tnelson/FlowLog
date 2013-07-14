(*
  Modified from ocaml tutorial by Tim

  Timer black-box. 
  - Receives notifications to set timers;
  - Sends timer notifications when timers expire;
  - Responds to requests for the current time. 
*)

open Arg
open Thrift
open Flowlog_rpc_types
open Thread

let timer_port = 9091;;

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
    let relname = (sod qry#get_relName()) in
    let args = (sod qry#get_arguments()) in
    if relname = "time" then
    begin
      Printf.printf "handling time() query\n%!";
      (* need to return a QueryReply. s/b only one argument. if it's a variable,
         return the value. if it's a constant, compare. e.g. if time=10,
         time(X) should return {[10]}. But time(3) should return {}. time(10) would return {[10]}.
         In this BB, time(10) is supremely unlikely to ever be called, but doing the check anyway. *)
      let rep = new queryReply in
        let tbl = (Hashtbl.create 1) in
        (* Can't use Sys.time because that's proc. seconds used by THIS process.
           Instead, use Unix.time(), which is seconds since epoch. *)
        let thetime = Unix.time() in
	(* If malformed query, flag an exception. *)

          Hashtbl.add tbl [thetime] true; 
          rep#set_result tbl;
          rep
    end
end

let dobb () =
  let h = new bb_handler in
  let proc = new BlackBox.processor h in
  let port = timer_port in
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
;;

let timer_expire id  = 
    Printf.printf "timer expired. sending notification\n%!"; 
    let cli = connect ~host:"127.0.0.1" 9090 in 
    try

      let notif = new notification in
        notif#set_notificationType "timer";
        let tbl = (Hashtbl.create 1) in
          Hashtbl.add tbl "id" id;
          notif#set_values tbl;
          cli.fl#notifyMe notif;
      cli.trans#close;

    with Transport.E (_,what) ->
      Printf.printf "ERROR: %s\n" what ; flush stdout
    Printf.printf "notification sent\n%!";; 


dobb();;


