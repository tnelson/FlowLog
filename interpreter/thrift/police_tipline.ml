(*
  Police tipline:
  Listen for reports that stolen laptops have been seen on the network
*)

(* TODO: Michael points out that we ought to have a Black-box functor, to aid in abstraction and avoid code-reuse.
    Much of this is boilerplate... *)

open Arg
open Thrift
open Flowlog_rpc_types
open Thread

let police_port = 5050;; (* the five oh *)

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

let send_notif notif = 
    let cli = connect ~host:"127.0.0.1" 9090 in 
    try
      cli.fl#notifyMe notif;
      cli.trans#close;
    with Transport.E (_,what) ->
      Printf.printf "ERROR: %s\n" what ; flush stdout


class bb_handler =
object (self)
  inherit BlackBox.iface

  method notifyMe notif = 
    let ntype = sod ((sod notif)#get_notificationType) in
    let values = sod ((sod notif)#get_values) in
      Printf.printf "received notification. type=%s\n%!" ntype;
      (* <>, not != *)
      if ntype <> "stolen_laptop_found" then
      begin
        let reply = new notification in
        let tbl = (Hashtbl.create 2) in
        (* TODO: abstract this out *) 
          reply#set_notificationType "exception";          
          Hashtbl.add tbl "sender" "Police_Tipline";
          Hashtbl.add tbl "message" "The tipline can't accept anything but stolen_laptop_found.";
          reply#set_values tbl;
          send_notif reply;
          Printf.printf "Sent exception.\n%!";
      end
      else
      begin
        (* TODO: need to check for valid fields *)
        (* note: needs to be upcase or made case-insensitive *)
        let mac = (Hashtbl.find values "MAC") in
        let swid = (Hashtbl.find values "SWID") in
        let time = (Hashtbl.find values "TIME") in
          Printf.printf "Our anonymous tipster, who sounds like a Flowlog Controller, saw %s on switch %s at time=%s.\n%!" mac swid time
      end
  

method doQuery qry =
    let relname = (sod (sod qry)#get_relName) in
    (*let args = (sod (sod qry)#get_arguments) in*)
    let rep = new queryReply in
    let tbl = (Hashtbl.create 1) in
    Printf.printf "The _man_ answers to no one's queries! Was: %s\n%!" relname;
    rep#set_exception_code "2";
    rep#set_exception_message "The _man_ answers to no one's queries!";
    rep#set_result tbl;
    rep
    
end

let dobb () =
  let h = new bb_handler in
  let proc = new BlackBox.processor h in
  let port = police_port in
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
    Printf.printf "Starting listener for STOLEN LAPTOP TIPS! (in main thread; this should block)...\n%!";
    server#serve;
;;

dobb();;


