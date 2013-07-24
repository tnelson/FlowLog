(*
  Modified from ocaml tutorial by Tim

  Timer black-box. 
  - Receives notifications to set timers;
  - Sends timer notifications when timers expire;
  - Responds to requests for the current time. 
*)

(* TODO: Michael points out that we ought to have a Black-box functor, to aid in abstraction and avoid code-reuse.
    Much of this is boilerplate... *)

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

  val counter: int ref = ref 0;

  method notifyMe notif = 
    let ntype = sod ((sod notif)#get_notificationType) in
    let values = sod ((sod notif)#get_values) in
      Printf.printf "received notification. type=%s\n%!" ntype;
      (* <>, not != *)
      if ntype <> "start_timer" then
      begin
        let reply = new notification in
        let tbl = (Hashtbl.create 2) in
        (* TODO: abstract this out *) 
          reply#set_notificationType "exception";          
          Hashtbl.add tbl "sender" "Timer";
          Hashtbl.add tbl "message" "Timer supports only start_timer notifications.";
          reply#set_values tbl;
          send_notif reply;
          Printf.printf "Sent exception.\n%!";
      end
      else
      begin
        (* TODO: need to check for valid fields *)
        (* note: needs to be upcase or made case-insensitive *)
        let timer_id = (Hashtbl.find values "ID") in
        let seconds = int_of_string (Hashtbl.find values "SECONDS") in
        ignore (Thread.create (fun x ->                                  
                                  Printf.printf "Starting timer for id=%s. seconds=%d.\n%!" timer_id seconds;
                                  Unix.sleep seconds;
                                  let reply = new notification in
                                  let tbl = (Hashtbl.create 2) in
                                  reply#set_notificationType "timer_expired";                                            
                                  Hashtbl.add tbl "id" timer_id;
                                  reply#set_values tbl;
                                  send_notif reply;
                                  Printf.printf "Sent timer for id=%s.\n%!" timer_id) 0);
      end
  
  
  method doQuery qry =
    let relname = (sod (sod qry)#get_relName) in
    let args = (sod (sod qry)#get_arguments) in

    let rep = new queryReply in
    let tbl = (Hashtbl.create 1) in
    if relname = "time" then
    begin
      Printf.printf "handling time query\n%!";
      (* need to return a QueryReply. s/b only one argument. if it's a variable,
         return the value. if it's a constant, compare. e.g. if time=10,
         time(X) should return {[10]}. But time(3) should return {}. time(10) would return {[10]}.
         In this BB, time(10) is supremely unlikely to ever be called, but doing the check anyway. *)          
      (* Can't use Sys.time because that's proc. seconds used by THIS process.
         Instead, use Unix.time(), which is seconds since epoch. *)
      let thetime = string_of_int(int_of_float(Unix.time())) in
        if (List.length args) != 1 then
        begin
          rep#set_exception_code "1";
          rep#set_exception_message "Timer.time expects a single argument."
        end 
        else if (List.hd args) = (String.capitalize (List.hd args)) then
        begin
          Hashtbl.add tbl [thetime] true
        end
        else if (List.hd args) = thetime then
        begin
          (* for constant, only return a tuple of it's equal to the current time. *)
          Hashtbl.add tbl [thetime] true 
        end;

        rep#set_result tbl;
        rep
    end
    else if relname = "nonce" then
    begin
      Printf.printf "handling nonce query\n%!";
      (* this nonce is not secure. 
         it's also sequential...
         it's not even guaranteed unique. 
         it's also generated adhoc---ocaml has no gensym? *)
      let nonce = string_of_int !counter in 
        counter := (!counter) + 1;

        Printf.printf "Nonce was: %d\n%!" !counter;

        if (List.length args) != 1 then
        begin
          rep#set_exception_code "1";
          rep#set_exception_message "Timer.nonce expects a single argument."
        end 
        else if (List.hd args) = (String.capitalize (List.hd args)) then
        begin
          Hashtbl.add tbl [nonce] true
        end
        else if (List.hd args) = nonce then
        begin
          (* for constant *)
          Hashtbl.add tbl [nonce] true 
        end;

        rep#set_result tbl;
        rep

    end
    else 
    begin
      Printf.printf "invalid query relation %s\n%!" relname;
      rep#set_exception_code "2";
      rep#set_exception_message "Timer only supports a single query relation: 'time'.";
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
    Printf.printf "Starting listener (in main thread; this should block)...\n%!";
    server#serve;
;;

let timer_expire id  = 
    Printf.printf "timer expired. sending notification\n%!"; 
    let notif = new notification in
      notif#set_notificationType "timer";
      let tbl = (Hashtbl.create 1) in
        Hashtbl.add tbl "id" id;
        notif#set_values tbl;
        send_notif notif;
        Printf.printf "notification sent\n%!";; 


dobb();;


