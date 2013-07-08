(*
  Modified from ocaml tutorial by Tim

  This is a Blackbox. 
*)

open Arg
open Thrift
open Flowlog_types

(* "Die, Bart, Die!" is German for "The, Bart, The!". -- Sideshow Bob *)
exception Die;;
let sod = function
    Some v -> v
  | None -> raise Die;;

class bb_handler =
object (self)
  inherit BlackBox.iface

  method notifyMe notif = 
    Printf.printf "notified()\n%!"

  method doQuery qry =
    Printf.printf"handlequery\n%!";
    (* need to return a QueryReply *)
    let rep = new queryReply in
      rep#set_result (Hashtbl.create 1);
      rep
end

let dobb () =
  let h = new bb_handler in
  let proc = new BlackBox.processor h in
  let port = 9090 in
  let pf = new TBinaryProtocol.factory in
  let server = new TThreadedServer.t
		 proc
		 (new TServerSocket.t port)
		 (new Transport.factory)
		 pf
		 pf
  in
    server#serve
;;

(* todo: SEND notifications---how? *)

dobb();;
