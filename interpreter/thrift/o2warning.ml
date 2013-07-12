(*
  This BlackBox sends a single oxygenwarning notification and then exits.
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

(* Optional argument (used by zombiecanary only): number of seconds to stay unconscious. *)
let numArgs = Array.length Sys.argv;;
let arg1 = 
  if numArgs > 1 then
    Sys.argv.(1)
  else 
    "0";;

let dobb () =
  let cli = connect ~host:"127.0.0.1" 9090 in 
  try
    Printf.printf "sending a low oxygen notification...\n%!"; 
    let notif = new notification in
      notif#set_notificationType "low_oxygen";
      let tbl = (Hashtbl.create 3) in
        Hashtbl.add tbl "time" arg1;
        notif#set_values tbl;
        cli.fl#notifyMe notif;
    cli.trans#close;

  with Transport.E (_,what) ->
    Printf.printf "ERROR: %s\n" what ; flush stdout
;;

dobb();;

