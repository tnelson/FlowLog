(*
  This Blackbox sends a notification (specified in cmd line args) and then exits.
*)

(*#load "str.cma"*)

open Arg
open Thrift
open Flowlog_rpc_types
open Str

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

if (Array.length Sys.argv) < 2 then 
begin
  Printf.printf "notify <notif type name> [argument-name=value] ...\n%!";
  exit(1)
end;;

let notifname = Sys.argv.(1);;
let numargs = (Array.length Sys.argv) - 2;;
Printf.printf "Notif type: %s\nNumber args: %d\n%!" notifname numargs;;

let tbl = (Hashtbl.create numargs);;

(* silly naive newbie parsing. replace with better soon. so bad! *)
Array.iter (fun str -> match (split (regexp "=") str) with
                       | [k;v] -> Printf.printf "Arg: %s -> %s\n%!" k v;
                                  Hashtbl.add tbl k v;
                       | _ -> failwith "bad defn")
           (Array.sub Sys.argv 2 ((Array.length Sys.argv) - 2));;
      

let dobb () =
  let cli = connect ~host:"127.0.0.1" 9090 in 
  try
    Printf.printf "sending notification...\n%!"; 
    let notif = new notification in
      notif#set_notificationType notifname;
      notif#set_values tbl;
      cli.fl#notifyMe notif;
      cli.trans#close;
  with Transport.E (_,what) ->
    Printf.printf "ERROR: %s\n" what ; flush stdout
;;

dobb();;

