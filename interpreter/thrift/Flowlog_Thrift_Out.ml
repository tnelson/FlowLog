

open Thrift
open Flowlog_rpc_types
open Flowlog_Types
open Flowlog_Helpers
open ExtList.List
open Printf

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

(* "Die, Bart, Die!" is German for "The, Bart, The!". -- Sideshow Bob *)
exception Die;;
let sod = function
    Some v -> v
  | None -> raise Die;;

let print_notif_values tbl =
  Hashtbl.iter (fun k v ->
                printf "%s -> %s\n%!" k v) tbl


(*  The resulting list contains all the tuples (as string lists) returned.

    Note: each query opens a separate, new connection to the black-box.
*)
let doBBquery (qryname: string) (bbip: string) (bbport: string) (args: term list) (types: typeid list): string list list =
    let dotted_host = Packet.string_of_ip (nwaddr_of_int_string bbip) in
    printf "Sending BB query to %s:%s...\n%!" dotted_host bbport;
    let cli = connect ~host:dotted_host (int_of_string bbport) in
    try
      printf "Connected.\n%!";
      let qry = new query in
      qry#set_relName qryname;

      let arguments = (map2 (fun t typid -> (pretty_print_value typid (string_of_term t))) args types) in
      printf "Args: %s\n%!" (string_of_list "," identity arguments);
      qry#set_arguments arguments;

      let qresult = cli.bb#doQuery qry in
      printf "doQuery called.\n%!";
        let result = (sod qresult#get_result) in
        let xsb_result = map (fun tup -> map flvalue_to_xsbable tup) result in
        cli.trans#close;
        xsb_result

      with Transport.E (_,what) ->
        printf "ERROR sending query: %s\n%!" what;
        raise (Failure what);;

(*
  Sends a notification to blackbox.
  Each notification opens a separate, new, connection to the black-box.
*)

let doBBnotify (ev: event) (bbip: string) (bbport: string) (evfields: (string * typeid) list): unit =
            let dotted_host = Packet.string_of_ip (nwaddr_of_int_string bbip) in
            let cli = connect ~host:dotted_host (int_of_string bbport) in
            try
              printf "Sending notification to %s:%s...\n%!" dotted_host bbport;
              let notif = new notification in
              let tbl = (Hashtbl.create (StringMap.cardinal ev.values)) in
              notif#set_notificationType ev.typeid;
              notif#set_values tbl;
              StringMap.iter (fun k v -> Hashtbl.add tbl k (pretty_print_value (assoc k evfields) v)) ev.values;
              Hashtbl.iter (fun k v -> printf "    %s -> %s\n%!" k v) tbl;
              printf "Making RPC invocation...\n%!";
              cli.bb#notifyMe notif;
              printf "RPC invocation complete. Closing socket...\n%!";
              cli.trans#close;
              printf "RPC invocation complete. Socket closed.\n%!";
            with | Transport.E (_,what) ->
                     printf "ERROR sending notification: %s\n%!" what;
                     raise (Failure what)
                 | _ -> printf "Unknown problem sending event.\n%!";

