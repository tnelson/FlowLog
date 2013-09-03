(*
  Modified from ocaml tutorial by Tim

  This is the FlowLog server for catching notifications,
    sending queries, and sending notifications.
*)

(* note to self: all open does is avoid the "module." 
   not quite like #include or require. *)
open Arg
open Thrift
open Flowlog_rpc_types
open Thread
open Flowlog_Types
open ExtList.List
open Flowlog_Helpers
open Partial_Eval
open Printf

(* "Die, Bart, Die!" is German for "The, Bart, The!". -- Sideshow Bob *)
exception Die;;
let sod = function
    Some v -> v
  | None -> raise Die;;

let lowercase_notif_values tbl = 
  Hashtbl.iter (fun k v ->                 
                  (* newbie ocaml note: <> is structural inequality, != is physical *)
                  if (String.lowercase k) <> k then 
                  begin
                    Hashtbl.add tbl (String.lowercase k) v;
                    Hashtbl.remove tbl k;
                    Printf.printf "Replacing %s key with lowercase: %s -> %s\n%!" k (String.lowercase k) v;
                  end;
                  Printf.printf "%s -> %s\n%!" k v) tbl;
                   
class fl_handler (a_program : flowlog_program)  =
object (self)
  inherit FlowLogInterpreter.iface
  
  (* Always created within a program context *)
  val the_program : flowlog_program = a_program;
    
  method notifyMe rpc_notif : unit = 
    let ntypestr = sod ((sod rpc_notif)#get_notificationType) in
    let values = sod ((sod rpc_notif)#get_values) in
    Printf.printf "received notification. type=%s\n%!" ntypestr;
    lowercase_notif_values values;  (* case insensitive field names *)
    
    try           
      let fieldlist = map String.lowercase (get_fields_for_type the_program ntypestr) in          
        (* construct a list of terms from the hashtbl in values. use the type as an index *)      
           
      let vals = fold_left (fun acc fld -> 
           if (not (Hashtbl.mem values fld)) then
               raise (Failure ("Field "^fld^" was not included in an incoming event of type: "^ntypestr))
           else
             (fld, Hashtbl.find values fld) :: acc)
         [] fieldlist in
        let ev: event = {typeid = ntypestr; values = construct_map vals} in
          respond_to_notification the_program ev None;      

    with Failure(msg) ->  Printf.printf "   *** ERROR! Ignoring notification for reason: %s\n%!" msg;     

end

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
    { trans = tx ; proto = proto; bb = bb};;

let start_listening (a_program : flowlog_program) : unit =
  let h = new fl_handler a_program in
  let proc = new FlowLogInterpreter.processor h in
  let port = 9090 in (* FL listen on 9090 *)
  let pf = new TBinaryProtocol.factory in
  let server = new TThreadedServer.t
		 proc
		 (new TServerSocket.t port)
		 (new Transport.factory)
		 pf
		 pf
  in
    (* first thing: listen for notifications *)
    (* returns handle of thread. ignore result to avoid warning *) 
    ignore (Thread.create (fun x -> (server#serve)) 0);
    printf "Started to listen for notifications.\n%!";;



