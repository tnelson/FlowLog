(*
  Modified from ocaml tutorial by Tim

  This is the FlowLog server for catching notifications,
    sending queries, and sending notifications.
*)

(* note to self: all open does is avoid the "module." 
   not quite like #include or require. *)
open Arg;;
open Thrift;;
open Flowlog_rpc_types;;
open Thread;;
open Flowlog_Types.Types;;
open Type_Helpers;;
open Evaluation;;


(* "Die, Bart, Die!" is German for "The, Bart, The!". -- Sideshow Bob *)
exception Die;;
let sod = function
    Some v -> v
  | None -> raise Die;;

let print_notif_values tbl = 
  Hashtbl.iter (fun k v -> 
                Printf.printf "%s -> %s\n%!" k v) tbl 

let get_ntype_from_list (ntypename : string) (thelist : notif_type list) : notif_type =
  let filtered = (List.filter (fun ntype -> (Type_Helpers.notif_type_to_string ntype) = ntypename)
                              thelist) in  
  if (List.length filtered) = 0 then     
    raise (Failure "Unknown notif type")  
  else
    List.hd filtered


module Flowlog_Thrift_In = struct
   
class fl_handler (a_program : program) (the_notif_types : notif_type list) =
object (self)
  inherit FlowLogInterpreter.iface
  
  (* Always created within a program context, with some set of notif types *)
  val the_program : program = a_program;
  val notif_types : notif_type list = the_notif_types;

  method notifyMe notif : unit = 
    let ntypestr = sod ((sod notif)#get_notificationType) in
    let values = sod ((sod notif)#get_values) in
    Printf.printf "received notification. type=%s\n%!" ntypestr;
    print_notif_values values;   
    try           
      let ntype = get_ntype_from_list ntypestr notif_types in    
      match ntype with
        Type(_, fieldnames) ->        
        (* construct a list of terms from the hashtbl in values. use the type as an index *)      
        let theterms = (List.map (fun fieldname -> (Constant (Hashtbl.find values fieldname)))
                                 fieldnames) in             
          Evaluation.respond_to_notification (Type_Helpers.terms_to_notif_val ntype theterms) the_program;      
    with Failure(_) ->  Printf.printf "Unknown notification type. IGNORING! Type was: %s\n%!" ntypestr; 

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
    { trans = tx ; proto = proto; bb = bb}
;;

let start_listening (a_program : program) : unit =
  match a_program with
    Program(_, the_notif_types,_) -> 
  let h = new fl_handler a_program the_notif_types in
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

  Printf.printf "Started to listen for notifications.\n%!";;

  
end


