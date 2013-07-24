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
open Types;;
open Type_Helpers;;
open Evaluation;;


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
                


module Flowlog_Thrift_In = struct

  (* not to be confused with Types_Helper.notif_type_to_string *)
  let notif_type_to_name (n : Types.term_type ) : string = 
    match n with
      | Types.Type(myname, _) -> myname;
      | Types.Term_defer(defname) -> defname;;

  let get_ntype_from_list (ntypename : string) (notif_types : Types.term_type list) : Types.term_type =
    let filtered = (List.filter (fun ntype -> (String.lowercase (notif_type_to_name ntype)) = 
                                              (String.lowercase ntypename))
                                notif_types) in          
    if (List.length filtered) = 0 then      
      raise (Failure ("Unknown notification type. Type was: "^ntypename^". Known types were: "^
                        (String.concat "; " (List.map notif_type_to_name notif_types)))) 
    else
      List.hd filtered

   
class fl_handler (a_program : Types.program) (the_notif_types : Types.term_type list) =
object (self)
  inherit FlowLogInterpreter.iface
  
  (* Always created within a program context, with some set of notif types *)
  val the_program : Types.program = a_program;
  val notif_types : Types.term_type list = the_notif_types;
    
  method notifyMe notif : unit = 
    let ntypestr = sod ((sod notif)#get_notificationType) in
    let values = sod ((sod notif)#get_values) in
    Printf.printf "received notification. type=%s\n%!" ntypestr;
    lowercase_notif_values values;  (* case insensitive field names *)
    try           
      let ntype = get_ntype_from_list ntypestr notif_types in          
      match ntype with
        | Types.Term_defer(_) -> failwith "notifyMe called for defer. should never get here"
        | Types.Type(_, fieldnames) ->                
        (* construct a list of terms from the hashtbl in values. use the type as an index *)      
        let notif_constant = Types.Constant(
                               (List.map 
                                 (fun fieldname -> 
                                   if (not (Hashtbl.mem values (String.lowercase fieldname))) then
                                     raise (Failure ("Field "^fieldname^" was not included in the notification of type: "^ntypestr));
                                   (Hashtbl.find values (String.lowercase fieldname)))
                                 fieldnames),
                             ntype) in                       
          Evaluation.respond_to_notification notif_constant the_program;      
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
    { trans = tx ; proto = proto; bb = bb}
;;

let start_listening (a_program : Types.program) : unit =
  match a_program with  
    Types.Program(_, _, the_blackboxes, the_term_types,_) -> 
  let h = new fl_handler a_program the_term_types in
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


