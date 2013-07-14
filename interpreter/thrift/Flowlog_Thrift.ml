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
open Type_Helpers.Type_Helpers;;
open Evaluation;;

(* "Die, Bart, Die!" is German for "The, Bart, The!". -- Sideshow Bob *)
exception Die;;
let sod = function
    Some v -> v
  | None -> raise Die;;

let print_notif_values tbl = 
  Hashtbl.iter (fun k v -> 
                Printf.printf "%s -> %s\n%!" k v) tbl 

let get_ntype_from_list ntypename thelist =
  let filtered = (List.filter (fun ntype -> match ntype with Type(aname, _) -> ntypename = aname)
                              thelist) in
  if (List.length filtered) = 0 then
    raise (Failure "Unknown notif type");
  List.hd filtered
    

class fl_handler (a_program : program) (the_notif_types : notif_type list) =
object (self)
  inherit FlowLogInterpreter.iface
  
  (* Always created within a program context, with some set of notif types *)
  val the_program : program = a_program;
  val notif_types : notif_type list = the_notif_types;

  method notifyMe notif = 
    let ntypestr = sod ((sod notif)#get_notificationType) in
    let values = sod ((sod notif)#get_values) in
    Printf.printf "received notification. type=%s\n%!" ntypestr;
    print_notif_values values;
    let ntype = get_ntype_from_list ntypestr notif_types in
    match ntype with
      Type(_, fieldnames) ->        
      (* construct a list of terms from the hashtbl in values. use the type as an index *)
      let theterms = (List.map (fun fieldname -> (Constant (Hashtbl.find values fieldname)))
                               fieldnames) in     
    Evaluation.respond_to_notification (terms_to_notif_val ntype theterms) the_program;

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

let start_listening (a_program : program) (the_notif_types : notif_type list) : unit =
  let h = new fl_handler in
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

(* dobbquery: atom * blackbox -> string list list
    The resulting list contains all the tuples (as string lists) returned.  

    TODO: should the parameter be a bbatom? (We need to have already substituted in
    any concrete values of the trigger notification, if any.)

    Note: each query opens a separate, new connection to the black-box.
*)
let doBBquery bbdecl bbatom = 
  match bbdecl with
    Internal_BB(_) -> raise (Failure "dobbquery passed internal BB.")
    | External_BB(_, bbip, bbport) -> 
      let cli = connect ~host:bbip bbport in 
      try

      match bbatom with
        Equals(_,_) -> Printf.printf "ERROR: passed equals atom to dobbquery.\n%!";
                       raise (Failure "ERROR: passed equals atom to dobbquery.")
      | Apply(_,_) -> Printf.printf "ERROR: passed apply atom to dobbquery.\n%!";
                       raise (Failure "ERROR: passed apply atom to dobbquery.")
      | Bool(_) -> Printf.printf "ERROR: passed boolean atom to dobbquery.\n%!";
                       raise (Failure "ERROR: passed boolean atom to dobbquery.")
      | Query(_, bbrel, tlist) -> 
        Printf.printf "querying...\n%!";
        let qry = new query in
        qry#set_relName bbrel;
        qry#set_arguments (List.map Flowlog_Types.Type_Helpers.term_to_string tlist); 
        let qresult = cli.bb#doQuery qry in          
          let result = Hashtbl.fold (fun k v sofar -> k :: sofar)                     
                                    (sod qresult#get_result)
                                    [] in
            cli.trans#close;
            result

      with Transport.E (_,what) ->
        Printf.printf "ERROR sending query: %s\n%!" what;
        raise (Failure what);
;;

(* dobbnotify: blackbox * notif_val  -> unit
     Sends a notification to blackbox.

   Each notification opens a separate, new connection to the black-box.
*)

let doBBnotify bbdecl nvalue =
  match bbdecl with
    Internal_BB(_) -> raise (Failure "dobbnotify passed internal BB.")
    | External_BB(_, bbip, bbport) -> 
      match nvalue with
        Notif_val(ntype, termlist) -> 
          match ntype with
          Type(typename, fieldnames) ->
	    if (List.length fieldnames) != (List.length termlist) then
              raise (Failure "dobbnotify called with notif value and notif type that had different arities!");

            let cli = connect ~host:bbip bbport in 
            try

              Printf.printf "sending a notification\n%!"; 
              let notif = new notification in
              let tbl = (Hashtbl.create (List.length termlist)) in
              notif#set_notificationType typename;
              notif#set_values tbl;

              (* need to lock-step iter over the field names and the params *)
              List.iter2 (fun ele1 ele2 -> 
                              Hashtbl.add tbl ele1 ele2)
                         fieldnames (List.map term_to_string termlist);
              cli.bb#notifyMe notif;      
              cli.trans#close

            with Transport.E (_,what) ->
              Printf.printf "ERROR sending notification: %s\n%!" what;
              raise (Failure what);
;;
  

