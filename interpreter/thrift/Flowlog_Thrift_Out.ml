
(* note to self: all open does is avoid the "module." 
   not quite like #include or require. *)
open Arg;;
open Thrift;;
open Flowlog_rpc_types;;
open Thread;;
open Type_Helpers;;
open Types;;

module Flowlog_Thrift_Out = struct

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
                Printf.printf "%s -> %s\n%!" k v) tbl 

(*let get_ntype_from_list ntypename thelist =
  let filtered = (List.filter (fun ntype -> match ntype with Type(aname, _) -> ntypename = aname)
                              thelist) in
  if (List.length filtered) = 0 then
    raise (Failure "Unknown notif type");
  List.hd filtered
*)

(* dobbquery: atom * blackbox -> string list list
    The resulting list contains all the tuples (as string lists) returned.  

    TODO: should the parameter be a bbatom? (We need to have already substituted in
    any concrete values of the trigger notification, if any.)

    Note: each query opens a separate, new connection to the black-box.
*)
let doBBquery (bbdecl : Types.blackbox) (bbatom : Types.atom) = 
  match bbdecl with
    | Types.BlackBox(_, Types.Internal) -> raise (Failure "dobbquery passed internal BB.")
    | Types.BlackBox(_, Types.External(bbip, bbport)) -> 
      let cli = connect ~host:bbip bbport in 
      try

      match bbatom with
        Types.Equals(_,_,_) -> Printf.printf "ERROR: passed equals atom to dobbquery.\n%!";
                       raise (Failure "ERROR: passed equals atom to dobbquery.")
      | Types.Bool(_) -> Printf.printf "ERROR: passed boolean atom to dobbquery.\n%!";
                       raise (Failure "ERROR: passed boolean atom to dobbquery.")
                (* bbname, bbrel are both strings here. *)
      | (*Types.Apply(sign, bbname, bbrel, tlist) -> *)
        Types.Apply(sign, bbname, tlist) -> 
        Printf.printf "querying...\n%!";
        let bbrel = "dsnmfkdsnfjasndf" in 
        let qry = new query in
        qry#set_relName bbrel;
        qry#set_arguments (List.map Type_Helpers.term_to_string tlist); 
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

let doBBnotify (bbdecl : Types.blackbox) (nvalue : Types.term) =
  match bbdecl with
    Types.BlackBox(_, Types.Internal) -> raise (Failure "dobbnotify passed internal BB.")
    | Types.BlackBox(_, Types.External(bbip, bbport)) -> 
      match nvalue with
        | Types.Variable(_, _) -> raise (Failure "doBBnotify given variable");
        | Types.Field_ref(_, _) -> raise (Failure "doBBnotify given fieldref");
        | Types.Constant(_, Types.Term_defer(_)) -> raise (Failure "doBBnotify given deferred constant");
        (* The notification is a constant term with a notification type. 
           So the "constant term" is a list of raw constants. *)
        | Types.Constant(vallist, Types.Type(typename, fieldnames)) -> 
    	    if (List.length fieldnames) != (List.length vallist) then
              raise (Failure "dobbnotify called with notif value and notif type that had different arities!");

            let cli = connect ~host:bbip bbport in 
            try

              Printf.printf "sending a notification\n%!"; 
              let notif = new notification in
              let tbl = (Hashtbl.create (List.length vallist)) in
              notif#set_notificationType typename;
              notif#set_values tbl;

              (* need to lock-step iter over the field names and the params *)
              List.iter2 (fun ele1 ele2 -> 
                              Hashtbl.add tbl ele1 ele2)
                         fieldnames 
                         vallist;
              cli.bb#notifyMe notif;      
              cli.trans#close

            with Transport.E (_,what) ->
              Printf.printf "ERROR sending notification: %s\n%!" what;
              raise (Failure what);
;;


end
