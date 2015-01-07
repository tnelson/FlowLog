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
open Printf
open Unix
open ExtList.List

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

let string_of_list (sep: string) (pro: 'a -> string) (alist: 'a list): string =
  (String.concat sep (List.map pro alist));;

(* The ~ denotes a keyword argument *)
let connect ~host port =
  let tx = new TSocket.t host port in
  let proto = new TBinaryProtocol.t tx in
  let fl = new FlowLogInterpreter.client proto proto in
    tx#opn;
    { trans = tx ; proto = proto; fl = fl};;

let send_notif notif =
    let cli = connect ~host:"127.0.0.1" 9090 in
    try
      cli.fl#notifyMe notif;
      cli.trans#close;
    with Transport.E (_,what) ->
      Printf.printf "ERROR: %s\n" what ; flush Pervasives.stdout

class bb_handler =
object (self)
  inherit BlackBox.iface

  val counter: int ref = ref 0;

  val timer_thread_ids: (string * Thread.t) list ref = ref [];

  method private get_optional_str values key =
    match Hashtbl.mem values key with
      | true -> Hashtbl.find values key
      | false -> ""

  method private get_optional_int values key =
    match Hashtbl.mem values key with
      | true -> Hashtbl.find values key
      | false -> "0"

  method private create_timer_thread timer_id timer_id2 seconds milliseconds =
       let new_thread_id = (Thread.create (fun x ->
                              Printf.printf "Starting timer for id=%s/%s. seconds=%d. milliseconds=%d\n%!" timer_id timer_id2 seconds milliseconds;
                             (* Unix.sleep seconds; (* Need millisecond granularity instead *) *)
                              Thread.delay (float_of_int seconds +. (float_of_int milliseconds /. 1000.0));
                              let reply = new notification in
                              let tbl = (Hashtbl.create 2) in
                              reply#set_notificationType "timer_expired";
                              Hashtbl.add tbl "id" timer_id;
                              Hashtbl.add tbl "id2" timer_id2;
                              reply#set_values tbl;
                              send_notif reply;
                              Printf.printf "Sent timer for id=%s/%s.\n%!" timer_id timer_id2) 0) in
         timer_thread_ids := (timer_id,new_thread_id)::!timer_thread_ids

  method private reset_timer values =
    let timer_id = (Hashtbl.find values "id") in
    let timer_id2 = (self#get_optional_str values "id2") in (* kludge *)
    let seconds = int_of_string (Hashtbl.find values "seconds") in
    let milliseconds = int_of_string (self#get_optional_int values "ms") in

      (* note that id2 is NOT used as an index here! Really we need 2 different events, one that
         preserves id2 (because it's not an identifier, it's a tag!) and one that treats id2 as a
         part of the timer's identifier. TODO. - TN *)

      if mem_assoc timer_id !timer_thread_ids then
      begin
        Thread.kill (assoc timer_id !timer_thread_ids);
        self#create_timer_thread timer_id timer_id2 seconds milliseconds
      end
      else
      begin
        self#create_timer_thread timer_id timer_id2 seconds milliseconds
      end

  method private start_timer values =
    (* case-sensitive. But Flowlog's parser downcases everything. So fields are always lowercase.*)
    let timer_id = (Hashtbl.find values "id") in

    (* OPTIONAL FIELD: id2. *)
    (* TODO: somewhat kludgey. what if we need id3? etc. etc. *)
    let timer_id2 = (self#get_optional_str values "id2") in
    let seconds = int_of_string (Hashtbl.find values "seconds") in
    let milliseconds = int_of_string (self#get_optional_int values "ms") in
       ignore (Thread.create (fun x ->
                              Printf.printf "Starting timer for id=%s/%s. seconds=%d. milliseconds=%d\n%!" timer_id timer_id2 seconds milliseconds;
                             (* Unix.sleep seconds; (* Need millisecond granularity instead *) *)
                              Thread.delay (float_of_int seconds +. (float_of_int milliseconds /. 1000.0));
                              let reply = new notification in
                              let tbl = (Hashtbl.create 2) in
                              reply#set_notificationType "timer_expired";
                              Hashtbl.add tbl "id" timer_id;
                              Hashtbl.add tbl "id2" timer_id2;
                              reply#set_values tbl;
                              send_notif reply;
                              Printf.printf "Sent timer for id=%s/%s.\n%!" timer_id timer_id2) 0)

  method private set_alarm values =
    (* case-sensitive. But Flowlog's parser downcases everything. So fields are always lowercase.*)
    let timer_id = (Hashtbl.find values "id") in
    let sec = int_of_string (Hashtbl.find values "sec") in
    let min = int_of_string (Hashtbl.find values "min") in
    let hr24 = int_of_string (Hashtbl.find values "hr24") in
    let t = Unix.time() in
    let localtime = Unix.localtime t in
    let alarmtime, _ = Unix.mktime {tm_sec=sec;tm_min=min;tm_hour=hr24;
                                    tm_mday=localtime.tm_mday;tm_mon=localtime.tm_mon;tm_year=localtime.tm_year;
                                    tm_wday=localtime.tm_wday;tm_yday=localtime.tm_yday;tm_isdst=localtime.tm_isdst} in
    let delay = int_of_float (alarmtime -. t) in
       ignore (Thread.create (fun x ->
                              Printf.printf "Starting alarm for id=%s. %d:%d:%d. Delay = %d\n%!" timer_id hr24 min sec delay;
                              Unix.sleep delay;
                              let reply = new notification in
                              let tbl = (Hashtbl.create 4) in
                              reply#set_notificationType "alarm_expired";
                              Hashtbl.add tbl "id" timer_id;
                              Hashtbl.add tbl "sec" (Hashtbl.find values "sec");
                              Hashtbl.add tbl "min" (Hashtbl.find values "min");
                              Hashtbl.add tbl "hr24" (Hashtbl.find values "hr24");
                              reply#set_values tbl;
                              send_notif reply;
                              Printf.printf "Sent alarm expiration for id=%s.\n%!" timer_id) 0)

  method notifyMe notif =
    let ntype = sod ((sod notif)#get_notificationType) in
    let values = sod ((sod notif)#get_values) in
      Printf.printf "received notification. type=%s\n%!" ntype;
      Hashtbl.iter (fun k v -> printf "key: %s, value: %s\n%!" k v) values;
      try
      (match ntype with
        | "start_timer" -> self#start_timer values
        | "set_alarm" -> self#set_alarm values
        | "reset_timer" -> self#reset_timer values
        | _ ->
        let reply = new notification in
        let tbl = (Hashtbl.create 2) in
          reply#set_notificationType "exception";
          Hashtbl.add tbl "sender" "Timer";
          Hashtbl.add tbl "message" "Unknown notification type.";
          reply#set_values tbl;
          send_notif reply;
          Printf.printf "Sent exception.\n%!")

      with Not_found ->
          printf "...but did not contain well-formed fields.\n%!"


  (********************************************************************************)
  method private get_time rep args =
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
          rep#set_exception_message "Timer.time expects a single argument.";
          rep#set_result [];
        end
        else if (List.hd args) = (String.capitalize (List.hd args)) then
        begin
          rep#set_result [[thetime]]
        end
        else if (List.hd args) = thetime then
        begin
          (* for constant, only return a tuple of it's equal to the current time. *)
          rep#set_result [[thetime]]
        end;

        printf "sending result: {%s}\n%!" (string_of_list ";" (fun tup -> (string_of_list "," (fun x->x) tup)) (sod (rep#get_result)));
        rep

  method private get_clock rep args =
    Printf.printf "handling clock-time query\n%!";

      let thetime = Unix.time() in
      let localtime = Unix.localtime thetime in
      if (List.length args) != 2 then
      begin
        rep#set_exception_code "1";
        rep#set_exception_message "Timer.clock expects two arguments: (time-element, value) e.g. (hr24, X).";
        rep#set_result [];
        rep
      end
      else
      begin
        let telement = (List.hd args) in
        let tvalueterm = (List.hd (List.tl args)) in
        let tvalue = (match telement with
          | "hr24" -> string_of_int localtime.tm_hour
          | "hr12" -> string_of_int (localtime.tm_hour mod 12)
          | "min" -> string_of_int localtime.tm_min
          | "sec" -> string_of_int localtime.tm_sec
          | "mday" -> string_of_int localtime.tm_mday
          | "mon" -> string_of_int localtime.tm_mon
          | "year" -> string_of_int localtime.tm_year
          | "wday" -> string_of_int localtime.tm_wday
          | "yday" -> string_of_int localtime.tm_yday
          | "isdst" -> string_of_bool localtime.tm_isdst
          | _ ->
            rep#set_exception_code "1";
            rep#set_exception_message "Unknown time element name. Should be, e.g., hr24.";
            rep#set_result [];
            "") in

        if tvalueterm = (String.capitalize tvalueterm) then
        begin
          rep#set_result [[telement;tvalue]]
        end
        else if tvalueterm = tvalue then
        begin
          (* for constant, only return a tuple of it's equal to the current time. *)
          rep#set_result [[telement;tvalue]]
        end;

        printf "sending result: {%s}\n%!" (string_of_list ";" (fun tup -> (string_of_list "," (fun x->x) tup)) (sod (rep#get_result)));
        rep
      end

  method private get_nonce rep args =
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
          rep#set_result [[nonce]]
        end
        else if (List.hd args) = nonce then
        begin
          (* for constant *)
          rep#set_result [[nonce]]
        end;
        rep

  (********************************************************************************)
  method doQuery qry =
    let relname = (sod (sod qry)#get_relName) in
    let args = (sod (sod qry)#get_arguments) in

    let rep = new queryReply in
    match relname with
      | "time" -> self#get_time rep args
      | "nonce" -> self#get_nonce rep args
      | "clock" -> self#get_clock rep args
      | _ ->
        Printf.printf "invalid query relation %s\n%!" relname;
        rep#set_exception_code "2";
        rep#set_exception_message "Timer only supports a single query relation: 'time'.";
        rep#set_result [];
        rep
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

