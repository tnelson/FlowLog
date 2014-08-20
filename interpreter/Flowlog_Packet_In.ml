(****************************************************************)
(* Testing listening for packet_in on alternative ports         *)
(****************************************************************)

open NetCore_Types
open Printf
open OpenFlow0x01
open OpenFlow0x01_Core
open Message
open Lwt_unix

module Socket = Socket

let otherListenPort = ref 9999;;

(* number of simultaneous connections *)
let max_pending : int = 64;;

let listener_fd : Lwt_unix.file_descr option ref = ref None;;

(***********************************************************************************)

(* Can't re-use OpenFlow module's init_with_port since that saves the server's *)
(* file-descriptor, and can only be initialized once. Instead, redo: *)

let init_with_fd (fd : Lwt_unix.file_descr) : unit Lwt.t =
  match !listener_fd with
  | Some _ ->
    raise_lwt (Invalid_argument "Platform already initialized")
  | None ->
    listener_fd := Some fd;
    Lwt.return ();;

let init_with_port (port : int) : unit Lwt.t =
  let open Lwt_unix in
  printf "initing on port %d\n%!" port;
  let fd = socket PF_INET SOCK_STREAM 0 in
    setsockopt fd SO_REUSEADDR true;
    bind fd (ADDR_INET (Unix.inet_addr_any, port));
    listen fd max_pending;
    init_with_fd fd;;

let string_of_lwtfd (fd: Lwt_unix.file_descr): string =
  let ufd = unix_file_descr fd in
    let st = Unix.fstat ufd in 
      (* Many other fields here *)
      sprintf "dev: %d, inode: %d" st.st_dev st.st_ino;;


let get_fd () : Lwt_unix.file_descr Lwt.t =
  match !listener_fd with
    | Some fd ->    
      Lwt.return fd
    | None ->
      raise_lwt (Invalid_argument "Platform not initialized");;

type msg = Message.t;;

let print_fd_status () =
  lwt fd = get_fd () in
  printf "Listener FD Stats: %s\n%!" (string_of_lwtfd fd);      
  match state fd with
    | Opened -> printf "opened\n%!"; Lwt.return();
    | Closed -> printf "closed\n%!"; Lwt.return();
    | Aborted exn -> printf "aborted\n%!"; Lwt.return();;



(***********************************************************************************)

let string_of_sockaddr (sa: sockaddr): string =
  match sa with
    | ADDR_UNIX str -> str
    | ADDR_INET(addr, pt) -> (Unix.string_of_inet_addr addr)^":"^(string_of_int pt);;

let rec listen_for_packet_ins_thread (servicefd: Lwt_unix.file_descr): unit Lwt.t = 
  match_lwt (OpenFlow0x01_Switch.recv_from_switch_fd servicefd) with
    | Some (xid, msg) ->
      printf "received from descriptor: \n%!";
      printf "  xid: %d\n%!" (Int32.to_int xid);
      printf "  msg: %s\n%!" (Message.to_string msg);
      (match msg with
        | PacketInMsg(pin) ->
          printf "  pkt_in: %s\n%!" (packetIn_to_string pin); 
          listen_for_packet_ins_thread servicefd
        | _ -> listen_for_packet_ins_thread servicefd)
    | None ->
      printf "recv_from_switch_fd returned None.\n%!";
      Lwt.return ();;

let rec terminate_pin_connection_thread (servicefd: Lwt_unix.file_descr): unit Lwt.t = 
  printf "Closing connection fd...\n%!";
  Lwt_unix.close servicefd;;

let rec send_hello_reply (servicefd: Lwt_unix.file_descr) (sa: sockaddr): unit Lwt.t =  
  let hello_message = Hello(Cstruct.create 0) in      
    match_lwt OpenFlow0x01_Switch.send_to_switch_fd servicefd (Int32.of_int 0) hello_message with
      | true -> Lwt.return ()
      | false -> failwith "wait_for_hello";; 

let rec send_features_request (servicefd: Lwt_unix.file_descr) (sa: sockaddr): unit Lwt.t =  
  let features_message = SwitchFeaturesRequest in      
    match_lwt OpenFlow0x01_Switch.send_to_switch_fd servicefd (Int32.of_int 0) features_message with
      | true -> Lwt.return ()          
      | false -> failwith "wait_for_features_reply";;
  
let rec wait_for_features_reply (servicefd: Lwt_unix.file_descr) (sa: sockaddr): int64 option Lwt.t =  
  match_lwt (OpenFlow0x01_Switch.recv_from_switch_fd servicefd) with
    | Some (_, SwitchFeaturesReply(features)) ->    
      Lwt.return (Some features.switch_id)
    | Some(_,_) -> 
      wait_for_features_reply servicefd sa;
    | None -> 
      printf "recv_from_switch_fd returned None.\n%!";
      Lwt.return None;;  

let rec wait_for_hello (servicefd: Lwt_unix.file_descr): unit Lwt.t =
  (* Expect hello -> send hello -> send features request -> expect features reply *) 
  match_lwt (OpenFlow0x01_Switch.recv_from_switch_fd servicefd) with
    | Some (_, Hello(_)) ->    
      Lwt.return();
    | Some(_,_) -> 
      wait_for_hello servicefd
    | None -> 
      printf "recv_from_switch_fd returned None.\n%!";
      Lwt.return ();;  

let rec listen_for_connections (): unit Lwt.t =
  (* use lwt instead of let to resolve "thread returning 'a" vs. "'a" *)
  printf "listening for connections. \n%!";
  print_fd_status ();

  lwt fd = get_fd() in
  lwt (servicefd, sa) = Lwt_unix.accept fd in
  printf "accepted from %s\n%!" (string_of_sockaddr sa);  
  printf "Service FD Stats: %s\n%!" (string_of_lwtfd servicefd);      
  
  wait_for_hello servicefd;
  send_hello_reply servicefd sa;
  send_features_request servicefd sa;
  lwt swid = (match_lwt wait_for_features_reply servicefd sa with 
    | None -> failwith "features reply returned None"
    | Some(id) -> Lwt.return id) in

  printf "Discovered switch with dpid=%Ld\n%!" swid;
  
  Lwt.async (fun () -> listen_for_packet_ins_thread servicefd >> terminate_pin_connection_thread servicefd);
  (* continue listening *)
  listen_for_connections();;

(***********************************************************************************)

let test_start (): unit =
  let shutdown_listeners_thread () = 
    lwt fd = get_fd() in 
      printf "shutting down listener FD\n%!";
      Lwt_unix.close fd in

  (* Taken from frenetic/Flowlog startup *)
  let listen_for_packets_thread () =
    init_with_port !otherListenPort >>
    (* real version will have multiple picks, like flowlog.ml has *)
    Lwt.pick [listen_for_connections ()] >> 
    shutdown_listeners_thread () in

    Sys.catch_break true;
    try      
      Lwt_main.run (listen_for_packets_thread ());
    with exn ->
      Printf.eprintf "unhandled exception: %s\n%s\n%!"
        (Printexc.to_string exn)
        (Printexc.get_backtrace ());
      (* TN: No explicit shutdown of active connections? *)
      Lwt_main.run(shutdown_listeners_thread ());
      exit 1;;

test_start()

(* Should have close or shutdown explicitly above *)