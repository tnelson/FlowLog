(****************************************************************)
(* Testing listening for packet_in on alternative ports         *)
(****************************************************************)

open NetCore_Types
open Printf
open OpenFlow0x01
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

(* ^^^ no accept call??? **)

let get_fd () : Lwt_unix.file_descr Lwt.t =
  match !listener_fd with
    | Some fd ->
      Lwt.return fd
    | None ->
      raise_lwt (Invalid_argument "Platform not initialized");;

type msg = Message.t;;

let recv_from_descriptor (servicefd: Lwt_unix.file_descr): 'a Lwt.t =
  match_lwt (OpenFlow0x01_Switch.recv_from_switch_fd servicefd) with
    | Some pr ->
      Lwt.return pr
    | None ->
      failwith "recv_from_switch_fd returned None";;


let print_fd_status () =
  lwt fd = get_fd () in
  match state fd with
    | Opened -> printf "opened\n%!"; Lwt.return();
    | Closed -> printf "closed\n%!"; Lwt.return();
    | Aborted exn -> printf "aborted\n%!"; Lwt.return();;



(***********************************************************************************)

(* Lwt.return ();;*)

let string_of_sockaddr (sa: sockaddr): string =
  match sa with
    | ADDR_UNIX str -> str
    | ADDR_INET(addr, pt) -> (Unix.string_of_inet_addr addr)^":"^(string_of_int pt);;

let rec test_listener (): unit Lwt.t =
  (* use lwt instead of let to resolve "thread returning 'a" vs. "'a" *)
  printf "in test_listener. \n%!";
  print_fd_status ();

  lwt fd = get_fd() in
  lwt (servicefd, sa) = Lwt_unix.accept fd in
  printf "accepted from %s\n%!" (string_of_sockaddr sa);

  lwt (xid, msg) = recv_from_descriptor servicefd in
    printf "received\n%!";
    printf "  xid: %d\n%!" (Int32.to_int xid);
    printf "  msg: %s\n%!" (Message.to_string msg);

    test_listener();;

  (*lwt _ = match msg with
    | PacketInMsg pktIn ->
      handle_packet_in xid pktIn;
      test_listener ();
    | _ -> failwith "unsupported OpenFlow message";;*)

(***********************************************************************************)

let test_start (): unit =
  (* Taken from frenetic/Flowlog startup *)
  let listen_for_packets () =
    init_with_port !otherListenPort >>
      (* real version will have multiple picks, like flowlog.ml has *)
      Lwt.pick [test_listener ()] in

    Sys.catch_break true;
    try
      Lwt_main.run (listen_for_packets ())
    with exn ->
      Printf.eprintf "unhandled exception: %s\n%s\n%!"
        (Printexc.to_string exn)
        (Printexc.get_backtrace ());
      exit 1;;

test_start()