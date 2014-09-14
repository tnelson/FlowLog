open Printf;;
open NetCore_Types;;

(*
  Provide a safe queue for passing packets to emit between Thrift's (OCaml) Thread and
  NetCore's (LWT) threads. Use a pipe to avoid polling.

  Also need to safe push new policies.
*)

class cl_SafeEmitQueue =
 object
      (* Docs: "The first component of the result is opened for reading, that's the exit to the pipe.
      	        The second component is opened for writing, that's the entrance to the pipe." *)
      val emit_fds = Lwt_unix.pipe_in (); (* write: unix; read: lwt_unix*)
      val policy_fds = Lwt_unix.pipe_in (); (* write: unix; read: lwt_unix*)
      val mutable to_emit = [];
      val mutable to_push_policy = None;
      val queue_mutex = Mutex.create ();

      method enqueue_emit (emit: (NetCore_Types.switchId * NetCore_Types.portId * OpenFlow0x01_Core.payload) option): unit =
      printf "enqueue_emit in current thread ID: %d\n%!" (Thread.id (Thread.self ()));
        let _,wr = emit_fds in
          Mutex.lock queue_mutex;
          to_emit <- emit::to_emit;
          Mutex.unlock queue_mutex;
          ignore (Unix.write wr "0" 0 1);
          printf "enqueued...";
          (); (* write one byte to the pipe, containing zero *)

      method dequeue_emit (): (NetCore_Types.switchId * NetCore_Types.portId * OpenFlow0x01_Core.payload) option list Lwt.t =
        let rd,_ = emit_fds in
        Lwt_unix.set_blocking rd true;
        lwt bl = (Lwt_unix.blocking rd) in
          printf "dequeue_emit in current thread ID: %d. blocking for rd: %b\n%!" (Thread.id (Thread.self ())) bl;
          let buff = String.create 2 in
            (* Threat: not checking to see if the write and read go through correctly *)
            (* Imperative to use Lwt_unix here! If not, will block NetCore's entire set of LWTs *)
            lwt read_result = Lwt_unix.read rd buff 0 1 in
            printf "dequeueing...";
            Mutex.lock queue_mutex;
            let result = to_emit in
              to_emit <- [];
              Mutex.unlock queue_mutex;
              Lwt.return result;

      (* enqueuing a policy is done within the XSB mutex in RTN *)
      method enqueue_pol (policy: pol): unit =
      printf "enqueue_pol in current thread ID: %d\n%!" (Thread.id (Thread.self ()));
        let _,wr = policy_fds in
          Mutex.lock queue_mutex;
          (match to_push_policy with
            | Some x -> printf "OVERWRITING PREVIOUS POLICY THAT HAS NOT BEEN READ\n%!"
            | None -> printf "No policy to overwrite\n%!");
          to_push_policy <- Some policy;
          Mutex.unlock queue_mutex;
          ignore (Unix.write wr "0" 0 1);
          printf "enqueued...";
          (); (* write one byte to the pipe, containing zero *)

      method dequeue_pol (): pol option Lwt.t =
        let rd,_ = policy_fds in
        Lwt_unix.set_blocking rd true;
        lwt bl = (Lwt_unix.blocking rd) in
          printf "dequeue_pol in current thread ID: %d. blocking for rd: %b\n%!" (Thread.id (Thread.self ())) bl;
          let buff = String.create 2 in
            (* Threat: not checking to see if the write and read go through correctly *)
            (* Imperative to use Lwt_unix here! If not, will block NetCore's entire set of LWTs *)
            lwt read_result = Lwt_unix.read rd buff 0 1 in
            printf "dequeueing...";
            Mutex.lock queue_mutex;
            let result = to_push_policy in
              to_push_policy <- None;
              Mutex.unlock queue_mutex;
              Lwt.return result;

    end;;
