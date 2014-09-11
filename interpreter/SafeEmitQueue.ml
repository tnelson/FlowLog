open Printf;;

(*
  Provide a safe queue for passing packets to emit between Thrift's (OCaml) Thread and
  NetCore's (LWT) threads. Use a pipe to avoid polling.
*)

class cl_SafeEmitQueue =
 object
      (* Docs: "The first component of the result is opened for reading, that's the exit to the pipe.
      	        The second component is opened for writing, that's the entrance to the pipe." *)
      val fds = Lwt_unix.pipe_in (); (* write: unix; read: lwt_unix*)
      val mutable to_emit = [];
      val queue_mutex = Mutex.create ();

      method enqueue (emit: (NetCore_Types.switchId * NetCore_Types.portId * OpenFlow0x01_Core.payload) option): unit =
      printf "enqueue in current thread ID: %d\n%!" (Thread.id (Thread.self ()));
        let _,wr = fds in
          Mutex.lock queue_mutex;
          to_emit <- emit::to_emit;
          Mutex.unlock queue_mutex;
          ignore (Unix.write wr "0" 0 1);
          printf "enqueued...";
          (); (* write one byte to the pipe, containing zero *)

      method dequeue (): (NetCore_Types.switchId * NetCore_Types.portId * OpenFlow0x01_Core.payload) option list Lwt.t =
        let rd,_ = fds in
        Lwt_unix.set_blocking rd true;
        lwt bl = (Lwt_unix.blocking rd) in
          printf "dequeue in current thread ID: %d. blocking for rd: %b\n%!" (Thread.id (Thread.self ())) bl;
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

    end;;
