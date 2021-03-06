(****************************************************************)
(* Main flowlog executable                                      *)
(****************************************************************)

open Flowlog_Types
open Flowlog_Helpers
open Partial_Eval
open Printf
open Arg
open Flowlog_To_Alloy
open Lwt
open Flowlog_Parse_Helpers
open Xsb_Communication
open Flowlog_Builtins

(* Provides filter_map, but also tail-recursive combine *)
open ExtList.List

(* usage message *)
let usage = Printf.sprintf "Usage: %s
                             [-alloy]
                             [-cimp]
                             [-cimpreach]
                             [-depend]
                             [-core <spec file>]
                             [-verbose <n>]
                             [-reportall]
                             [-notables]
                             file.flg" (Filename.basename Sys.argv.(0));;
let alloy = ref false;;
let cimp = ref false;;
let cimpreach = ref false;;
let notables = ref false;;
let reportall = ref false;;
let depend = ref false;;
let core = ref "";;
let args = ref [];;

let speclist = [
  ("-verbose", Arg.Int (fun lvl -> global_verbose := lvl), ": set level of debug output");
  ("-alloy", Arg.Unit (fun () -> alloy := true), ": convert (single file) to Alloy");
  ("-cimp", Arg.Unit (fun () -> cimp := true), ": convert multiple files to Alloy and compare their semantics");
  ("-cimpreach", Arg.Unit (fun () -> cimpreach := true), ": change impact via Alloy, plus bounded reachability check");
  ("-depend", Arg.Unit (fun () -> depend := true), ": [EXPERIMENTAL] output a JSON dependency graph");
  ("-reportall", Arg.Unit (fun () -> reportall := true), ": report all packets. WARNING: VERY SLOW!");
  (* Not calling this "reactive" because reactive still implies sending table entries. *)
  ("-notables", Arg.Unit (fun () -> notables := true), ": send everything to controller");
  ("-unsafe", Arg.Unit (fun () -> global_unsafe := true), ": allow switches to outpace controller");
  ("-core", Arg.String (fun fn -> core := fn), ": Spark Core specification file name");
  ];;

let listenPort = ref 6633;;
let listenCPPort = ref 9999;;

let run_flowlog (p: flowlog_program): unit Lwt.t =
  (* Start up XSB, etc. *)
  let additional_xsb = (fold_left (fun acc (n, dec) -> match dec.bip_prepare with
      | Some(strlst) -> strlst@acc
      | _ -> acc)
    [] builtin_predicates) in
  Communication.start_program p !notables additional_xsb;

  (* Listen for incoming notifications via RPC *)
  Flowlog_Thrift_In.start_listening p;

  (try_lwt

  (* Start the policy stream *)
  (* >> is from Lwt's Pa_lwt. But you MUST have -syntax camlp4o or it won't be recoginized. *)
  OpenFlow0x01_Platform.init_with_port !listenPort >>
    let (trigger_re_policy_func, (gen_stream, stream), push_pol) = (make_policy_stream p !notables !reportall) in

    (* streams for incoming/exiting packets *)
    let (pkt_stream, push_pkt) = Lwt_stream.create () in
    emit_push := Some push_pkt;

    (* See Partial_Eval and SafeEmitQueue *)
    let rec dequeue_packets_for_emit (): unit Lwt.t =
      (* Block until something is dequeued *)
      lwt to_emit = safe_queue#dequeue_emit () in (* "lwt" needed here or the CREATION OF THE THREAD will block. *)
        iter push_pkt to_emit;
        printf "In dequeue_packets_for_emit. to_emit length = %d\n%!" (length to_emit);
        dequeue_packets_for_emit () in

      (* Note use of LWT here! *)
      (* Send the "startup" notification. Enables initialization, etc. in programs *)
      Lwt.return (respond_to_notification p {typeid="startup"; values=StringMap.empty} IncThrift) >>
      Lwt.return (respond_to_notification p {typeid="corestartup"; values=StringMap.empty} IncThrift) >>

      (* pick cancels all threads given if one terminates *)
      (* DO NOT attempt to copy ox/frenetic's switch connection detection code here. It will clash with
         Frenetic's. Instead, register a HandleSwitchEvent policy, which gives us a nice clean callback. *)
      Lwt.pick [gen_stream; NetCore_Controller.start_controller pkt_stream stream; dequeue_packets_for_emit (); (*dequeue_new_policy (); *) ]

   with | Failure(x) ->
     printf "Received failure; exiting LWT.\n%!";
     printf "Failure was %s\n%!" x;
     Lwt.return ());;

let process_core_if_any program =
  match !core with
    | "" -> program
    | _ -> {program with statements=program.statements} (* TODO(Brett): replace this with parsing !core and producing new AST *)
;;

let main () =
  let collect arg = args := !args @ [arg] in
  let _ = Arg.parse speclist collect usage in
  let filename = try hd !args with exn -> raise (Failure "Input a .flg file name.") in

  if (!global_unsafe && (!notables || !cimpreach || !cimp || !alloy || !depend)) then
  begin
    printf "Invalid combination of -unsafe and other flags.\n%!";
    exit(0);
  end;

  if !depend then
  begin
    Flowlog_Graphs.output_single_dependency_graph filename;
    printf "Output dependency graph with .json extension.\n%!";
    exit(0);
  end;

  out_log := Some(open_out "log_for_flowlog.log");

  printf "Loading %s\n%!" filename;
  let ast = read_ast filename in
    (* Add core spec to program *)
    let enhanced_ast = process_core_if_any ast in
    let program = (desugared_program_of_ast enhanced_ast filename) in
    printf "-----------\n%!";

    (**********************************)
    if !alloy then
    begin
      write_as_alloy program (alloy_filename filename) None
    end
    (**********************************)
    else if !cimp || !cimpreach then
    begin
      if (length !args) < 2 then
        raise (Failure "Input a second .flg file name for use with change-impact.");

      let other_filenames = (tl !args) in
      let other_asts = (map (fun fn ->
            (desugared_program_of_ast (read_ast fn) fn, (alloy_filename fn)))
          other_filenames) in
        write_as_alloy_change_impact ((program,(alloy_filename filename))::other_asts)
                                     !cimpreach
    end
    (**********************************)
    (* ACTUALLY RUN THE PROGRAM HERE *)
    else
    begin
      (* Intercede when Ctrl-C is pressed to close XSB, etc. *)
      Sys.catch_break true;
      (* If SIGPIPE ("broken pipe") failure (exit code 141), actually give an error.
         Without this set, the program terminates with no message. *)
      Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
      try
        if !notables then printf "\n*** FLOW TABLE COMPILATION DISABLED! ***\n%!";
        Lwt_main.at_exit (fun () -> return (printf "LWT exiting~\n%!") );
        at_exit (fun () -> (printf "Ocaml exiting~\n%!"));

        Lwt_main.run (run_flowlog program);

        printf "LWT Terminated!\n%!";
      with
        | Sys.Break ->
          Xsb.halt_xsb();
          close_log();
          printf "\nExiting gracefully due to break signal.\n%!";
          exit 101
        | exn ->
        Xsb.halt_xsb ();
        Format.printf "\nUnexpected exception: %s\n%s\n%!"
          (Printexc.to_string exn)
          (Printexc.get_backtrace ());
        close_log();
        exit 100;
    end;;

 main();;


