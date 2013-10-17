open Flowlog_Types
open Flowlog_Helpers
open Partial_Eval
open Printf
open Arg
open Flowlog_To_Alloy

open NetCore_Types
open Packet
open OpenFlow0x01
open OpenFlow0x01_Platform
open OpenFlow0x01_Core
open Lwt
open NetCore_Pattern
open NetCore_Wildcard
open NetCore_Controller

open Flowlog_Parse_Helpers

open Xsb_Communication

(* Use ExtList.List instead -- provides filter_map, but also tail-recursive combine *)
(*open List;;*)
open ExtList.List

                

(* usage message *)
let usage = Printf.sprintf "Usage: %s 
                             [-alloy] 
                             [-verbose <n>] 
                             [-reportall] 
                             [-notables] 
                             file.flg" (Filename.basename Sys.argv.(0));;
let alloy = ref false;;
let cimp = ref false;;
let notables = ref false;;
let reportall = ref false;;
let args = ref [];;

let speclist = [
  ("-verbose", Arg.Int (fun lvl -> global_verbose := lvl), ": set level of debug output");
  ("-alloy", Arg.Unit (fun () -> alloy := true), ": convert to Alloy");
  ("-cimp", Arg.Unit (fun () -> cimp := true), ": convert two files to Alloy and compare their semantics");
  ("-reportall", Arg.Unit (fun () -> reportall := true), ": report all packets. WARNING: VERY SLOW!");
  (* Not calling this "reactive" because reactive still implies sending table entries. *)
  ("-notables", Arg.Unit (fun () -> notables := true), ": send everything to controller");];;

let listenPort = ref 6633;;

let run_flowlog (p: flowlog_program): unit Lwt.t =  
  (* Start up XSB, etc. *)
  Communication.start_program p !notables;
 
  (* Listen for incoming notifications via RPC *)
  Flowlog_Thrift_In.start_listening p;

  (* Start the policy stream *)
  (* >> is from Lwt's Pa_lwt. But you MUST have -syntax camlp4o or it won't be recoginized. *)   
  OpenFlow0x01_Platform.init_with_port !listenPort >>
    let (trigger_re_policy_func, (gen_stream, stream)) = (make_policy_stream p !notables !reportall) in
    refresh_policy := Some trigger_re_policy_func;

    (* streams for incoming/exiting packets *)
    let (pkt_stream, push_pkt) = Lwt_stream.create () in        
    emit_push := Some push_pkt;

    (* Send the "startup" notification. Enables initialization, etc. in programs *)         
    respond_to_notification p {typeid="startup"; values=StringMap.empty};

      (* pick cancels all threads given if one terminates *)             
      (* DO NOT attempt to copy ox/frenetic's switch connection detection code here. It will clash with 
         Frenetic's. Instead, register a HandleSwitchEvent policy, which gives us a nice clean callback. *)
      Lwt.pick [gen_stream; NetCore_Controller.start_controller pkt_stream stream];;


let main () =
  let collect arg = args := !args @ [arg] in
  let _ = Arg.parse speclist collect usage in
  let filename = try hd !args with exn -> raise (Failure "Input a .flg file name.") in  
  let ast = read_ast filename in
  let program = (desugared_program_of_ast ast) in    
    printf "-----------\n%!";
    (*List.iter (fun cl -> printf "%s\n\n%!" (string_of_clause cl)) program.clauses;*)

    if !alloy then 
      write_as_alloy program (alloy_filename filename)
    else if !cimp then
    begin
      let filename2 = try hd (tl !args) with exn -> raise (Failure "Input a second .flg file name for use with change-impact.") in 
      let ast2 = read_ast filename2 in
      let program2 = (desugared_program_of_ast ast2) in   
        write_as_alloy_change_impact program (alloy_filename filename) 
                                     program2 (alloy_filename filename)
    end
    else
    begin
      (* Intercede when Ctrl-C is pressed to close XSB, etc. *)
      Sys.catch_break true;
      (* If SIGPIPE ("broken pipe") failure (exit code 141), actually give an error. 
         Without this set, the program terminates with no message. *)
      Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
      try      
        out_log := Some(open_out "log_for_flowlog.log");  
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


