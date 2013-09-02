open Surface_Parser
open Surface_Lexer
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

open Xsb_Communication

(* Use ExtList.List instead -- provides filter_map, but also tail-recursive combine *)
(*open List;;*)
open ExtList.List

(* Thanks to Jon Harrop on caml-list *)
let from_case_insensitive_channel ic =
  let aux buf n =
    let i = input ic buf 0 n in
    for i=0 to i-1 do
      buf.[i] <- Char.lowercase buf.[i]
    done;
    i in
  Lexing.from_function aux

let read_ast (filename : string) : flowlog_ast = 
    printf "Trying to open %s\n%!" filename;
    let lexbuf = from_case_insensitive_channel (open_in filename) in
    try 
      let result = Surface_Parser.main Surface_Lexer.token lexbuf in 
        printf "Done parsing.\n%!";
        pretty_print_program result;
        result
    with exn -> 
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
      print_endline ("File " ^ filename ^ " has an error on line " ^ 
                    (string_of_int line) ^ " column " ^ (string_of_int cnum) ^
                    " at the token " ^ tok);
      raise exn;;

(*let rec parse_imports_helper (filenames : string list) (already_parsed : Types.program list) (already_seen : string list) : Types.program list =
    match filenames with
    | [] -> already_parsed;
    | h :: t -> if List.mem h already_seen then raise (Failure ("circular imports: " ^ (Type_Helpers.list_to_string (fun x -> x) (h :: already_seen)))) else
    let first = read_program h in
        match first with Types.Program(_, imports, _, _, _) ->
        parse_imports_helper ((List.map (fun str -> str ^ ".flg") imports) @ t) (first :: already_parsed) (h :: already_seen);;

let build_finished_program (filename : string) : Types.program = 
    let prgm = read_program filename in
    match prgm with Types.Program(_, imports, _, _, _) ->
    Parse_Helpers.process_program_types (Parse_Helpers.import prgm (parse_imports_helper (List.map (fun str -> str ^ ".flg") imports) [] [filename]));;
*)

(**************************************************************************)

let build_clause (r: srule) (in_atom: formula) (relname: string) (terms: term list) (prefix: string) (conj: formula): clause =
    let head = FAtom("", prefix^"_"^relname, terms) in
    let body = FAnd(in_atom, conj) in
    {orig_rule = r; head = head; body = body};;

let clauses_of_rule (r: srule): clause list =
    match r with Rule(increlname, incterm, act) ->    
    let atom_for_on = FAtom("", increlname, [TVar(incterm)]) in (* local atom, no module name *)
    match act with 
        | ADelete(relname, terms, condition) -> 
            map (build_clause r atom_for_on relname terms minus_prefix) (disj_to_list (disj_to_top condition));
        | AInsert(relname, terms, condition) -> 
            map (build_clause r atom_for_on relname terms plus_prefix) (disj_to_list (disj_to_top condition));
        | ADo(relname, terms, condition) -> 
            map (build_clause r atom_for_on relname terms do_prefix) (disj_to_list (disj_to_top condition));;     

let desugared_program_of_ast (ast: flowlog_ast): flowlog_program =
    printf "*** REMINDER: IMPORTS NOT YET HANDLED! (Remember to handle in partial eval, too.) ***\n%!"; (* TODO *)
    match ast with AST(imports, stmts) ->
        (* requires extlib *)
        let the_decls  =  built_in_decls @ 
                          filter_map (function SDecl(d) -> Some d     | _ -> None) stmts in 
        let the_reacts =  built_in_reacts @ 
                          filter_map (function SReactive(r) -> Some r | _ -> None) stmts in 
        let the_rules  =  filter_map (function SRule(r) -> Some r     | _ -> None) stmts in 
        
            let clauses = (fold_left (fun acc r -> (clauses_of_rule r) @ acc) [] the_rules) in 
                {decls = the_decls; reacts = the_reacts; clauses = clauses};;

(* usage message *)
let usage = Printf.sprintf "Usage: %s [-alloy] file.flg" (Filename.basename Sys.argv.(0));;
let alloy = ref false;;
let args = ref [];;
let speclist = [
  ("-alloy", Arg.Unit (fun () -> alloy := true), ": convert to Alloy");];;

let simplify_clause (cl: clause): clause =   
    {head = cl.head; orig_rule = cl.orig_rule; body = minimize_variables cl.body};;

let simplify_clauses (p: flowlog_program) =
  let newclauses = map simplify_clause p.clauses in
    {decls = p.decls; reacts = p.reacts; clauses = newclauses};;


let listenPort = ref 6633;;

let event_with_assn (p: flowlog_program) (ev : event) (assn: assignment): event =
  {ev with values=(StringMap.add assn.afield assn.avalue ev.values)};;


let forward_packet (context: (switchId * port * Packet.packet) option) (ev: event): unit =
  printf "forwarding: %s\n%!" (string_of_event ev);
  ();;

let emit_packet (ev: event): unit =
  printf "emitting: %s\n%!" (string_of_event ev);
  ();;

let send_event (ev: event) (ip: string) (pt: string): unit =
  printf "sending: %s\n%!" (string_of_event ev);
  ();;


let execute_output (p: flowlog_program) (context: (switchId * port * Packet.packet) option) (defn: sreactive): unit =
  match defn with 
    | ReactOut(relname, arglist, outtype, assigns, spec) ->
     
      let execute_tuple (tup: string list): unit =
        (* arglist orders the xsb results. assigns says how to use them, spec how to send them. *)
        let initev = (match spec with 
                  | OutForward | OutEmit -> {typeid = "packet"; values=StringMap.empty}      
                  | OutLoopback -> failwith "loopback unsupported currently"
                  | OutSend(ip, pt) -> {typeid=outtype; values=StringMap.empty}) in
        let ev = fold_left (event_with_assn p) initev assigns in
          match spec with 
            | OutForward -> forward_packet context ev
            | OutEmit -> emit_packet ev
            | OutLoopback -> failwith "loopback unsupported currently"
            | OutSend(ip, pt) -> send_event ev ip pt in

      (* query xsb for this output relation *)  
      let xsb_results = Communication.get_state (FAtom("", relname, map (fun s -> TVar(s)) arglist)) in        
        (* execute the results *)
        iter execute_tuple xsb_results 
    | _ -> failwith "execute_output";;

(* XSB query on plus or minus for table *)
let change_table_how (p: flowlog_program) (toadd: bool) (tbldecl: sdecl): formula list =
  match tbldecl with
    | DeclTable(relname, argtypes) -> 
      let modrelname = if toadd then (plus_prefix^"_"^relname) else (minus_prefix^"_"^relname) in
      let varlist = init (length argtypes) (fun i -> TVar("X"^string_of_int i)) in
      let xsb_results = Communication.get_state (FAtom("", modrelname, varlist)) in
      map (fun strtup -> FAtom("", relname, map (fun sval -> TConst(sval)) strtup)) xsb_results
    | _ -> failwith "change_table_how";;


(* separate to own module once works for sw/pt *)
let respond_to_notification (p: flowlog_program) (notif: event) (context: (switchId * port * Packet.packet) option): unit =
  (* populate the EDB with event *)  
  Communication.assert_event p notif;

  (* Update remote state if needed*)
  (* TODO *)

  (* for all declared outgoing events ...*)
  let outgoing_defns = get_output_defns p in
    iter (execute_output p context) outgoing_defns;

  (* for all declared tables +/- *)
  let table_decls = get_local_tables p in
  let to_assert = flatten (map (change_table_how p true) table_decls) in
  let to_retract = flatten (map (change_table_how p false) table_decls) in
  printf "  *** WILL ADD: %s\n%!" (String.concat " ; " (map string_of_formula to_assert));
  printf "  *** WILL DELETE: %s\n%!" (String.concat " ; " (map string_of_formula to_retract));
  (* update state as dictated by +/- *)
  iter Communication.assert_formula to_assert;
  iter Communication.retract_formula to_retract;

  Xsb.debug_print_listings();

  (* depopulate event EDB *)
  Communication.retract_event p notif;
  ();;


let switch_connected (p: flowlog_program) (sw : switchId) (feats : OpenFlow0x01.SwitchFeatures.t) : unit =
  Printf.printf "Switch %Ld connected.\n%!" sw;
  let port_nums = map (fun (x : PortDescription.t)-> x.PortDescription.port_no) feats.SwitchFeatures.ports in
  let sw_string = Int64.to_string sw in  
  let notifs = map (fun portid -> {typeid="switch_port"; 
                                   values=construct_map [("sw", sw_string); ("pt", (string_of_int portid))] }) port_nums in
  printf "SWITCH REGISTERED! %s\n%!" (String.concat ", " (map string_of_event notifs));
  List.iter (fun notif -> respond_to_notification p notif None) notifs;;

(* infinitely recursive function that listens for switch connection messages 
   Cribbed nearly verbatim from Ox lib by Tim on Aug 29 2013
   since we're moving from Ox to Frenetic as a base *)
let rec handle_switch_reg (p: flowlog_program) = 
    let open Message in
    let open FlowMod in
    lwt feats = OpenFlow0x01_Platform.accept_switch () in 
    let sw = feats.SwitchFeatures.switch_id in 
    (*lwt _ = Log.info_f "switch %Ld connected" sw in*)
    lwt _ = OpenFlow0x01_Platform.send_to_switch sw 0l (FlowModMsg delete_all_flows) in
    lwt _ = OpenFlow0x01_Platform.send_to_switch sw 1l BarrierRequest in
    (* JNF: wait for barrier reply? *)
    let _ = switch_connected p sw feats in 
    (*Lwt.async (fun () -> switch_thread sw);*)
    handle_switch_reg p;;


let run_flowlog (p: flowlog_program): unit Lwt.t =  
  (* Start up XSB, etc. *)
  Communication.start_program p;
 
  (* Listen for incoming notifications via RPC *)
  (*Flowlog_Thrift_In.start_listening p;; *)

  (* Send the "startup" notification. Enables initialization, etc. in programs *)
  (*let startup = Types.Constant([], Types.startup_type) in
    Evaluation.respond_to_notification startup Program.program None;; *)

  (* Catch switch-connect events: *)

  (* Start the policy stream *)
  (* >> is from Lwt's Pa_lwt. But you MUST have -syntax camlp4o or it won't be recoginized. *)   
  OpenFlow0x01_Platform.init_with_port !listenPort >>
    let (gen_stream, stream) = make_policy_stream p in
    (* streams for incoming/exiting packets *)
    let (pkt_stream, push_pkt) = Lwt_stream.create () in
      Lwt.pick [gen_stream;
                NetCore_Controller.start_controller pkt_stream stream;
                handle_switch_reg p
               ];;

let main () =
  let collect arg = args := !args @ [arg] in
  let _ = Arg.parse speclist collect usage in
  let filename = try hd !args with exn -> raise (Failure "Input a .flg file name.") in  
  let ast = read_ast filename in
  let program = simplify_clauses (desugared_program_of_ast ast) in    
    printf "-----------\n%!";
    List.iter (fun cl -> printf "%s\n\n%!" (string_of_clause cl)) program.clauses;

    if !alloy then write_as_alloy program (filename^".als")
    else 
      Sys.catch_break true;
      try
        Lwt_main.run (run_flowlog program)
      with exn ->
        Xsb.halt_xsb ();
        Format.printf "Unexpected exception: %s\n%s\n%!"
          (Printexc.to_string exn)
          (Printexc.get_backtrace ());
        exit 1;;
    
 main();;
