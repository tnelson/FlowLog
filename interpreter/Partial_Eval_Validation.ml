(****************************************************************)
(* Flowlog's core evaluation and compilation code               *)
(****************************************************************)

open Flowlog_Types
open Flowlog_Packets
open Flowlog_Helpers
open ExtList.List
open Printf

exception InvalidINUse of formula;;
exception UsesUncompilableBuiltIns of formula;;
exception IllegalFieldModification of string;;
exception NonOFTableField of string;;
exception NeedsStronglySafeTerm of term list;;
exception IllegalEquality of (term * term);;

let legal_field_to_modify (fname: string): bool =
	mem fname legal_to_modify_packet_fields;;

let compilable_field_to_test (fname: string): bool =
(* (printf "cftt: %s %s %b\n%!" fname (String.concat "," legal_to_match_packet_fields) (mem fname legal_to_match_packet_fields));  *)
  mem fname legal_to_match_packet_fields;;

(* WEAKENING and VALIDATION *)
let rec validate_formula_for_compile (strong_safe_list: term list) (newpkt: string) (f: formula): formula option =

    (* Can't modify or match on fields disallowed by OpenFlow *)
  let check_legal_pkt_fields = function
							| TField(varname, fld)
                (* newpkt: must be legal to modify *)
                when varname = newpkt ->
      				   			if not (legal_field_to_modify fld) then
                        raise (IllegalFieldModification fld)
              | TField(varname, fld) ->
                 (* any term: cannot compile if involves non-base fieldnames *)
                 if not (compilable_field_to_test fld) then
                    raise (NonOFTableField fld)
      	 			| _ -> ()
      	 				in

  (* A strongly safe term is a constant or in the list *)
  let check_one_strong_safe (tl: term list): unit =
    if for_all (fun t -> match t with | TConst(_) -> false | _ -> not (mem t strong_safe_list)) tl then
      raise (NeedsStronglySafeTerm(tl)) in

  let check_new_strong_safe (t: term): unit =
    match t with | TField(v, f) when v = newpkt -> check_one_strong_safe [t] | _ -> () in

  (* To be folded over. Gather errors and new literals. If no errors, then not weakened at all. *)
  let rec validate_literal (innot: bool) (accf, acce: formula list * exn list) (lit: formula): (formula list) * exn list =
	  (match lit with
		| FTrue -> (lit::accf, acce)
    | FFalse -> (lit::accf, acce)
    | FNot(_) when innot -> failwith ("validate_literal had not-within-not: "^(string_of_formula lit))
    | FNot(f) ->
      let inacclits,inaccexns = validate_literal (not innot) (accf, acce) f in
      if (length acce) < (length inaccexns) then
        (accf, inaccexns)
      else (* don't forget to negate the literal produced by subcall *)
        (FNot(hd inacclits)::(tl inacclits), acce)
    | FIn(t,a,m) ->
      (try
        check_legal_pkt_fields t;
        check_one_strong_safe [a];
        check_one_strong_safe [m];
        (* Disallow new packet in range. *)
        (match t with
          | TField(tv, tf) when tv = newpkt -> raise (InvalidINUse lit)
          | _ -> ());
        (lit::accf, acce) (* success *)
      with
        | e -> (accf, e::acce))

    | FEquals(t1, t2) ->
      (try
        (* p.locPt = new.locPt is the sole exception to strong safety requirement *)
        (match t1,t2 with
        | TField(v1,f1), TField(v2,f2) when (v1 = newpkt || v2 = newpkt) && f1 = "locpt" && f2 = "locpt" ->
          ()
        | _ ->
          check_one_strong_safe [t1;t2];
          check_new_strong_safe t1;
          check_new_strong_safe t2);

        (* Legal fields are required regardless *)
    		check_legal_pkt_fields t1;
    		check_legal_pkt_fields t2;

        (lit::accf, acce) (* success *)
      with
        | e -> (accf, e::acce))

    | FAtom(modname, relname, tlargs) ->
      (try
          (* All fields legal for modification? *)
          iter check_legal_pkt_fields tlargs;

          (* All newpkt fields need to be strongly safe *)
          if innot then iter check_new_strong_safe tlargs;

          (* Is this a built-in that can't be compiled? *)
          if Flowlog_Builtins.is_uncompilable_built_in relname then
            raise (UsesUncompilableBuiltIns f);

          (lit::accf, acce) (* success *)
      with
        | e -> (accf, e::acce))
    | _ -> failwith ("validate_literal did not expect:  "^(string_of_formula lit))) in

  (* Validate and weaken (if needed) each literal *)
  let literals = conj_to_list f in
  let (newliterals, errors) = fold_left (validate_literal false) ([], []) literals in

  if (length errors) = 0 then
  begin
    if !global_verbose > 4 then
      printf "clause can be compiled.\n%!";
    None
  end
  else
  begin
    if !global_verbose > 4 then
    begin
      printf "clause CANNOT be compiled. weakened instead. reasons:\n%!";
      iter (fun e -> match e with
            | UsesUncompilableBuiltIns(f) -> if !global_verbose > 0 then printf "UsesUncompilableBuiltIns: %s\n%!" (string_of_formula f);
            | IllegalFieldModification(s) -> if !global_verbose > 0 then printf "IllegalFieldModification: %s\n%!" s;
            | NeedsStronglySafeTerm(tl) -> if !global_verbose > 0 then printf "NeedsStronglySafeTerm: %s\n%!" (String.concat "; " (map string_of_term tl));
            | NonOFTableField(s) -> if !global_verbose > 0 then printf "NonOFTableField: %s\n%!" s;
            | InvalidINUse(f) -> if !global_verbose > 0 then printf "InvalidINUse: %s\n%!" (string_of_formula f);
            | IllegalEquality(t1,t2) -> if !global_verbose > 0 then printf "IllegalEquality: %s = %s\n%!" (string_of_term t1) (string_of_term t2);
            | _ -> failwith "unexpected exception in errors")
          errors;
      printf "\n%!";
    end;
    Some(build_and newliterals)
  end;;

(* Returns processed clause, along with a Boolean that indicates whether the clause is fully-compilable. *)
let validate_and_process_pkt_triggered_clause (cl: clause): (clause * bool) =
	let newpkt = (match cl.head with
		| FAtom("", "forward", [TVar(newpktname)]) -> newpktname
		| _ -> "") in
      let (_, trimmed) = (trim_packet_from_body cl.body) in
      let strong_safe_list = get_safe_terms trimmed in

      if !global_verbose > 4 then
      begin
        printf "\nValidating clause with body (trimmed) = %s\n%!" (string_of_formula trimmed);
        printf "Strong safe list: %s\n%!" (String.concat ", " (map string_of_term strong_safe_list));
      end;

      let final_formula_maybe = validate_formula_for_compile strong_safe_list newpkt trimmed in
      (match final_formula_maybe with
        (* forwarding clause, no weakening needed *)
        | None when newpkt <> "" ->
          if !global_verbose > 4 then
            printf "Forwarding clause, no weakening needed. Fully compilable.\n%!";
          (cl, true)
        (* non-forwarding clause, no weakening needed *)
        | None ->
          if !global_verbose > 4 then
            printf "NON-forwarding clause, no weakening needed (but trimmed).\n%!";
          ({head = cl.head; orig_rule = cl.orig_rule; body = trimmed}, false)
        (* weakened *)
        | Some(final_formula) ->
          if !global_verbose > 4 then
            printf "Weakened (either forwarding or non-forwarding). New body: %s\n%!" (string_of_formula final_formula);
          ({head = cl.head; orig_rule = cl.orig_rule; body = final_formula}, false));;