open Flowlog_Types
open Flowlog_Helpers
open Printf
open ExtList.List
open Xsb_Communication

      (* if the body contains no triggering atom, safety is called "strong safety" *)

      (* Need to know context in order to expand variable names into component fields as needed *)
      let get_safe_terms (prgm: flowlog_program) (cl_for_context: clause option) (fmla: formula): term list =        
        (* only keep positive atomic formulas *)
        let atoms = filter_map (fun (s,a) -> if s = true then Some(a) else None) (get_atoms_with_sign fmla) in
        let eqs = filter_map (fun (s,a) -> if s = true then Some(a) else None) (get_equalities fmla) in
        
        (* Don't count IN as making something safe. new.nwSrc IN 10.0.0.0/8 is "safe" but not SAFE. *)
        (*let ins = filter_map (fun s,a -> if s = true then Some(a) else None) get_ins body in*)

        (* todo concern: what if the atom is a built-in predicate? is that still valid? *)
        let get_immediate_safe_terms_from (f: formula): term list =
          match f with 
            | FAtom(modname, relname, tl) -> 
                (* If this is a field variable, we need to say that every field is safe, not just a variable.
                   If this variable appears in a SameAsOn action pipe like forward(v), need to take into account the type of the trigger. *)

                if is_incoming_table prgm relname then
                begin
                  let on_context = (match cl_for_context with | None -> None | Some cl -> Communication.get_on_context prgm cl) in   
                  let pfields = Communication.decls_expand_fields prgm modname relname on_context 1 (hd tl) in 
                    pfields@tl
                end
                else
                  tl 
            | FEquals(x, TConst(_)) 
            | FEquals(TConst(_), x) -> [x] | _ -> [] in
        let get_equal_deps (eq: formula): (term * term) list =
          match eq with | FEquals(TConst(_), x) | FEquals(x, TConst(_)) -> [] | FEquals(t1, t2) -> [(t1,t2);(t2,t1)] | _ -> [] in

        let immediates = unique (flatten (map get_immediate_safe_terms_from (atoms @ eqs))) in
        let eqsteps = unique (flatten (map get_equal_deps eqs)) in

        let rec gst_helper (proven: term list): term list =
          let new_proven = unique (proven @
                                   (* follow the dependencies discovered via equalities *)
                                   (filter_map (fun (ante,cons) -> if mem ante proven then Some(cons) else None) eqsteps) @
                                   (* Also: If TVar(x) is proven, then so too is TField(x, f) for any f. Check after the fact: *)
                                   (filter_map (fun (ante,cons) ->
                                      (match ante with
                                        | TField(v, f) -> if mem (TVar(v)) proven then Some(cons) else None
                                        | _ -> None)) eqsteps)
                                    ) in
            if (length new_proven) > (length proven) then gst_helper new_proven
            else proven in
        gst_helper immediates;;
