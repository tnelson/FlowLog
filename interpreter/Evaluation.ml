open Flowlog_Types;;
open Xsb;;

(* Provides functions for running a Flowlog program.
ASSUMPTIONS: We assume that programs passed into functions in this module have
1) all implied relations defined (i.e. if there's +R or -R then there's R)
2) all relations except NotifRelations have names ending in /(name of program).
3) the name of the program is lower case.
*)
module Evaluation = struct

	let fire_relation (prgm : program) (rel : relation) (notif : notif_val)  : unit =
		match rel with
		| Types.NotifRelation(Types.BlackBox(bbname, ip, port), args, clauses) -> print_endline "fire notif relation";
		| Types.MinusRelation(name, args, clauses) -> print_endline "fire minus relation";
		| Types.PlusRelation(name, args, clauses) -> print_endline "fire plus relation";
		| _ -> raise (Failure "fire_relation called on a helper relation");;

		(*match rel with Relation(name, args, _) -> 
		match args with
		| [] -> raise (Failure "called fire_relation on a relation with empty args.");
		| head :: tail -> match notif with Notif_val(_, terms) ->
		let results = query_relation rel ((List.map (fun t -> Arg_term(t)) terms) @ tail) out_ch in_ch in
		match notif with Notif_val(ntype, _) ->
		if (is_forward_relation prgm rel) then List.map (terms_to_notif_val ntype) results else
		let _ = (match explode name with
		| '+' :: rest -> List.iter (fun terms -> let _ = tentative_assert_clause (Clause(implode rest, (List.map (fun x -> Arg_term(x)) terms), [])) out_ch in_ch in ()) results;
		| '-' :: rest -> List.iter (fun terms -> let _ = retract_clause (Clause(implode rest, (List.map (fun x -> Arg_term(x)) terms), [])) out_ch in_ch in ()) results;
		| _ -> ()) in [];;*)


	let respond_to_notification (notif : Types.notif_val) (prgm : Types.program) : unit = 
		match prgm with Types.Program(name, relations) ->
		match notif with Types.Notif_val(ntype, _) ->
		let _ = List.iter (fun rel -> match rel with
			| Types.NotifRelation(_, args, _) -> (match args with
				| [] -> raise (Failure "NotifRelations always have two arguments");
				| Types.Arg_notif(Types.Notif_var(nt, _)) :: _ -> if nt = ntype then fire_relation prgm rel notif;
				| _ -> raise (Failure "NotifRelations always have an Arg_notif as their first argument"););
			| _ -> ();) relations in
		let _ = List.iter (fun rel -> match rel with
			| Types.MinusRelation(_, args, _) -> (match args with
				| [] -> raise (Failure "MinusRelations always have at least two arguments");
				| Types.Arg_notif(Types.Notif_var(nt, _)) :: _ -> if nt = ntype then fire_relation prgm rel notif;
				| _ -> raise (Failure "MinusRelations always have an Arg_notif as their first argument"););
			| _ -> ();) relations in
		let _ = List.iter (fun rel -> match rel with
			| Types.PlusRelation(_, args, _) -> (match args with
				| [] -> raise (Failure "PlusRelations always have at least two arguments");
				| Types.Arg_notif(Types.Notif_var(nt, _)) :: _ -> if nt = ntype then fire_relation prgm rel notif;
				| _ -> raise (Failure "PlusRelations always have an Arg_notif as their first argument"););
			| _ -> ();) relations in ();;
	

end