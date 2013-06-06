#load "unix.cma";;
#load "xsb.cmo";;
(*#load "oUnit.cma";;*)

open Unix;;
open Xsb;;
(*open OUnit;;*)

module Program = struct

(* constants and variables (recall variables are uppercase) *)
type term = Constant of string | Variable of string;;

(* Things like A = B or R(A, B, C) *)
type atom = Equals of term * term | Apply of relation * term list | Bool of string
and
(* the and is for mutually recursive types *)
(* Atoms and negations of atoms *)
	literal = Pos of atom | Neg of atom
and
(* First string is name, second is list of arguments, third is body *)
	 clause = Clause of relation * term list * literal list
and
(* First string is relation's name, the string list is a list of arguments
the clauses are definitional clauses, the two options are plus_ and minus_ relations or none respectively *)
	 relation = Relation of string * term list * clause list * relation option * relation option;;

(* list of non-emit relations, emit relation*)
type program = Program of relation list * relation;;

let term_to_string (t : term) : string = 
	match t with
	| Constant(c) -> c; 
	| Variable(v) -> v;; (* v must be upper case *)

let relation_name (rel : relation) : string =
	match rel with 
	| Relation(name, _, _, _, _) -> name;;

let list_to_string (l : 'a list) (conversion : 'a -> string) : string = 
	let ans = List.fold_right (fun x acc -> (conversion x) ^ "," ^ acc) l "" in
	String.sub ans 0 (String.length ans - 1);;

let atom_to_string (a : atom) : string =
	match a with
	| Equals(t1, t2) -> term_to_string(t1) ^ " = " ^ term_to_string(t2);
	| Apply(rel, args) -> (relation_name rel) ^ "(" ^ (list_to_string args term_to_string) ^ ")";
	| Bool(str) -> str;;

let literal_to_string (l : literal) : string = 
	match l with
	| Pos(a) -> atom_to_string a;
	| Neg(a) -> "not(" ^ (atom_to_string a) ^ ")";;

let clause_to_string (cl : clause) : string =
	match cl with
	| Clause(rel, args, []) -> (relation_name rel) ^ "(" ^ (list_to_string args term_to_string) ^ ")";
	| Clause(rel, args, body) -> (relation_name rel) ^ "(" ^ (list_to_string args term_to_string) ^ ") :- " ^
		(list_to_string body literal_to_string);;

let empty_body (cl : clause) : bool =
	match cl with
	| Clause(_, _, []) -> true;
	| _ -> false;;

let get_atom (lit : literal) : atom =
	match lit with
	| Pos(a) -> a;
	| Neg(a) -> a;;

let add_unique (x : 'a) (l : 'a list) : 'a list= 
	if List.mem x l then l else x :: l;;

let add_unique_var (t : term) (acc : term list) : term list = 
	match t with
	| Variable(_) -> add_unique t acc;
	| Constant(_) -> acc;;

let get_vars (cl : clause) : term list =
	match cl with
	| Clause(_, args, body) -> List.fold_right 
		(fun (lit : literal) (acc : term list) -> 
			match get_atom(lit) with
			| Equals(t1, t2) -> add_unique_var t1 (add_unique_var t2 acc);
			| Apply(_, tl) -> List.fold_right add_unique_var tl acc;
			| Bool(str) -> acc;)
		body
		(List.fold_right add_unique_var args []);;

let assert_clause (cl : clause) (out_ch : out_channel) (in_ch : in_channel): string =
	let output = "assert((" ^ (clause_to_string cl) ^ "))." in
	(*let _ = print_endline output in*)
	let num_vars = List.length (get_vars cl) in
	if num_vars > 0 then Xsb.lol_to_string(Xsb.send_query output (List.length (get_vars cl)) out_ch in_ch)
	else let _ = Xsb.send_assert output out_ch in_ch in "[]";;

let query_clause (cl : clause) (out_ch : out_channel) (in_ch : in_channel): string =
	let output = (clause_to_string cl) ^ "." in
(*	let _ = print_endline output in*)
	let num_vars = List.length (get_vars cl) in
	if num_vars > 0 then Xsb.lol_to_string(Xsb.send_query output (List.length (get_vars cl)) out_ch in_ch)
	else let _ = Xsb.send_assert output out_ch in_ch in "[]";;

let assert_relation (rel : relation) (out_ch : out_channel) (in_ch : in_channel) : string =
	match rel with
	| Relation(_, args, [], _, _) -> assert_clause (Clause(rel, args, [Pos(Bool("false"))])) out_ch in_ch;
	| Relation(_, _, clauses, _, _) -> List.fold_right (fun cls acc -> (assert_clause cls out_ch in_ch) ^ acc) clauses "";;

let start_program (prgm : program) (out_ch : out_channel) (in_ch : in_channel) : string = 
	match prgm with
	| Program(relations, emit) -> let out = List.fold_right (fun rel acc -> (assert_relation rel out_ch in_ch) ^ acc) relations "" in
		out ^ (assert_relation emit out_ch in_ch);;

let respond_to_packet (prgm : program) : (string list) list =
	[];;

end

(* encoding of mac_learning *)
open Program;;

let packet_vars = List.map (fun (str : string) -> Variable(str)) ["LocSw"; "LocPt"; "DlSrc"; "DlDst"; "DlTyp"; "NwSrc"; "NwDst"; "NwProto"];;
let packet_vars_2 = List.map (fun (str : string) -> Variable(str)) ["LocSw2"; "LocPt2"; "DlSrc2"; "DlDst2"; "DlTyp2"; "NwSrc2"; "NwDst2"; "NwProto2"];;
let learned_vars = [Variable("Sw"); Variable("Pt"); Variable("Mac")]

let rec plus_learned = Clause(plus_learned_relation, packet_vars @ learned_vars,
	[Pos(Equals(Variable("LocSw"), Variable("Sw")));
	Pos(Equals(Variable("DlSrc"), Variable("Mac")));
	Pos(Equals(Variable("LocPt"), Variable("Pt")))])
	and
	plus_learned_relation = Relation("plus_learned", packet_vars @ learned_vars, [plus_learned], None, None);;

let rec minus_learned = Clause(minus_learned_relation, packet_vars @ learned_vars,
	[Pos(Equals(Variable("LocSw"), Variable("Sw")));
	Pos(Equals(Variable("DlSrc"), Variable("Mac")));
	Neg(Equals(Variable("LocPt"), Variable("Pt")))])
	and
	minus_learned_relation = Relation("minus_learned", packet_vars @ learned_vars, [minus_learned], None, None);;

let learned_relation = Relation("learned", learned_vars, [], Some plus_learned_relation, Some minus_learned_relation);;

let rec emit_1 = Clause(emit_relation, packet_vars @ packet_vars_2,
	[Pos(Apply(learned_relation, [Variable("LocSw"); Variable("LocPt2"); Variable("DlDst")]));
	Pos(Equals(Variable("LocSw"), Variable("LocSw2")));
	Pos(Equals(Variable("DlSrc"), Variable("DlSrc2")));
	Pos(Equals(Variable("DlDst"), Variable("DlDst2")));	
	Pos(Equals(Variable("DlTyp"), Variable("DlTyp2")));
	Pos(Equals(Variable("NwSrc"), Variable("NwSrc2")));
	Pos(Equals(Variable("NwDst"), Variable("NwDst2")));
	Pos(Equals(Variable("NwProto"), Variable("NwProto2")))
	])
	and
	emit_2 = Clause(emit_relation, packet_vars @ packet_vars_2,
	[Pos(Equals(Variable("LocSw"), Variable("LocSw2")));
	Pos(Equals(Variable("DlSrc"), Variable("DlSrc2")));
	Pos(Equals(Variable("DlDst"), Variable("DlDst2")));
	Pos(Equals(Variable("DlTyp"), Variable("DlTyp2")));
	Pos(Equals(Variable("NwSrc"), Variable("NwSrc2")));
	Pos(Equals(Variable("NwDst"), Variable("NwDst2")));
	Pos(Equals(Variable("NwProto"), Variable("NwProto2")));
	Pos(Apply(switch_has_ports_relation, [Variable("LocSw2"); Variable("LocPt2")]));
	Neg(Equals(Variable("LocPt"), Variable("LocPt2")));
	Neg(Apply(learned_relation, [Variable("LocSw"); Variable("Any"); Variable("DlDst")]))
	])
	and
	emit_relation = Relation("emit", packet_vars @ packet_vars_2, [emit_1; emit_2], None, None)
	and
	switch_1 = Clause(switch_has_ports_relation, [Constant("1"); Constant("1")], [])
	and
	switch_2 = Clause(switch_has_ports_relation, [Constant("1"); Constant("2")], [])
	and
	switch_3 = Clause(switch_has_ports_relation, [Constant("1"); Constant("3")], [])
	and
	switch_has_ports_relation = Relation("switchHasPorts", [Variable("Sw"); Variable("Pt")], [switch_1; switch_2; switch_3], None, None);;

let mac_learning_program = Program([plus_learned_relation; minus_learned_relation; learned_relation; switch_has_ports_relation], emit_relation);;

(* try out the functions *)

(*print_endline(clause_to_string switch_1);;*)

let out_ch, in_ch = Xsb.start_xsb ();;
(* run the program *)
Program.start_program mac_learning_program out_ch in_ch;;

print_endline(Program.query_clause (Clause(switch_has_ports_relation, [Variable("X"); Variable("Y")], [])) out_ch in_ch);;
print_endline(Program.query_clause (Clause(emit_relation,
	[Constant("1");
	Constant("2");
	Constant("3");
	Constant("4");
	Constant("5");
	Constant("6");
	Constant("7");
	Constant("8");
	Variable("LocSw2");
	Variable("LocPt2");
	Variable("DlSrc2");
	Variable("DlDst2");
	Variable("DlTyp2");
	Variable("NwSrc2");
	Variable("NwDst2");
	Variable("NwProto2")
	], [])) out_ch in_ch);;

Xsb.halt_xsb out_ch;;

(* these work! *)
(*

print_endline (list_to_string (Program.get_vars (Clause(emit_relation,
	[Constant("1");
	Constant("2");
	Constant("3");
	Constant("4");
	Constant("5");
	Constant("6");
	Constant("7");
	Constant("8");
	Variable("LocSw2");
	Variable("LocPt2");
	Variable("DlSrc2");
	Variable("DlDst2");
	Variable("DlTyp2");
	Variable("NwSrc2");
	Variable("NwDst2");
	Variable("NwProto2")
	], []))) term_to_string);;


print_endline(Xsb.lol_to_string (Xsb.send_query "switchHasPorts(X, Y)." 2 out_ch in_ch));;
flush Pervasives.stdout;;
*)
(*
print_endline(Program.assert_clause switch_2 out_ch in_ch);;

print_endline(Xsb.lol_to_string (Xsb.send_query "switchHasPorts(X, Y)." 2 out_ch in_ch));;
flush Pervasives.stdout;;

print_endline(Program.assert_clause switch_3 out_ch in_ch);;

print_endline(Xsb.lol_to_string (Xsb.send_query "switchHasPorts(X, Y)." 2 out_ch in_ch));;
flush Pervasives.stdout;;

print_endline(Program.assert_clause plus_learned out_ch in_ch);;

print_endline(Program.assert_clause emit_2 out_ch in_ch);;
(* THIS doesn't work because the Any variable gets assigned something so the semicolons are messed up. *)

*)








(*let packet_list (pkt : packet) : int list =
	match pkt with
	Packet(locSw, locPt, dlSrc, dlDst, dlTyp, nwSrc, nwDst, nwProto) ->
	[locSw; locPt; dlSrc; dlDst; dlTyp; nwSrc; nwDst; nwProto];;





let assert_relation (rel : relation) (out_ch : out_channel) (in_ch : in_channel) : string = 
	match rel with
	Relation(name, args, clist, plus, minus) -> List.fold_right (fun cls st -> (assert_clause cls out_ch in_ch) ^ st) clist "";;

let start_program (prgm : program) (out_ch : out_channel) (in_ch : in_channel) : string = 
	match prgm with
	Program(rl) -> List.fold_right (fun rel st -> (assert_relation rel out_ch in_ch) ^ st) rl "";;

let rec drop (l : 'a list) (n : int) : 'a list = 
	if n <= 0 then l else
	match l with
	| [] -> [];
	| h :: t -> drop t (n - 1);;

let query_relation (rel : relation) (pkt : packet) (out_ch : out_channel) (in_ch : in_channel) : (string list) list =
	match rel with
	Relation(name, args, clist, plus, minus) -> let query_string = name ^ "(" ^ (list_to_string (packet_list pkt) string_of_int) ^ 
		(if (List.length args = 8) then "" else "," ^ (list_to_string  (drop args 8) (fun x -> x))) ^ ")." in
		Xsb.send_query query_string (List.length args - 8) out_ch in_ch;;

(*let respond_packet (prgm : program) (pkt : packet) : packet list = 
	match prgm with
	Program(rl) -> *)
end

(* encoding of mac_learning *)


(* print_endline(Program.start_program mac_learning_program out_channel in_ch);; *)

(* use ounit for testing? *)
(*let test_fixture = "Program" >:::
[
	"add" >:: (fun () ->
		assert_equal 4 (add 2 2);
	);
	...
]

let _ = run_test_tt -verbose:true test_fixture*)

*)


