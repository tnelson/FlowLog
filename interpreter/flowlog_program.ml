#load "unix.cma";;
#load "xsb.cmo";;
(*#load "oUnit.cma";;*)

open Unix;;
open Xsb;;
(*open OUnit;;*)

module Program = struct

(* LocSw, LocPt, DlSrc, DlDst, DlTyp, NwSrc, NwDst, NwProto *)
type packet = Packet of int * int * int * int * int * int * int * int;;

(* First string is name, second is list of arguments, third is body *)
type clause = Clause of string * string list * string;;

(* First string is relation's name, the string list is a list of arguments
the clauses are definitional clauses, the two options are plus_ and minus_ relations or none respectively *)
type relation = Relation of string * string list * clause list * relation option * relation option;;

type program = Program of relation list;;

let packet_list (pkt : packet) : int list =
	match pkt with
	Packet(locSw, locPt, dlSrc, dlDst, dlTyp, nwSrc, nwDst, nwProto) ->
	[locSw; locPt; dlSrc; dlDst; dlTyp; nwSrc; nwDst; nwProto];;

let list_to_string (l : 'a list) (conversion : 'a -> string) : string = 
	let ans = List.fold_right (fun x st -> (conversion x) ^ "," ^ st) l "" in
	String.sub ans 0 (String.length ans - 1);;

let assert_clause (cls : clause) (out_ch : out_channel) (in_ch : in_channel): string =
	match cls with
	Clause(name, args, body) -> let args_string_comma = (List.fold_right (fun arg str -> arg ^ "," ^ str) args "") in
		let full_body = if (body = "") then "" else (":-" ^ body) in
		let output = "assert((" ^ name ^ "(" ^ (String.sub args_string_comma 0 (String.length args_string_comma - 1)) ^ ")" ^ full_body ^ "))." in
		Xsb.send_assert output out_ch in_ch;;

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
open Program;;

let plus_learned = Clause("plus_learned", ["LocSw"; "LocPt"; "DlSrc"; "DlDst"; "DlTyp"; "NwSrc"; "NwDst"; "NwProto"; "Sw"; "Pt"; "Mac"],
	"LocSw = Sw, DlSrc = Mac, LocPt = Pt");;

let minus_learned = Clause("minus_learned", ["LocSw"; "LocPt"; "DlSrc"; "DlDst"; "DlTyp"; "NwSrc"; "NwDst"; "NwProto"; "Sw"; "Pt"; "Mac"],
	"LocSw = Sw, DlSrc = Mac, not(LocPt = Pt)");;

let emit_1 = Clause("emit", ["LocSw"; "LocPt"; "DlSrc"; "DlDst"; "DlTyp"; "NwSrc"; "NwDst"; "NwProto"; 
	"LocSw2"; "LocPt2"; "DlSrc2"; "DlDst2"; "DlTyp2"; "NwSrc2"; "NwDst2"; "NwProto2"],
"learned(LocSw, LocPt2, DlDst),
LocSw = LocSw2,
DlSrc = DlSrc2,
DlDst = DlDst2,
DlTyp = DlTyp2,
NwSrc = NwSrc2,
NwDst = NwDst2,
NwProto = NwProto2");;

let emit_2 = Clause("emit", ["LocSw"; "LocPt"; "DlSrc"; "DlDst"; "DlTyp"; "NwSrc"; "NwDst"; "NwProto"; 
	"LocSw2"; "LocPt2"; "DlSrc2"; "DlDst2"; "DlTyp2"; "NwSrc2"; "NwDst2"; "NwProto2"],
"LocSw = LocSw2,
DlSrc = DlSrc2,
DlDst = DlDst2,
DlTyp = DlTyp2,
NwSrc = NwSrc2,
NwDst = NwDst2,
NwProto = NwProto2,
switchHasPorts(LocSw2, LocPt2),
LocPt \\= LocPt2,
not(learned(LocSw, Any, DlDst))");;

let switch_1 = Clause("switchHasPorts", ["1"; "1"], "");;
let switch_2 = Clause("switchHasPorts", ["1"; "2"], "");;
let switch_3 = Clause("switchHasPorts", ["1"; "3"], "");;

let plus_learned_relation = Relation("plus_learned", ["LocSw"; "LocPt"; "DlSrc"; "DlDst"; "DlTyp"; "NwSrc"; "NwDst"; "NwProto"; "Sw"; "Pt"; "Mac"],
[plus_learned], None, None);;

let minus_learned_relation = Relation("minus_learned", ["LocSw"; "LocPt"; "DlSrc"; "DlDst"; "DlTyp"; "NwSrc"; "NwDst"; "NwProto"; "Sw"; "Pt"; "Mac"],
[minus_learned], None, None);;

let learned_relation = Relation("learned", ["Sw"; "Pt"; "Mac"], [], Some plus_learned_relation, Some minus_learned_relation);;

let switch_has_ports_relation = Relation("switchHasPorts", ["Sw"; "Pt"], [switch_1; switch_2; switch_3], None, None);;

let emit_relation = Relation("emit", ["LocSw"; "LocPt"; "DlSrc"; "DlDst"; "DlTyp"; "NwSrc"; "NwDst"; "NwProto"], [emit_1; emit_2], None, None);;

let mac_learning_program = Program([plus_learned_relation; minus_learned_relation; learned_relation; switch_has_ports_relation; emit_relation]);;

(* try out the functions *)

let out_ch, in_ch = Xsb.start_xsb ();;

(* these work! *)
print_endline(Program.assert_clause switch_1 out_ch in_ch);;

print_endline(Xsb.lol_to_string (Xsb.send_query "switchHasPorts(X, Y)." 2 out_ch in_ch));;
flush Pervasives.stdout;;

Xsb.halt_xsb out_ch;;

(* use ounit for testing? *)
(*let test_fixture = "Program" >:::
[
	"add" >:: (fun () ->
		assert_equal 4 (add 2 2);
	);
	...
]

let _ = run_test_tt -verbose:true test_fixture*)




