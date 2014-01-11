open Flowlog_Types
open Flowlog_Helpers
open NetCore_Types
open Printf
open ExtList.List

(* Some predicates are built in, e.g. "add". So for instance if a formula is:
   FAtom("", "add", [TConst("1"), TVar("x"), TVar("pt")]) the add is interpreted by this module, not
   treated as a state predicate. *)

(* predid,
   arity (types),
   XSB definition,
   can-compile? predicate on specific fmla,
   fmla->netcore predicate (for when can-compile? returns true) *)
type builtin_predicate = { bipid: string;
                           biparity: typeid list;
                           bipxsb: xsbmode -> term list -> string;
                           bip_compile: (term list -> pred) option};;

(* TODO: concern about types here: add has arity (int,int,int) but also should apply to
   any other numeric type, like tpport or macaddr. *)

exception BIPAddException of term list;;

(* FAtom("", "add", ...) -->
   *)
let bip_add_xsb (mode:xsbmode) (tl: term list): string =
	match tl with
		| [t1; t2; t3] ->
			(*printf "%s %s %s\n%!" (string_of_term t1) (string_of_term t2) (string_of_term t3);*)

			(* XSB requires the "result" to be on the LHS. E.g., 5 is X + 4 will give an error.
            TODO validate: how to know which variable goes on the LHS? May be multiples!
            E.g., Y is X + 4 is fine if preceded by r(X).
            For now, use 3rd arg as LHS. *)
			sprintf "(%s is %s + %s)"
           (xsb_of_term ~mode:mode t3) (xsb_of_term ~mode:mode t1) (xsb_of_term ~mode:mode t2)
		| _ -> raise (BIPAddException(tl));;

let bip_add = { bipid="add";
				biparity = ["int"; "int"; "int"];
				bipxsb = bip_add_xsb;
				bip_compile = None};;


(**************************************)
let builtin_predicates = [(bip_add.bipid, bip_add)];;


let is_built_in (relname: string): bool =
  mem_assoc relname builtin_predicates;;

let get_built_in (relname: string): builtin_predicate =
  assoc relname builtin_predicates;;

let is_uncompilable_built_in (relname: string): bool =
  is_built_in relname && (get_built_in relname).bip_compile = None;;

let xsb_for_built_in (mode:xsbmode) (relname: string) (tl: term list): string =
   let bip = get_built_in relname in
      bip.bipxsb mode tl;;

(* if unused for... do ... ? *)