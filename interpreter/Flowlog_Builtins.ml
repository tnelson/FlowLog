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
   prepare: run this once in xsb at startup
   compile: this is the expression to give to XSB
    *)
type builtin_predicate = { (* ID must be lowercase *)
                           bipid: string;
                           biparity: typeid list;
                           bipxsb: xsbmode -> term list -> string;
                           (* PREFIX ALL VARIABLES WITH UNDERSCORE IN PREPARE, OR XSB INTERFACE WILL FREEZE *)
                           bip_prepare: (string list) option;
                           bip_compile: unit option};;

(* TODO: concern about types here: add has arity (int,int,int) but also should apply to
   any other numeric type, like tpport or macaddr. *)

exception BIPAddException of term list;;
exception BIPPrefixException of term list;;

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
        bip_prepare = None;
				bip_compile = None};;

(* hasLongerPrefixMatch(p.locSw, pre, mask)
   HARD CODED to use a SPECIFIC table name and arity! *)

let bip_has_longer_xsb (mode:xsbmode) (tl: term list) : string =
  match tl with
    | [sw;dst;pre;mask] ->
      printf "\nInvoked: hasLongerPrefix: %s %s %s %s\n%!" (string_of_term sw) (string_of_term dst) (string_of_term pre) (string_of_term mask);
      sprintf "hasLongerPrefix(%s, %s, %s, %s)"
           (xsb_of_term ~mode:mode sw) (xsb_of_term ~mode:mode dst)  (xsb_of_term ~mode:mode pre) (xsb_of_term ~mode:mode mask)
    | _ -> raise (BIPPrefixException(tl));;

(* Note the hard-coded relation name (routes) and arity.
   Dest is in this new range, and the new range is smaller, and in the old range *)
let bip_prepare_prefix_xsb =
  ["assert((hasLongerPrefix(_SW, _DST, _PRE, _MASK) :- routes(_SW, _P2, _M2, _), _M2 > _MASK, in_ipv4_range(_P2, _PRE, _MASK), in_ipv4_range(_DST, _P2, _M2))).";
   "assert((getPossibleLongerPrefixMasks(_SW, _PRE, _MASK, _P2, _M2) :- routes(_SW, _P2, _M2, _), _M2 > _MASK, in_ipv4_range(_P2, _PRE, _MASK)))."];;

let bip_hasLongerPrefixMatch = {bipid = "haslongerprefixmatch";
          biparity = ["switchid"; "ipaddr"; "ipaddr"; "int"];
          bipxsb = bip_has_longer_xsb;
          bip_prepare = Some bip_prepare_prefix_xsb;
          bip_compile = Some ()
          }

(**************************************)
let builtin_predicates = [(bip_add.bipid, bip_add);
                          (bip_hasLongerPrefixMatch.bipid, bip_hasLongerPrefixMatch)];;


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