open Flowlog;;

module type PROGRAM = sig
	val program : Flowlog.program;;
end

module Make_Controller : functor (Program : PROGRAM) -> sig end;;

(*module Union: functor (Pg1 : PROGRAM) -> functor (Pg2 : PROGRAM) -> PROGRAM;;*)