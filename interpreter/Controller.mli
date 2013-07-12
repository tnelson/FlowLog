open Flowlog_Types;;

module type PROGRAM = sig
	val program : Types.program;;
end

module Make_Controller : functor (Program : PROGRAM) -> sig end;;