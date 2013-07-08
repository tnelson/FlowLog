open Flowlog;;
open Controller;;

module Mac_learning : PROGRAM = struct
include Flowlog_Parsing;; 

let packet_arg = variable_value packet_type "Pkt";;
let packet_arg_2 = variable_value packet_type "Pkt2";;
let learned_args = [Arg_term(Variable("Sw")); Arg_term(Variable("Pt")); Arg_term(Variable("Mac"))];;

let plus_learned = Clause("+learned", packet_arg :: learned_args,
	[Pos(Equals(Field("Pkt", "LocSw"), Variable("Sw")));
	Pos(Equals(Field("Pkt", "DlSrc"), Variable("Mac")));
	Pos(Equals(Field("Pkt", "LocPt"), Variable("Pt")))]);;

let minus_learned = Clause("-learned", packet_arg :: learned_args,
	[Pos(Equals(Field("Pkt", "LocSw"), Variable("Sw")));
	Pos(Equals(Field("Pkt", "DlSrc"), Variable("Mac")));
	Neg(Equals(Field("Pkt", "LocPt"), Variable("Pt")))]);;
	
let forward_1 = Clause("forward", [packet_arg; packet_arg_2],
	[Pos(Apply("learned", [Field("Pkt" , "LocSw"); Field("Pkt2", "LocPt"); Field("Pkt", "DlDst")]))]);;

let	forward_2 = Clause("forward", [packet_arg; packet_arg_2],
	[Neg(Apply("learned", [Field("Pkt", "LocSw"); Variable("Any"); Field("Pkt", "DlDst")]));
	Neg(Equals(Field("Pkt", "LocPt"), Field("Pkt2", "LocPt")))]);;
	
let program = make_program "mac_learning" [packet_type] (make_relations [plus_learned; minus_learned; forward_1; forward_2]);;

end

module Run = Controller.Make_Controller (Mac_learning);;