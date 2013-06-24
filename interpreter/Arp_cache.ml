open Flowlog;;
open Controller;;

module Arp_cache : PROGRAM = struct
include Flowlog;;

let learned_vars = [Variable("Ip"); Variable("Mac")];;

let plus_learned = Clause("+learned", packet_vars @ learned_vars,
	[Pos(Equals(Variable("DlTyp"), Constant("0x0806")));
	Pos(Equals(Variable("NwProto"), Constant("2")));
	Pos(Equals(Variable("Ip"), Variable("NwSrc")));
	Pos(Equals(Variable("Mac"), Variable("DlSrc")));
	Neg(Apply("learned", [Variable("NwSrc"); Variable("Any")]))])

let	plus_learned_relation = Relation("+learned", packet_vars @ learned_vars, [plus_learned])

let	learned_relation = Relation("learned", learned_vars, []);;

let forward_1 = Clause("forward", packet_vars @ packet_vars_2,
	[Pos(Equals(Variable("DlTyp"), Constant("0x0806")));
	Pos(Equals(Variable("NwProto"), Constant("2")))])

let	forward_2 = Clause("forward", packet_vars @ packet_vars_2,
	[Pos(Equals(Variable("DlTyp"), Constant("0x0806")));
	Pos(Equals(Variable("NwProto"), Constant("1")));
	Neg(Apply("learned", [Variable("NwSrc"); Variable("Any")]))])

let	forward_3 = Clause("forward", packet_vars @ packet_vars_2,
	[Pos(Equals(Variable("DlTyp"), Constant("0x0806")));
	Pos(Equals(Variable("NwProto"), Constant("1")));
	Pos(Equals(Variable("DlTyp2"), Constant("0x0806")));
	Pos(Equals(Variable("NwProto2"), Constant("2")));
	Pos(Apply("learned", [Variable("NwDst"); Variable("DlSrc2")]));
	Pos(Equals(Variable("NwDst2"), Variable("NwSrc")));
	Pos(Equals(Variable("DlDst2"), Variable("DlSrc")));
	Pos(Equals(Variable("NwSrc2"), Variable("NwDst")))])

let	forward_relation = Relation("forward", packet_vars @ packet_vars_2, [forward_1; forward_2; forward_3]);;

let program = Program("arp_cache", [plus_learned_relation; learned_relation], forward_relation);;
end

(*module Run = Controller.Make_Controller (Arp_cache);;*)