open Flowlog;;
open Controller;;

module Arp_cache : PROGRAM = struct
include Flowlog;;

let learned_vars = [Variable("Ip"); Variable("Mac")];;

let rec plus_learned = Clause(plus_learned_relation, packet_vars @ learned_vars,
	[Pos(Equals(Variable("DlTyp"), Constant("0x0806")));
	Pos(Equals(Variable("NwProto"), Constant("2")));
	Pos(Equals(Variable("Ip"), Variable("NwSrc")));
	Pos(Equals(Variable("Mac"), Variable("DlSrc")));
	Neg(Apply(learned_relation, [Variable("NwSrc"); Variable("Any")]))])
	and
	plus_learned_relation = Relation("plus_learned", packet_vars @ learned_vars, [plus_learned], None, None)
	and
	learned_relation = Relation("learned", learned_vars, [], Some(plus_learned_relation), None);;

let rec forward_1 = Clause(forward_relation, packet_vars @ packet_vars_2,
	[Pos(Equals(Variable("DlTyp"), Constant("0x0806")));
	Pos(Equals(Variable("NwProto"), Constant("2")))])
	and
	forward_2 = Clause(forward_relation, packet_vars @ packet_vars_2,
	[Pos(Equals(Variable("DlTyp"), Constant("0x0806")));
	Pos(Equals(Variable("NwProto"), Constant("1")));
	Neg(Apply(learned_relation, [Variable("NwSrc"); Variable("Any")]))])
	and
	forward_3 = Clause(forward_relation, packet_vars @ packet_vars_2,
	[Pos(Equals(Variable("DlTyp"), Constant("0x0806")));
	Pos(Equals(Variable("NwProto"), Constant("1")));
	Pos(Equals(Variable("DlTyp2"), Constant("0x0806")));
	Pos(Equals(Variable("NwProto2"), Constant("2")));
	Pos(Apply(learned_relation, [Variable("NwDst"); Variable("DlSrc2")]));
	Pos(Equals(Variable("NwDst2"), Variable("NwSrc")));
	Pos(Equals(Variable("DlDst2"), Variable("DlSrc")));
	Pos(Equals(Variable("NwSrc2"), Variable("NwDst")))])
	and
	forward_relation = Relation("forward", packet_vars @ packet_vars_2, [forward_1; forward_2; forward_3], None, None);;

let program = Program("arp_cache", [plus_learned_relation; learned_relation], forward_relation);;
end

(*module Run = Controller.Make_Controller (Arp_cache);;*)