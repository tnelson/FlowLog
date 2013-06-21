open Flowlog;;
open Controller;;

module Mac_learning : PROGRAM = struct
include Flowlog;;

let packet_vars = List.map (fun (str : string) -> Variable(str)) ["LocSw"; "LocPt"; "DlSrc"; "DlDst"; "DlTyp"; "NwSrc"; "NwDst"; "NwProto"];;
let packet_vars_2 = List.map (fun (str : string) -> Variable(str)) ["LocSw2"; "LocPt2"; "DlSrc2"; "DlDst2"; "DlTyp2"; "NwSrc2"; "NwDst2"; "NwProto2"];;
let learned_vars = [Variable("Sw"); Variable("Pt"); Variable("Mac")];;

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

let learned_relation = Relation("learned", learned_vars, [], Some(plus_learned_relation), Some(minus_learned_relation));;

let rec forward_1 = Clause(forward_relation, packet_vars @ packet_vars_2,
	[Pos(Apply(learned_relation, [Variable("LocSw"); Variable("LocPt2"); Variable("DlDst")]))])
	and
	forward_2 = Clause(forward_relation, packet_vars @ packet_vars_2,
	[Neg(Apply(learned_relation, [Variable("LocSw"); Variable("Any"); Variable("DlDst")]))])
	and
	forward_relation = Relation("forward", packet_vars @ packet_vars_2, [forward_1; forward_2], None, None);;

let program = Program([plus_learned_relation; minus_learned_relation; learned_relation], forward_relation);;
end

module Run = Controller.Make_Controller (Mac_learning);;