open Flowlog;;
open Controller;;

module Mac_learning : PROGRAM = struct
include Syntax;; 

let packet_var = Notif_var(Syntax.packet_type, "Pkt");;
let packet_arg = Arg_notif(packet_var);;
let packet_var_2 = Notif_var(Syntax.packet_type, "Pkt2");;
let packet_arg_2 = Arg_notif(packet_var_2);;
let switch_port_var = Notif_var(Syntax.switch_port_type, "Sw_pt");;
let switch_port_arg = Arg_notif(switch_port_var);;
let learned_args = [Arg_term(Variable("Sw")); Arg_term(Variable("Pt")); Arg_term(Variable("Mac"))];;

let plus_learned = Clause("+learned/mac_learning", packet_arg :: learned_args,
	[Pos(Equals(Field_ref(packet_var, "LocSw"), Variable("Sw")));
	Pos(Equals(Field_ref(packet_var, "DlSrc"), Variable("Mac")));
	Pos(Equals(Field_ref(packet_var, "LocPt"), Variable("Pt")))]);;

let minus_learned = Clause("-learned/mac_learning", packet_arg :: learned_args,
	[Pos(Equals(Field_ref(packet_var, "LocSw"), Variable("Sw")));
	Pos(Equals(Field_ref(packet_var, "DlSrc"), Variable("Mac")));
	Neg(Equals(Field_ref(packet_var, "LocPt"), Variable("Pt")))]);;
	
let forward_1 = Clause("forward/mac_learning", [packet_arg; packet_arg_2],
	[Pos(Apply("learned/mac_learning", [Field_ref(packet_var , "LocSw"); Field_ref(packet_var_2, "LocPt"); Field_ref(packet_var, "DlDst")]))]);;

let	forward_2 = Clause("forward/mac_learning", [packet_arg; packet_arg_2],
	[Pos(Apply("switch_has_port/mac_learning", [Field_ref(packet_var, "LocSw"); Field_ref(packet_var_2, "LocPt")]));
	Neg(Apply("learned/mac_learning", [Field_ref(packet_var, "LocSw"); Variable("Any"); Field_ref(packet_var, "DlDst")]));
	Neg(Equals(Field_ref(packet_var, "LocPt"), Field_ref(packet_var_2, "LocPt")))]);;

let plus_switch_has_port = Clause("+switch_has_port/mac_learning", [switch_port_arg; Arg_term(Variable("Sw")); Arg_term(Variable("Pt"))],
[Pos(Equals(Field_ref(switch_port_var, "Sw"), Variable("Sw")));
Pos(Equals(Field_ref(switch_port_var, "Pt"), Variable("Pt")))]);;


let plus_learned_relation = Relation("+learned/mac_learning", packet_arg :: learned_args, [plus_learned]);;
let minus_learned_relation = Relation("-learned/mac_learning", packet_arg :: learned_args, [minus_learned]);;
let learned_relation = Relation("learned/mac_learning", learned_args, []);;
let forward_relation = Relation("forward/mac_learning", [packet_arg; packet_arg_2], [forward_1; forward_2]);;
let plus_switch_port_relation = Relation("+switch_has_port/mac_learning", [switch_port_arg; Arg_term(Variable("Sw")); Arg_term(Variable("Pt"))], [plus_switch_has_port]);;
let switch_port_relation = Relation("switch_has_port/mac_learning", [Arg_term(Variable("Sw")); Arg_term(Variable("Pt"))], []);;

let program = Program("mac_learning", [plus_learned_relation; minus_learned_relation; learned_relation; forward_relation; plus_switch_port_relation; switch_port_relation]);;

end

module Run = Controller.Make_Controller (Mac_learning);;