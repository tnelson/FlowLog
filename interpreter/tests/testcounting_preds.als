/*********************************************************************************/

fact add {
	// no cycles of length 1
	all x,y,z: univ |	z in BuiltIns.add[x][y] implies not z = y and not z = x
}

// Otherwise the containment check in the +R rule is vacuously true
fact constants_exist {
	some C_1
}

pred testPred[] {
	some s, s': State, e: Event |
	{
		transition[s, e, s']
		s.r != s'.r
	}
}

// unsat
run testPred for 0 but 2 State, 1 Event, 
	1 Switchid,1 Portid,2 Macaddr,1 FLInt,1 Ethtyp

// sat
run testPred for 0 but 2 State, 1 Event, 
	1 Switchid,1 Portid,2 Macaddr,2 FLInt,1 Ethtyp
