open NAT as nat
open util/ternary

// Note: this should only hold assumptions about static config
// assumptions about dynamic tables should be put in appropriate preds
fact assumptions {
	all s: State | {

		// seqpt GW match natconfig GW
		s.natconfig[Switchid][Portid][Portid] = s.seqpt.Tpport.Nwprotocol

		// no gateways shared between switches
		all disj sw1, sw2: Switchid | no s.natconfig[sw1][Portid][Portid] & s.natconfig[sw2][Portid][Portid]

		// ASSUMPTION FROM MODULE CONSTRUCTION: only one outside nat gateway per router
		all sw: Switchid | lone s.natconfig[sw][Portid][Portid]		
	}

	// The add built-in is total and functional for TPports+1
	all pt : Tpport | one BuiltIns.add[pt][C_1]
	// Only increments, only Tpports
	BuiltIns.add in (Tpport -> C_1 -> Tpport)

	// add has no length 1 cycles
	no (select13[BuiltIns.add] & iden)

	// NOTE: Nothing forces add to behave like a true successor function. It may have cycles, etc.
	// Good enough for analysis so far. If we need more axioms, we can add them.
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// tcp only
pred IPFAssign[s: State] {
  // ip tcp ip tcp

	all ip: Ipaddr, pt: Tpport | {
		// lone: partial function. at most one row left for each original ip/pt combo
		lone s.ptassigntcp[ip][pt]	
		
	}
}

pred PFunctionalSeq[s: State] {	
	// For every NAT-configured gateway and L4 protocol
	all ip: s.natconfig[Switchid][Portid][Portid], pro: C_nwprotocol_6+C_nwprotocol_17 | {
		one s.seqpt[ip][pro]
	}
}

assert indInjectivePartialFunction
{
	all s, s': State, pin: EVpacket | 
		(nat/transition[s, pin, s'] and // transition state thusly
		  IPFAssign[s] and PFunctionalSeq[s]) 
		implies
		(IPFAssign[s'] and PFunctionalSeq[s'])
}
check indInjectivePartialFunction

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// For sanity checking
run {some s, s': State, pin: EVpacket | 
		nat/transition[s, pin, s']} 
