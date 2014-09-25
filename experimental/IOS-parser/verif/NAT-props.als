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

	// Want: The add built-in is total and functional for TPports+1
    // But stating that every Tpport has one after it is problematic for term-counting (induces a function)
	// Instead, say that there is at MOST one success for all ports, and force existence only for fields of packets
  	all pt : EVtcp_packet.tpsrc + EVtcp_packet.tpdst + EVudp_packet.tpsrc + EVudp_packet.tpdst  | 
		{one BuiltIns.add[pt][C_1]  }
	all pt : Tpport | lone BuiltIns.add[pt][C_1]

	// Only increments, only Tpports
	BuiltIns.add in (Tpport -> C_1 -> Tpport)

	// add has no length 0 cycles
	no (select13[BuiltIns.add] & iden)
	// and no length 1 cycles [enough to guarantee safe increment for ONE event
	all pt1,pt2: Tpport | pt1 not in select13[BuiltIns.add][pt2] or pt2 not in select13[BuiltIns.add][pt1]

	// next port to use is always bigger than any used (assume no wraparound)
	// Suffices to say that the NEXT is never equal to any used; limited no wraparound 
	// Safe for TC because we say "if there *is* any next, then..."
/*	all pt, pt' : Tpport | 
		pt' in BuildIns.add[pt][C_1] implies {
			
		}
*/
// ^^ what if chain of 2 in current state?

	// NOTE: Nothing forces add to behave like a true successor function. It may have cycles, etc.
	// Good enough for analysis so far. If we need more axioms, we can add them.
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


pred IPFAssign[s: State] {
/*	all ip: Ipaddr, pt: Tpport | {
		// lone: partial function. at most one row left for each original ip/pt combo
		lone s.ptassigntcp[ip][pt]			
		lone s.ptassignudp[ip][pt]
	}
*/
	// injectivity: for every new port used, there is at most one original pair
	all pt: Tpport | {
		lone s.ptassigntcp.pt
	//	lone s.ptassignudp.pt
	}

}

// TODO injectivity!

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
		  IPFAssign[s] and PFunctionalSeq[s] ) // "good" prestate
		implies
		(IPFAssign[s'] and PFunctionalSeq[s'] ) // "good" post-state
}
check indInjectivePartialFunction

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Confirm inductive property used in outNATFlipped below

assert indNoGWtoGWFlows {
	all s, s': State, pin: EVpacket | 
		(nat/transition[s, pin, s'] and // transition state thusly
		 pin.nwsrc not in s.natconfig[pin.locsw][Portid][Portid]  and // not coming from a local gateway (odd situation)
		 noSameAddressAssigns[s]) // good prestate
		implies noSameAddressAssigns[s'] // good poststate
}
check indNoGWtoGWFlows

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// "good state" requirement: no wonky flows already established where the original src is the same gateway
pred noSameAddressAssigns[s: State] {
	no iden & select13[s.ptassigntcp.Tpport]
	no iden & select13[s.ptassignudp.Tpport]
}

// We're reasoning about packets on the NAT sub-switch. If a packet arrives, it needs
// to be NATted or de-NATted. Confirm that passing through always flips UDP/TCP traffic.
assert outNATFlipped {
	all s: State, pin, pout: EVip_packet | 
	let localnatgateways = s.natconfig[pin.locsw][Portid][Portid] | // maybe a chain of NAT boxes
		(nat/outpolicy[s, pin, pout] and  // packet passes through
		 noSameAddressAssigns[s] and
          (pin.nwproto in C_nwprotocol_6+C_nwprotocol_17) ) // packet is TCP or UDP
         implies 
		{
			pin.nwdst in localnatgateways implies pout.nwdst not in localnatgateways // translate returning traffic to orig ip DST
			pin.nwdst not in localnatgateways implies pout.nwsrc in localnatgateways // translate outgoing traffic to new ip SRC
		 }
}

check outNATFlipped
for 5 but 1 State, 2 Event, 2 Switchid, 3 Portid, 10 Tpport, 2 Nwprotocol, 4 Macaddr, 5 Ipaddr, 2 FLInt, 2 Ethtyp
// no counterexample; translation took 236 seconds (solve in <4 sec)

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// For sanity checking
run {some s, s': State, pin: EVpacket | 
		nat/transition[s, pin, s']} 


// for 2x event (w/ assumptions +(2*2); w/ outpolicy variables):
//for 5 but 1 State, 2 Event, 2 Switchid, 3 Portid, 10 Tpport, 2 Nwprotocol, 4 Macaddr, 5 Ipaddr, 2 FLInt, 2 Ethtyp
// + mod from queries?

// for 1x event (w/ assumptions +2)  TODO: TRANSITION VARS
// for 5 but 2 State, 1 Event, 1 Switchid, 1, Portid, 4 Tpport, 1 NwProtocol, 2 Macaddr, 2 Ipaddr, 1 FLInt, 1 Ethtyp

/*
  CEILINGS

  Single event ceiling: 1 Switchid,1 Portid,2 Macaddr,2 Ipaddr,2 Tpport, 1 FLInt,1 Ethtyp, 1 Nwprotocol

  2 queries have 2x state, 1x Event
  1 has 2x Event, 1x State (double ceiling)

  Positive references to outpolicy and transition only. Only single references for outpolicy.
    (Transition necessitates possibly multiple references)

  for outpolicy: 1 ip, 2 tpport, 1 portid

  for transition: TODO

  from assumptions: +2 Tpport for every Event

*/


