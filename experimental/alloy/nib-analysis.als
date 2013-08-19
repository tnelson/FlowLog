module Flowlog/NIB

////////////////////////////////////////////////////////////////////////////////////////////////
// TYPES
// ALL boilerplate except controller state definition (and which events are declared)
////////////////////////////////////////////////////////////////////////////////////////////////

sig Event {}

sig Switch {}
sig MacAddr {}
sig IPAddr {}
sig EthTyp {}
sig PhysicalPort {} 
sig NwProtocol {}
sig EVpacket extends Event
{ 
	locSw: one Switch,
    locPt: one PhysicalPort,
    dlSrc: one MacAddr,
    dlDst: one MacAddr,
    dlTyp: one EthTyp,
    nwSrc: one IPAddr,
    nwDst: one IPAddr,
    nwProto: one NwProtocol
}

sig EVprobe extends Event 
{
	locSw: one Switch,
	locPt: one PhysicalPort,
	srcSw: one Switch,
	srcPt: one PhysicalPort 
}			   

sig TimerID {}
sig EVtimerexpired extends Event
{ 
	id: one TimerID
}

// Controller state for NIB
sig State 
{ 
	ucST: Switch -> PhysicalPort,
	ucTC: Switch -> Switch
}

/*one sig emptyState extends State {} 
fact emptyIsEmpty {
	(no emptyState.ucST) and 
	(no emptyState.ucTC)
}*/

// Prevent garbage results: extensional equivalence
fact StateExtensional { all st1, st2: State |
	 (st1.ucTC = st2.ucTC) and (st1.ucST = st2.ucST) implies st1 = st2 }
fact EVpacketExtensional { all pkt1, pkt2: EVpacket | 
        (pkt1.locSw = pkt2.locSw && pkt1.dlSrc = pkt2.dlSrc && pkt1.dlDst = pkt2.dlDst &&
         pkt1.dlTyp = pkt2.dlTyp && pkt1.nwSrc = pkt2.nwSrc && pkt1.nwDst = pkt2.nwDst &&
         pkt1.nwProto = pkt2.nwProto && pkt1.locPt = pkt2.locPt) implies pkt1=pkt2}

fact EVprobeExtensional { all pkt1, pkt2: EVprobe | 
        (pkt1.locSw = pkt2.locSw && pkt1.srcPt = pkt2.srcPt && pkt1.srcSw = pkt2.srcSw &&
          pkt1.locPt = pkt2.locPt) implies pkt1=pkt2}

fact EVtimerexpiredExtensional { all ev1, ev2: EVtimerexpired | ev1.id = ev2.id implies ev1 = ev2 } 

////////////////////////////////////////////////////////////////////////////////////////////////
// PROGRAM: STATE TRANSITIONS AND POLICY
////////////////////////////////////////////////////////////////////////////////////////////////

// IMPORTANT: slight modification: symmetric ucST

// TODO: can we delete either of the symmetric cases?
pred plus_ucTC[st: State, ev: EVprobe, sw1: Switch, sw2: Switch] {
/*	(ev.locSw = sw2 and ev.srcSw = sw1)
	or
  	(ev.locSw = sw1 and ev.srcSw = sw2)

	or
	(ev.locSw->sw2 in st.ucTC and ev.srcSw = sw1)
	or
	(sw1->ev.srcSw in st.ucTC and ev.locSw = sw2)

    // END original 4 rules

	// MODIFICATION: 
	or
	(ev.srcSw->sw2 in st.ucTC and ev.locSw = sw1)
	or
	(sw1->ev.locSw in st.ucTC and ev.srcSw = sw2)

	// Modification: each edge always causes 2 loops! (assuming symmetry)
	(ev.locSw = sw2 and ev.locSw = sw1)
	or
  	(ev.srcSw = sw2 and ev.srcSw = sw1)

	// heh, and one more pair that was missing! 
	or
	(sw1->ev.srcSw in st.ucTC and ev.locSw->sw2 in st.ucTC)
	or 
	(sw1->ev.locSw in st.ucTC and ev.srcSw->sw2 in st.ucTC)*/
// above is force-symmetric TC

	// There are four rules _required_ to do true stepwise TC:

	// Base
	(ev.srcSw = sw1 and ev.locSw = sw2) or
	// Forward extend
	(sw1->ev.srcSw in st.ucTC and ev.locSw = sw2) or
	// Backward extend
	(ev.srcSw = sw1 and ev.locSw->sw2 in st.ucTC) or
	// Bridge
	(sw1->ev.srcSw in st.ucTC and ev.locSw->sw2 in st.ucTC)

	// The initial NIB program lacked the bridge rule.

}

// MODIFIED! 2nd rule to catch symmetric case, and stopped using TC
pred plus_ucST[st: State, ev: EVprobe, sw: Switch, pt: PhysicalPort] {
//	not ev.srcSw->ev.locSw in st.ucTC and
	no st.ucST[ev.srcSw] or no st.ucST[ev.locSw] 
	((ev.srcSw = sw and ev.srcPt = pt) or (ev.locSw = sw and ev.locPt = pt))
}

pred minus_ucTC[st: State, ev: EVprobe, sw: Switch, sw2: Switch] {
	// EMPTY WOULD MEAN *TRUE*
	st != st
}

pred minus_ucST[st: State, ev: EVprobe, sw: Switch, pt: PhysicalPort] {
	// EMPTY WOULD MEAN *TRUE*
	st != st
}


pred transitionFunction [st: State, ev: Event, st': State] {
  // FRAME (BOILERPLATE)
  st'.ucTC = st.ucTC
  	 - { sw: Switch, sw2: Switch | minus_ucTC[st, ev, sw, sw2] } 
     +{ sw: Switch, sw2: Switch | plus_ucTC[st, ev, sw, sw2] }                
  st'.ucST = st.ucST
  	 - { sw: Switch, pt: PhysicalPort | minus_ucST[st, ev, sw, pt] } 
     +{ sw: Switch, pt: PhysicalPort | plus_ucST[st, ev, sw, pt] }      
} 

// TODO: timer, finalization, resets

pred outputPolicy1[st: State, ev: Event, newev: Event] {
// can't just say rrule1 + rrule2
//rrule1[st, ev, newev] or rrule2[st, ev, newev]  
	// EMPTY WOULD MEAN *TRUE*
	st != st

}

////////////////////////////////////////////////////////////////////////////////////////////////
// VERIFICATION
////////////////////////////////////////////////////////////////////////////////////////////////

// Not isSpanningTree(s) and transition(s') -> isSpanningTree(s')
// That would require Alloy's TC, which would prevent FMT use. (Though it'd be easy to write.)
// Instead:
// "Would this single step add a cycle?" (i.e., check for necessary condition to create a cycle)
// "Can this single step ever neglect to add all new switches (s/b only one new)?"


// (2) Assumption about topology:
fact noSelfLoopProbes {
	all pr: EVprobe | pr.srcSw != pr.locSw
}


// For ucTC: can't express "do add when needed" without TC in Alloy
// For ucST: we can. 

// Ahh, the proof isn't lining up because using TC is overcomplicating it.
//  "Add an edge if one of the endpoints isn't in the tree"

// use of ^R in Alloy invalidates bounds for the sorts involved in R. So (for ucTC) we'd be unable to 
// give a bound for Switch, but could give bounds for all others.

// This double-use of TC is the source of almost all the clauses generated.
// Produces a v. dense, ugly problem. 8 sw ~= 5 sec. 9 sw ~= 45 sec.
assert isNonReflexiveReachReal {
	all st: State, st2: State, ev: EVprobe | 
		transitionFunction[st, ev, st2] and (st.ucTC = ^(st.ucTC + ~(st.ucTC)))
		implies				
		st2.ucTC = ^(st.ucTC + (ev.srcSw -> ev.locSw) + (ev.locSw -> ev.srcSw)) 

// Proper TC includes self-loops. Make another relation for reachability if you like...
//                         - { s1, s2: Switch | s1 = s2 }
}

// How is this taking longer? (8 hangs, 7 takes 52 seconds)
// "If I start in a state with ucTC transitively closed, I'll only transition
// to states that are a transitively closed extension by the probe's src/dst."
assert isTCReallyTC {
	all st: State, st2: State, ev: EVprobe | 
		transitionFunction[st, ev, st2]  implies
			(st.ucTC = ^(st.ucTC)) implies
				st2.ucTC = ^(st.ucTC + (ev.srcSw -> ev.locSw)) 
}


// Neglect to say 2 State, 1 Event at your peril.
check isTCReallyTC for 0 but 6 Switch, 2 State, 1 Event,
2 PhysicalPort, 0 MacAddr,
0 IPAddr, 0 EthTyp, 0 NwProtocol,
0 TimerID, 0 EVtimerexpired, 0 EVpacket

// DO add when needed
// If added, added safe
assert isSpanningTree {
	// Inductive step
	all st: State, st2: State, ev: Event | 
		transitionFunction[st, ev, st2] implies			
			// If there is a change
			some (st2.ucST - st.ucST) implies 

			(ev in EVprobe and 
			// is this a prob. for FMT? run through webapp?
             some newsw : (st2.ucST - st.ucST).PhysicalPort |
			 	no st.ucST[newsw]
            	      ) 

   	// Base step is obvious. Not worth expanding universe by 1 more state
}

// All probe events exists switch (field accessor) does not cause a cycle with
// all probe events exists switch (from formula)

// A St, St, Ev |
//    A Sw or
//      exists sw (some newsw: the justification)
//        forall sw  (no st.ucST[newsw]) <---- problem 
//        

check isSpanningTree for 7 but 2 State, 1 Event // "1 Event" is a >50% redux in cars and clauses.
// ??? TODO: calculate bounds via FMT

