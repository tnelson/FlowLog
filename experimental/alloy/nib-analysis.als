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
	// (sw1, pt1) connected to (sw2, pt2) in the tree.
	ucST: Switch -> Switch -> PhysicalPort -> PhysicalPort,

	// True transitive-closure (no forced symmetry)
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

// MODIFIED! stopped using TC, and now complete path in one edge
// !!! This actually won't work. ;) Because we aren't doing B's alg. We have no
// luxury of saying "the spanning TREE". We're stuck building a forest. Need TC!
pred plus_ucST[st: State, ev: EVprobe, sw: Switch, sw2: Switch, pt: PhysicalPort, pt2: PhysicalPort] {
// WILL WORK
//	not ev.srcSw->ev.locSw in st.ucTC and
// BROKEN
	(no st.ucST[ev.srcSw] and ev.srcSw = sw and ev.srcPt = pt and ev.locSw = sw2 and ev.locPt = pt2)
	or (no st.ucST[ev.locSw] and ev.locSw = sw and ev.locPt = pt and ev.srcSw = sw2 and ev.srcPt = pt2)
	
// rule blow-up if want to be fully symmetric
}

pred minus_ucTC[st: State, ev: EVprobe, sw: Switch, sw2: Switch] {
	// EMPTY WOULD MEAN *TRUE*
	st != st
}

pred minus_ucST[st: State, ev: EVprobe,  sw: Switch, sw2: Switch, pt: PhysicalPort, pt2: PhysicalPort] {
	// EMPTY WOULD MEAN *TRUE*
	st != st
}


pred transitionFunction [st: State, ev: Event, st': State] {
  // FRAME (BOILERPLATE)
  st'.ucTC = st.ucTC
  	 - { sw: Switch, sw2: Switch | minus_ucTC[st, ev, sw, sw2] } 
     +{ sw: Switch, sw2: Switch | plus_ucTC[st, ev, sw, sw2] }                
  st'.ucST = st.ucST
  	 - { sw: Switch, sw2: Switch, pt: PhysicalPort, pt2: PhysicalPort | minus_ucST[st, ev, sw, sw2, pt, pt2] } 
     +{ sw: Switch, sw2: Switch, pt: PhysicalPort, pt2: PhysicalPort | plus_ucST[st, ev, sw, sw2, pt, pt2] }      
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

///////////////////////////


// Preserve the two following conditions across transitions:
// 1: The TC of the tree is ucTC. (maximally spanning, discon when necessary)
// 2: TC(tree - any edge) != ucTC (tree: all edges are cut edgess)
// note that we cannot say "#edges = #nodes - 1" because we're building 
// a forest. it'd be "#edges <= #nodes - 1" 

pred isLoopFreeUCTC[st: State] {
	^(st.ucST.PhysicalPort.PhysicalPort) - {n1, n2: Switch | n1=n2} 
    = 
	st.ucTC - {n1, n2: Switch | n1=n2}
}

pred allEdgesCuts[st: State] {
	all sw1, sw2: Switch | sw1 -> sw2 in st.ucST.PhysicalPort.PhysicalPort 
    	implies ^(st.ucST.PhysicalPort.PhysicalPort - (sw1 -> sw2))
                   != st.ucTC - {n1, n2: Switch | n1=n2}
// DO NOT try {n,n : Switch}. Shadowing results in full product
}

pred wellFormedST[st: State] {
	// Exclude bad starting span trees, like [0, 0,x,x] [1,1,y,y]
	all sw : Switch | not (sw->sw) in st.ucST.PhysicalPort.PhysicalPort
}

assert isSpanningTreeUsesTC {
	all st: State, st2: State, ev: EVprobe | 
		transitionFunction[st, ev, st2] and wellFormedST[st] and isLoopFreeUCTC[st] and allEdgesCuts[st] implies			
        	wellFormedST[st2] and isLoopFreeUCTC[st2] and allEdgesCuts[st2]
}
check isSpanningTreeUsesTC for 3 but 2 State, 1 Event, 4 Switch



// The domain restriction of e1 to e2 contains all tuples in e1 that 
// start with an element in the set e2. e1 <: e2. Range restriction 
// e1 :> e2 uses ENDS, not start.

// Base step is obvious. Not worth expanding universe by 1 more state
