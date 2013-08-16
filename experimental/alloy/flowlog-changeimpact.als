module NetCore/MACCIA

// Don't worry about state reachability/comparing traces for now. Just flat change-impact:

// Vastly oversimplifying the structure of packet-headers to 
// keep the model clean. Only using the fields that I need. 
// Also using one instead of lone to simplify

////////////////////////////////////////////////////////////////////////////////////////////////
// TYPES
////////////////////////////////////////////////////////////////////////////////////////////////

sig Event {}

sig Switch {}
sig MacAddr {}
sig IPAddr {}
sig EthTyp {}
sig PhysicalPort {} 
sig NwProtocol {}
sig EVpacket extends Event
                  { locSw: one Switch,
                     locPt: one PhysicalPort,
                     dlSrc: one MacAddr,
                     dlDst: one MacAddr,
                     dlTyp: one EthTyp,
                     nwSrc: one IPAddr,
                     nwDst: one IPAddr,
                     nwProto: one NwProtocol }

sig TimerID {}
sig EVtimerexpired extends Event
                  { id: one TimerID }

// State for MAC learning. Policy is induced from this.
// (Don't make this abstract, or emptyState will be the only valid state.)
sig State { maclearned: Switch -> PhysicalPort -> MacAddr,
                                  switchhasport: Switch -> PhysicalPort }

one sig emptyState extends State {} 
fact emptyIsEmpty {
	(no emptyState.maclearned) and 
	(no emptyState.switchhasport)
}

// Prevent garbage results: extensional equivalence
fact StateExtensional { all st1, st2: State | st1.maclearned = st2.maclearned implies st1 = st2}
fact EVpacketExtensional { all pkt1, pkt2: EVpacket | 
        (pkt1.locSw = pkt2.locSw && pkt1.dlSrc = pkt2.dlSrc && pkt1.dlDst = pkt2.dlDst &&
         pkt1.dlTyp = pkt2.dlTyp && pkt1.nwSrc = pkt2.nwSrc && pkt1.nwDst = pkt2.nwDst &&
         pkt1.nwProto = pkt2.nwProto && pkt1.locPt = pkt2.locPt) implies pkt1=pkt2}
fact EVtimerexpiredExtensional { all ev1, ev2: EVtimerexpired | ev1.id = ev2.id implies ev1 = ev2 } 

////////////////////////////////////////////////////////////////////////////////////////////////
// PROGRAM 1: STATE TRANSITIONS AND POLICY
////////////////////////////////////////////////////////////////////////////////////////////////

// Note that there are TWO transition functions here, to examine how change-impact
// works over state change. In a more basic model, we'd only need one.

// AUTO-GENERATED
pred minusLearned [st: State, ev: Event, sw: Switch, pt: PhysicalPort, mac: MacAddr] {
// !!! Safe?
	ev.locSw = sw && mac = ev.dlSrc

// For checking consistency check
// no st // FOR TEST ONLY: FALSE
}

// AUTO-GENERATED
pred plusLearned [st: State, ev: Event, sw: Switch, pt: PhysicalPort, mac: MacAddr] {
	ev.locSw = sw && ev.locPt = pt && ev.dlSrc = mac
}

pred transitionFunction [st: State, ev: Event, st': State] {
  // FRAME (BOILERPLATE)
  st'.maclearned = st.maclearned
  // ONE LINE FOR EACH STATE RELATION
  	 - { sw: Switch, pt: PhysicalPort, mac: MacAddr | minusLearned[st, ev, sw, pt, mac] } 
     +{ sw: Switch, pt: PhysicalPort, mac: MacAddr |  plusLearned[st, ev, sw, pt, mac] }                
} 

pred outputPolicy1[st: State, ev: Event, newev: Event] {
  // RULE 1
  // If known, send only to that port.
	(
		(ev.locSw -> newev.locPt -> ev.dlDst) in st.maclearned &&
  		newev.locSw = ev.locSw && newev.dlSrc = ev.dlSrc && newev.dlDst = ev.dlDst &&
         newev.dlTyp = ev.dlTyp && newev.nwSrc = ev.nwSrc && newev.nwDst = ev.nwDst &&
         newev.nwProto = ev.nwProto
	)
 	or
  // (OR) RULE 2
  // Otherwise, flood
	(
		(ev.locSw -> PhysicalPort -> ev.dlDst) not in st.maclearned &&
		newev.locPt != ev.locPt && (ev.locSw-> newev.locPt) in st.switchhasport &&
  		newev.locSw = ev.locSw && newev.dlSrc = ev.dlSrc && newev.dlDst = ev.dlDst &&
         newev.dlTyp = ev.dlTyp && newev.nwSrc = ev.nwSrc && newev.nwDst = ev.nwDst &&
         newev.nwProto = ev.nwProto
	)
}


////////////////////////////////////////////////////////////////////////////////////////////////
// PROGRAM 2: STATE TRANSITIONS AND POLICY
////////////////////////////////////////////////////////////////////////////////////////////////

pred transitionFunction2 [st: State, ev: Event, st': State] {
  transitionFunction[st, ev, st'] // no difference
}

pred outputPolicy2[st: State, ev: Event, newev: Event] {
	(
		(ev.locSw -> newev.locPt -> ev.dlDst) in st.maclearned &&
  		newev.locSw = ev.locSw && newev.dlSrc = ev.dlSrc && newev.dlDst = ev.dlDst &&
         newev.dlTyp = ev.dlTyp && newev.nwSrc = ev.nwSrc && newev.nwDst = ev.nwDst &&
         newev.nwProto = ev.nwProto
	)
 	or
	(
		(ev.locSw -> PhysicalPort -> ev.dlDst) not in st.maclearned &&
		newev.locPt != ev.locPt && (ev.locSw -> newev.locPt) in st.switchhasport &&
  		newev.locSw = ev.locSw && newev.dlSrc = ev.dlSrc && newev.dlDst = ev.dlDst &&
         newev.dlTyp = ev.dlTyp && newev.nwSrc = ev.nwSrc && newev.nwDst = ev.nwDst &&
         newev.nwProto = ev.nwProto
	)
    
}

////////////////////////////////
// CHANGE IMPACT
// (1) SSSH is still new b/c state+behavior together
// (2) verification of inductive step in state properties (e.g. consistency)
// (3) verification of inductive step in behavioral properties ???
////////////////////////////////

// Single state, single hop: 
// "On what packets and states will these two policies disagree?"
// This is "standard" Margrave change-impact, with the state predicate(s) as EDBs
// (But more complex because looking for change in new state as well as change in output)
pred changeImpactSSSH[] {
    some ev: Event, st: State | 
		(some newst1, newst2: State | transitionFunction[st, ev, newst1] and 
                                                            transitionFunction2[st, ev, newst2] and
                                                            newst1 != newst2) 
         or
		some outev : Event |     (outputPolicy1[st, ev, outev] and not outputPolicy2[st, ev, outev])
                                            or (outputPolicy2[st, ev, outev] and not outputPolicy1[st, ev, outev])
}

// !!! Very slow re: FOL. Try to rewrite as relational.
// !!! MORE: function from st,ev -> st violates OSEPL.


run changeImpactSSSH for 5

// Question + TODO: can prove *state* invariants "flatly", right? Yes:
// inductive step: assumpt(st1) and transition(st1, st2) implies assumpt(st2)
// base step: assumpt(empty-state)

// so for instance, we may have bad states that send packets bad places, but still be
// able to prove that no such state is reachable via the above (which would show that 
// reachable states were a *subset* of the valid states)

//////////////////////////////////////////////////////////////////////////////////
////// Easy one: consistency:
// (!!! TODO: check OSEPL counts) 

// (to check this, make minusLearned never true)

pred consistentMAC[st: State] {
	all sw: Switch, pt, pt2: PhysicalPort, mac: MacAddr | 
		(sw -> pt -> mac) in st.maclearned && (sw -> pt2 -> mac) in st.maclearned
		implies pt = pt2
}
assert ConsistencyOfMAC {
	// Inductive
	all st: State, st2: State, ev: Event | 
		(consistentMAC[st] && transitionFunction[st, ev, st2]) implies consistentMAC[st2]
    // Base
	consistentMAC[emptyState]		
}
check ConsistencyOfMAC for 4 but 3 State // needs to be 3 (empty + pre + post)
//////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////



//////////////////////////////////////////////////////////////////////////////////

// Question + TODO: topology!
// What happens with this packet as it surges through the network? 
// (Don't need multiple "initial" packets to need traces.
//    Controller state shifts as even one packet moves through, touching multiple switches.)
