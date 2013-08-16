module NetCore/MACCIA

// This module is about change-impact over mac-learning. We have transition-functions,
// so proper exploration of change-impact may require >1 such function. We want scenarios
// that can have traces, so we need ordering. Since we can multiple traces being compared,
// we need multiple orderings:
open util/ordering [StateF1] as ord1
open util/ordering [StateF2] as ord2

// TN: Attempting to understand how change impact works in this domain
//  and also how it connects with a stateful idiom like mac learning.

// Arjun's reframe didn't include modification, 
// but that seems critical, so added it in similar-to-Haskell way.

// This model uses a lot of set-comprehension. Can be slow?

// Note: I'm worried that MAC-learn and NAT will be easy to model, but
// rate-limiting will not be, due to high integers.
/////////////////////////////////////////////////////////////////////////////////////////////////

// Vastly oversimplifying the structure of packet-headers to 
// keep the model clean. Only using the fields that I need. 
// Also using one instead of lone to simplify

sig Switch {}
sig MacAddr {}
sig Packet { srcMAC: one MacAddr,
                     destMAC: one MacAddr}
sig Loc { switch: one Switch,
               port: one PhysicalPort}

// Removed AllPorts -- want to be able to say "broadcast EXCEPT on incoming port"?
// Equivalent is to leave port unconstrained in fwd action
// TODO: Discuss this drop w/ AG
sig PhysicalPort {} 

abstract sig Action {}
sig Forward extends Action {to: some PhysicalPort, 
                                               modified: one Packet}
// Note: codebase implies that this can't modify before sending?
one sig Controller extends Action {} 

sig LocPacket { packet: one Packet,
                          loc: one Loc }

////////////////////////////////////////////////////////////////////////////////////////////////
// STATE TRANSITIONS
////////////////////////////////////////////////////////////////////////////////////////////////

// State for MAC learning. Policy is induced from this.
abstract sig State { maclearned: Switch -> PhysicalPort -> MacAddr}
// Note "in" not "extends": allows overlap, which is vital.
sig StateF1 in State {}
sig StateF2 in State {}

// Note that there are TWO transition functions here, to examine how change-impact
// works over state change. In a more basic model, we'd only need one.

pred transitionFunction [st: State, pkt: LocPacket, st': State] {
  // FRAME
  st'.maclearned = st.maclearned
  // MAC-LEARNING
              - (pkt.loc.switch -> PhysicalPort -> pkt.packet.srcMAC)
              +(pkt.loc.switch -> pkt.loc.port -> pkt.packet.srcMAC)
} 

pred transitionFunction2 [st: State, pkt: LocPacket, st': State] {
  transitionFunction[st, pkt, st'] // no difference
}

// One unique initial state, and the orderings start there.
fact InitialState {
  no ord1/first.maclearned
  no ord2/first.maclearned
  one { st: State | no st.maclearned }
}

// We need this, lest unbounded-universals eat our results. Even more: can mess up change-impact if not.
fact tracesSameLength {
	#ord1/next == #ord2/next
}

// Connect the states. Notice that this formalization allows
// Packets to be "re-used", in that they represent the logical
// packet header, not actual instances of packets in the multiset sense.
fact EnforceTransitions {
  // Grouped together because need to match trigger packets. But beware unbounded-universals:
  all st: State, st1': st.ord1/next, st2': st.ord2/next {
	some samepkt: LocPacket | transitionFunction[st, samepkt, st1'] and 
                                        	   transitionFunction2[st, samepkt, st2']
  }
}


////////////////////////////////////////////////////////////////////////////////////////////////
// PACKET-HANDLING POLICY
////////////////////////////////////////////////////////////////////////////////////////////////

// Helper functions that generate policy from state
// (use policyAtState to produce the top-level policy) 

fun routePolicy(st: State): (LocPacket -> Action) {
  // If known, send only to that port.
  { p : LocPacket, act: Forward |  (p.loc.switch -> act.to -> p.packet.destMAC) in st.maclearned &&
                                                         act.modified == p.packet }
  // Otherwise, flood
  +
  { p : LocPacket, act: Forward |  (p.loc.switch -> PhysicalPort -> p.packet.destMAC) not in st.maclearned &&
                                                         act.modified == p.packet //&&
                                                         //act.to == AllPorts 
  }
}

fun routePolicy2(st: State): (LocPacket -> Action) {
// Intending to not use AllPorts, but instead all ports but arrival port.
// But there is a bug: if the learned MAC really does point back to the same port, traffic 
//   will be dropped entirely!
  { p: LocPacket, act: Forward | (p -> act) in routePolicy[st] 
                                                and act.to != p.loc.port} 

}


// Note the VITAL use of negation here! Need to be able to say:
// "If I have not learned this, keep trying to learn it."
fun learnPolicy(st: State): (LocPacket -> Action) {
  { p : LocPacket | (p.loc.switch -> p.loc.port -> p.packet.srcMAC) not in st.maclearned }-> Controller
}

fun policyAtState(st: State): (LocPacket -> Action) {
  routePolicy[st] + learnPolicy[st]
}
fun policyAtState2(st: State): (LocPacket -> Action) {
  routePolicy2[st] + learnPolicy[st]
}



////////////////////////////////
////////////////////////////////
// CHANGE IMPACT

// Question: Is single-packet CIA valuable?
// Question: What complications/advantages from trace CIA?

// Question: What is the other "policy" (read: program) to compare in a trace situation?
// (This is what led to the different-kinds-of-CIA question.)
// Since we are change-impact-analyzing a program, 
//   the changes can be in the packet handling or in the state changes.

// Question + TODO: topology!
// What happens with this packet as it surges through the network? 
// (Don't need multiple "initial" packets to need traces.
//    Controller state shifts as even one packet moves through, touching multiple switches.)


// Single state, single hop: 
// "On what packets and states will these two policies disagree?"
// This is "standard" Margrave change-impact, with the state predicate(s) as EDBs
pred changeImpactSSSH[pkt: LocPacket, st: State] {
	some act: Action | (pkt ->act) in ((policyAtState[st] + policyAtState2[st]) - 
                                                      (policyAtState[st] & policyAtState2[st]))
}

// Trace, single hop:
// "On what traces will these two policies disagree re: the final step?"
// Final step may be packet disposition or state change.
pred changeImpactTRSH[] {
	// Case 1: packet disposition changed between policies on LAST STATE
	(some pkt : LocPacket | changeImpactSSSH[pkt, ord1/last]) 
	or
	// Case 2: state transition changed at very end:
	let st1 = ord1/last, st2 = ord2/last | (st1 != st2) 

// ??? Is it safe to use "some" to extract "a" packet between states? 
// ??? We've already used it above in the axioms. Risk we could get 2 different packets here? (does it even matter?)

}

/////////////////////
//run { #State > 1} for 5 but 2 State
//run { #State > 1 and some pkt: LocPacket, st: State | changeImpactSSSH[pkt, st] } for 5 but 2 State
run { #State > 1 and changeImpactTRSH[] } for 5 but 2 State
