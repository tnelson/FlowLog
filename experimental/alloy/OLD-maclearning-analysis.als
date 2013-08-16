module maclearninganalysis

open util/ordering [NetworkState]

// Perform analysis of mac-learning (assume auto-compilation to Alloy from our language)

// Use Alloy rather than Margrave here for at least two reasons:
// (a) transitive closure 
// (b) built-in sequences
// Margrave _could_ support these (since it uses Kodkod), but tight on time, and we
// don't even know this is the right analysis paradigm anyway. Prototype!

// ISSUE: the analysis doesn't say that packets MUST exist, beyond bound. 
// So risk of getting spurious results where there is simply no Packet that works
//  (even though we know that one exists platonically) 

// EVEN MORE: because of that, these scenarios may not be great for illustrating
// behavior, ESPECIALLY when flooding is involved. A flood may end up looking like a single pkt, etc.

// Another BIG ISSUE: performance. Surprising number of clauses in this spec, even for a simple
// property. Can it be optimized?

/////////////////////////////////////////
// Import from dynamic policy. If multiple rules, take their disjunction.
// Producing these preds ought to be a simple process.
/////////////////////////////////////////

pred nLearned[st: ControllerState, pkt: LocPacket, sw: Switch, pt: PhysicalPort, mac: MacAddr] {
	sw = pkt.loc.switch 
	mac = pkt.packet.dlsrc	
}

pred pLearned[st: ControllerState, pkt: LocPacket, sw: Switch, pt: PhysicalPort, mac: MacAddr] {
	sw = pkt.loc.switch 
	mac = pkt.packet.dlsrc
	pt = pkt.loc.port	
}

pred fwdRule1[st: ControllerState, pkt: LocPacket, pkt' : LocPacket] {
	// R1 (know which)
	pkt.packet = pkt'.packet and // no modification of header
	pkt.loc.switch = pkt'.loc.switch and // no teleportation
	(pkt.loc.switch -> pkt'.loc.port -> pkt.packet.dldst) in st.learned
}

pred fwdRule2[st: ControllerState, pkt: LocPacket, pkt' : LocPacket] {
	// R2 (flood)
	pkt.packet = pkt'.packet and // no modification of header
	pkt.loc.switch = pkt'.loc.switch and // no teleportation
	not (pkt.loc.port = pkt'.loc.port) and
	not (some sendto : PhysicalPort | (pkt.loc.switch -> sendto -> pkt.packet.dldst) in st.learned)
}

// Emit was ambiguous, forward is a better verb.
pred forward[st: ControllerState, pkt: LocPacket, pkt': LocPacket] {
	fwdRule1[st, pkt, pkt'] or 
	fwdRule2[st, pkt, pkt']
}

pred forceEmit[st: ControllerState, pkt: LocPacket, pkt': LocPacket] {
	// Elements of a pred are conjunctive, so the empty pred is true. Instead:
	some none
}


/////////////////////////////////////////
// These declarations suck in topology info.
/////////////////////////////////////////


one sig Sw1 extends Switch {}
one sig Sw2 extends Switch {}

one sig Mac1 extends MacAddr {}
one sig Mac2 extends MacAddr {}
one sig Mac3 extends MacAddr {}

one sig Port1 extends PhysicalPort {}
one sig Port2 extends PhysicalPort {}
one sig Port3 extends PhysicalPort {}
one sig Port4 extends PhysicalPort {}
one sig Port5 extends PhysicalPort {}

// No host mobility yet
one sig Topology { wires: set (PhysicalPort + MacAddr) -> (PhysicalPort + MacAddr), 
	  					  	    externalPorts: set PhysicalPort,
								switchToPort: set (PhysicalPort -> Switch)}
{
// Represent physical-layer connectivity
// FOR NOW: no mobility of hosts
	wires = (Port1 -> Mac1) + (Mac1 -> Port1) +
			   (Port2 -> Mac2) + (Mac2 -> Port2) +
			   (Port3 -> Port4) + (Port4 -> Port3) +
			   (Port5 -> Mac3) + (Mac3 -> Port5)	

	switchToPort = (Port1 -> Sw1) + (Port2 -> Sw1) +
							(Port3 -> Sw1) + (Port4 -> Sw2) +
							(Port5 -> Sw2)

	externalPorts = wires.MacAddr

	// Constrain switch IDs and ports in non-endpoint locs
	all loc: Loc |
		 (loc.switch not in MacAddr) implies 
 		 loc.port -> loc.switch in switchToPort
}


pred transitionFunction [st: ControllerState, pkt: LocPacket, st': ControllerState] {
	// For each relation in the state, produce a construct like this:
	st'.learned = st.learned
		- {  sw: Switch, pt: PhysicalPort, mac: MacAddr | nLearned[st, pkt, sw, pt, mac] }
		+{  sw: Switch, pt: PhysicalPort, mac: MacAddr | pLearned[st, pkt, sw, pt, mac] }
} 

/////////////////////////////////////////
/////////////////////////////////////////
// END OF POLICY+TOPO SPECIFIC PARTS
/////////////////////////////////////////
/////////////////////////////////////////

// Now for the standard, unchanging defns.

/////////////////////////////////////////
// Network state
// for analysis of behavior under the policy
/////////////////////////////////////////

sig NetworkState {	 cstate : one ControllerState,
							 	packets : set LocPacket } 


fun networkStateTransition[s: one NetworkState] : set NetworkState {
	{ s' : NetworkState | 
				some firstpacket : s.packets |
							transitionFunction[s.cstate, firstpacket, s'.cstate] and
							s'.packets = (s.packets - firstpacket) + 
                                                      packetResultsTopo[s.cstate, firstpacket]  }
} 


//  I can't just write ^networkStateTransition...
// We want to use transitive closure, which means we need a binary relation that
// condenses network state.
one sig Behavior { 
	behavior: set NetworkState -> NetworkState,
	transBehavior: set NetworkState -> NetworkState
}
{
	// Populate a binary relation with the transitions from network-state to network-state.
	behavior = {s, s' : NetworkState | 
									s' in networkStateTransition[s] }
										
	// Now we can have the transitive closure of that relation: reachability!
	transBehavior = ^behavior

// This isn't right though, because it needs to be set of pkts?
// Not needed anyway, right? 
//	all s : ControllerState, pkts: LocPacket | some ns : NetworkState | (ns.cstate == s and ns.packets = pkts)
}


/////////////////////////////////////////
// Constant declarations:
/////////////////////////////////////////

sig LocPacket { packet: one Header,
    			             loc: one Loc }

fact packetAndHeaderAndLocEquality {
	// Packet is fully defined by header and loc:
	all p1, p2 : LocPacket | 
		(p1.loc = p2.loc and p1.packet = p2.packet) implies p1 = p2
	all l1, l2 : Loc |
		(l1.port = l2.port and l1.switch = l2.switch) implies l1 = l2

	// !!! VITAL: CHANGE THIS IF WE ADD NEW FIELDS TO HEADER
	all h1, h2 : Header |
		(h1.dlsrc = h2.dlsrc and h1.dldst = h2.dldst) implies h1 = h2
}

// !!! VITAL: if adding new fields here, change packetAndHeaderAndLocEquality
sig Header { dlsrc: one MacAddr,
                     dldst: one MacAddr}

// Policies only care about switches and switch ports. But for analysis, we need 
// the notion of a packet arriving at its final location. That's represented by a 
// mac address in each field.
sig Loc {
	switch: one (Switch + MacAddr),
     port: one (PhysicalPort + MacAddr)
} {
	(switch in MacAddr iff port in MacAddr)
	(switch in MacAddr implies switch = port)
}

abstract sig PseudoPort {}
abstract sig PhysicalPort extends PseudoPort {} 

abstract sig Switch {}
abstract sig MacAddr {}


// If starting in state [st], reception of [pkt] causes the production of [pkt]
// Note that the result is an OUTGOING packet.
fun packetResults[st: ControllerState, pkt: LocPacket] : set LocPacket {
	{ newpkt : LocPacket | forward[st, pkt, newpkt] or forceEmit[st, pkt, newpkt] }
}

// Account for topology
// Results are INCOMING packets (and thus can be located at a host)
fun packetResultsTopo [st: ControllerState, pkt: LocPacket] : set LocPacket {
	{ newpkt : LocPacket |
       	 some p : packetResults[st, pkt] | (p.loc.port -> newpkt.loc.port) in Topology.wires and
															newpkt.packet = p.packet // no mutation by topology
    }
}



////////////////////////////////////////////////////////////////////////////////////////////////
// STATE TRANSITIONS
////////////////////////////////////////////////////////////////////////////////////////////////

// State for MAC learning 
abstract sig ControllerState { learned: set Switch -> PhysicalPort -> MacAddr }

fact initialNetworkStatePackets {
	// Only one packet in the initial NETWORK state
	one first.packets
	// That packet is freshly received from an end node
	first.packets.loc.port in Topology.externalPorts
}

fact initialNetworkStateCState {
	no first.cstate.learned
}

fact deliveredPacketsStayDelivered {
	// If a packet has been delivered (macaddress in its location) leave it in place across network states
	all ns : NetworkState, pkt : ns.packets | 
		pkt.loc.switch in MacAddr implies 
		pkt in ns.next.packets
}

fact enforceNetworkStateTransitionsInOrdering {
	all ns: NetworkState | ns.next in networkStateTransition[ns]			
}

// All packets that appear on external interfaces have the proper MAC address.
fact noSpoofing {
	all ns: NetworkState, pkt: ns.packets |
		pkt.loc.port in Topology.externalPorts implies
		pkt.packet.dlsrc = Topology.wires.(pkt.loc.port)
}

// DO NOT TURN ON THIS FACT:
// We need "helper" LocPackets to be the result of policy that are then translated by the topo.
// Networkstate only has packets after topo applies.
//fact noUnusedPackets {
//	LocPacket in NetworkState.packets	
//}

fact noUnusedControllerStates {
	ControllerState in NetworkState.cstate	
}

	// always at least one packet for the result.
	// This is INSUFFICIENT to faithfully model floods.
	// and in fact it breaks the ability of switches to DROP packets!
	//all s: ControllerState, p : LocPacket | 
	//	(not p.loc.switch in MacAddr) implies 
	//	(some p' : LocPacket | p' in packetResults[s, p])

////////////////////////////////////////

// NOT CORRECT! (also, no need to reference nw state here?)
// Correct version needs to account for the fact that we're also learning
// INTERNAL routes, so we may very well learn a port/mac pair that
// isn't an endpoint.
/*assert noWrongEndpointsLearned {
	all ns1, ns2: NetworkState | 
		// One hop away, didn't learn any unusual connections
		(ns2 in networkStateTransition[ns1] and
         	ns1.cstate.learned[Switch] in Topology.wires) implies
		 ns2.cstate.learned[Switch] in Topology.wires
}*/


// Progressive attempt to verify preservation of connectivity:

// TODO: is there a better way to do this filtering in Alloy? 
assert noWrongEndpointsLearned {
	all ns1, ns2: NetworkState | 
		// One hop away, didn't learn any unusual connections

		(ns2 in networkStateTransition[ns1] and
         	{ x: PhysicalPort, y: MacAddr | x->y in ns1.cstate.learned[Switch] and x in Topology.externalPorts } in Topology.wires) implies
		 { x: PhysicalPort, y: MacAddr | x->y in ns2.cstate.learned[Switch] and x in Topology.externalPorts } in Topology.wires
}

// This works (and faster, though still ugly filtering) given the empty-initial-cstate fact above.
assert noWrongEndpointsLearnedStronger {
	all ns: NetworkState | 		
		 { x: PhysicalPort, y: MacAddr | 
			x->y in ns.cstate.learned[Switch] and 
		    x in Topology.externalPorts }
		 in Topology.wires
}

// Test behavior and ordering: want this to be unsatisfiable.
assert noLoops {
	// Only one originating packet in entire scenario 
	all ns:  NetworkState | ns != first implies no (ns.packets.loc.port & Topology.externalPorts)
	
	implies

	// Should never see same switch twice in the trace (which is over ALL network states)
	// TODO: must be a way to write this relationally...
	// Note: NOT SAFE to make them adjacent. Need to be arbitrary states in the trace
	all ns1, ns2: NetworkState | ns1 != ns2 implies 
				no (ns1.packets.loc.switch & (ns2.packets - ns1.packets).loc.switch)
}
// ^^^ Doesn't work yet, because only ONE packet in set is handled at a time.
// ^^^ Also odd mutation of packet fields... missing a constraint?

// ^^^ Retest this with changes. Relies on the fact that only one packet is being handled at a time, and tht
// handling of packets is never idempotent except when all pakcets have been delivered

// TODO ^^^ If this works, it should **FAIL** on a triangle network. Test that. Test it now!!

check noLoops
	for 8 but 4 NetworkState, 4 ControllerState

run {}
	for 8 but 3 NetworkState, 3 ControllerState

// This is obvious, because the first packet always gets flooded!
assert preservationOfConnectivityForFirstPacketEver {
	all locpkt : first.packets |
		some endstate : NetworkState, endpkt : endstate.packets | 
			(endpkt.loc.port = locpkt.packet.dldst)										
}

// We want something more subtle, that bounds complicate. We can't say
// "for all packets, exists a state in which..." because that packet might get introduced
// at the end of the sequence!
/*assert preservationOfConnectivityForFirstPacketEver {
	all locpkt : first.packets |
		some endstate : NetworkState, endpkt : endstate.packets | 
			(endstate.loc.port = locpkt.packet.dldst)										
}*/

// The algorithm converges: without mobility of hosts, we eventually stop changing
// the controller state.
assert convergenceWithoutMobility {

}

assert eventuallyAllArrive {

}

check preservationOfConnectivityForFirstPacketEver
	for 8 but 4 NetworkState, 4 ControllerState

// Expect this property to hold
check noWrongEndpointsLearnedStronger
	for 8 but 3 NetworkState, 3 ControllerState
