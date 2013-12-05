
module cimp

open ontology_Mac_Learning_For_Notables_vs_Mac_Learning as o
open examples/Mac_Learning_For_Notables as prog1
open examples/Mac_Learning as prog2

one sig overall { 
	trace: seq State 
}

// because pigeonhole (this fact gives us an order-of-magnitude solve speedup)
fact noDuplicateStates {
	all s: State | lone overall.trace.indsOf[s] 
}

fact startingState { 
	no overall.trace.first.learned
    no overall.trace.first.switch_has_port
}

fact allStatesInSeq {
	State = overall.trace.elems
}

// Can't say this naively: we may have branching in the last state, due to ch imp.
fact orderRespectsTransitions {
	all i : overall.trace.inds - overall.trace.lastIdx |  
		let s = overall.trace[i] | 
		let nexts =	overall.trace[i+1] |
		let prevs = overall.trace[i-1] |
		// If this state is immediately after a "branching ch imp" state
		// the next state is the branch by prog2.
		(some chev: Event | changeStateTransition[prevs, chev])
         	=> (some ev: Event | prog2/transition[s, ev, nexts]) 
			else (some ev: Event | prog1/transition[s, ev, nexts])
}

fact stopChainAtChange {
	all s: State, ev: Event | changeImpact[s, ev] implies 
		let i = overall.trace.idxOf[s] |
// TODO this is ugly...
			(overall.trace.subseq[i,i].first = overall.trace.last || s = overall.trace.last)
}

// Make sense to divide the two types of change impact,
//  to avoid confusion with what the final state in a trace represents.
pred changeStateTransition[prestate: State, ev: Event]
{
  some newst1, newst2: State |
    (prog1/transition[prestate, ev, newst1] and 
     prog2/transition[prestate, ev, newst2] and 
     newst1 != newst2)
}
pred changePolicyOutput[prestate: State, ev: Event] { 
    some outev: Event | 
      (prog1/outpolicy[prestate, ev, outev] and not prog2/outpolicy[prestate, ev, outev]) ||
      (prog2/outpolicy[prestate, ev, outev] and not prog1/outpolicy[prestate, ev, outev])
}

pred changeImpact[prestate: State, ev: Event] { 
    changeStateTransition[prestate, ev]   
    || changePolicyOutput[prestate, ev]
}
run changeImpact for 6 but 4 State, 5 Event, 4 seq