// We know that TC cannot be expressed in FOL, unless the resulting TC
// is loop-free (which means a base R that's a directed tree).

// But with the Dedalus construction / Flowlog, we can do so safely.

open util/ordering[Time]

sig Node {
	succ: set Node
} 

sig Time {	    
    tc: set (Node->Node)
}

// First time should just have TC = edges
// Successive iterations will add to TC for the next state.
pred NaiveTC {
	all t: Time | 
		all n1, n2: Node | 
			((n1->n2) in t.tc) iff 

	         ((n1->n2) in succ or
           	  some nmid : Node | (n1->nmid) in succ and (nmid->n2) in (t.prev).tc) 
}

// Last state's TC is the real TC
assert Test { NaiveTC[] implies last.tc = ^succ }
// check Test but 
check Test for 6 but 6 Time

