// TODOs:

// Proper encoding of instantaneous ch imp predicate
// More events?

// Bounds?
// Other packet fields? Packet types? [mac learning: only eth fields matter]
// Simplify checks if possible

///////////////////////////////////////////////

Strategy: use only one state-transition relation, but
  check with asserts that (a/the/some) instantaneous change-impact 
  predicate is never satisfied. Divergance won't occur unless that
  assert is violated.

Caveat: bounds on types. Macs = 1, 2, 3, ... etc.
  This is NOT the same as "depends on a topology".
  For change-impact, we do not worry about the connections.
  Want a conservative test that resists corner cases like 
  packets already in transit and concurrency in the runtime.

Presentation: 
  (1) Instantaneous change-impact. If equivalent, done.
  (2) If inequivalent, scenarios may be expected. 
  (3) If unexpected, check if reachable using this method.

  ??? Does that mean the predicate we're !asserting is a scenario, the full
      instantaneous predicate, or what?

///////////////////////////////////////////////

#undef VERBOSE
#define CHIMP_ASSERT

typedef Packet {
  byte locsw;
  byte locpt;
  byte dlsrc;
  byte dldst;
}

typedef Tuple_learned {
  byte sw;
  byte pt;
  byte mac;
}

//////////////////////////////////////////////////////
//
// Incoming packets channel. Since we're being conservative, can get away with a single packet here.
chan packets = [1] of {Packet}

// Controller state: learned relation. 
// @@@@@@ ***BOUND***
chan learned = [12] of {Tuple_learned}
//////////////////////////////////////////////////////

//////////////////////////////////////////////////////
// ONLY used for local scratch space inside atomic statement
hidden Packet newp;
hidden Tuple_learned atup;
hidden Packet p;
hidden Packet freshp;
hidden byte ipt;
hidden byte imac;
//////////////////////////////////////////////////////

proctype Controller() {

do
  :: 
    atomic {
    
    packets?p;
    printf("CONTROLLER: Controller saw: locsw=%d, locpt=%d, dlsrc=%d, dldst=%d\n", p.locsw, p.locpt, p.dlsrc, p.dldst);

    ///////////////////////////
    // Change impact   
    ///////////////////////////
  
    // Is the instantaneous change-impact predicate true in the current state, for the current incoming packet?

    #ifdef CHIMP_ASSERT
    if
      :: learned??[eval(p.locsw), _, eval(p.dlsrc)] ->  
            assert(false);
      :: else
    fi;
    #endif


    //////////////////////////////////////
    // State Changes
    //////////////////////////////////////

  d_step {
    // For everything in the set, should it be removed?
    for(atup in learned)
    {
      if // MINUS RULE (1)   
        // @@@@@@@@ TODO: is this check needed? why can't we do learned??eval(atup.sw), ... etc?
        :: (atup.sw == p.locsw && atup.mac == p.dlsrc && atup.pt != p.locpt) ->
           #ifdef VERBOSE 
             printf("CONTROLLER: Removing from learned: %d, %d, %d...\n", atup.sw, atup.pt, atup.mac);
           #endif
           learned??eval(atup.sw),eval(atup.pt),eval(atup.mac); 
        :: else
      fi;
    }

    // Adding new things to set.
    if
      :: !learned??[eval(p.locsw), eval(p.locpt), eval(p.dlsrc)] ->
              
        atup.sw = p.locsw;
        atup.pt = p.locpt;
        atup.mac = p.dlsrc;
        learned!atup;   
        
        #ifdef VERBOSE
          printf("CONTROLLER: Adding to learned: %d, %d, %d.\n", atup.sw, atup.pt, atup.mac);
        #endif

      :: else 
    fi;
  }

  ///////////////////////////////////////////////////////////////////

  } // end atomic within this branch (the only branch) of the do.
  od; // end core do
} // end process



/////////////////////

// @@@@@@@ TODO: more than just incoming packets? events as well?

active proctype Network() {
  run Controller();

  try_again:
  do 
    :: empty(packets) ->
   
    // Random packet on the network WITHIN BOUNDS    
    // @@@@@@@ BOUNDS

    atomic { // true nondeterminism
      
      // Any location (even from a non-endpoint): being conservative
      // since network protocols are not always perfectly behaved in the real world.

      if
        :: freshp.locsw = 1; freshp.locpt = 1;
        :: freshp.locsw = 1; freshp.locpt = 2;
        :: freshp.locsw = 1; freshp.locpt = 3;
        :: freshp.locsw = 2; freshp.locpt = 4;
        :: freshp.locsw = 2; freshp.locpt = 5;
        :: freshp.locsw = 2; freshp.locpt = 6;
        :: freshp.locsw = 3; freshp.locpt = 7;
        :: freshp.locsw = 3; freshp.locpt = 8;
        :: freshp.locsw = 3; freshp.locpt = 9;        
      fi;

      // Spoofing is allowed. Any combo of fields is allowed.

      if 
       :: freshp.dlsrc = 1
       :: freshp.dlsrc = 2
       :: freshp.dlsrc = 3
       :: freshp.dlsrc = 100
      fi;

      if 
       :: freshp.dldst = 1
       :: freshp.dldst = 2
       :: freshp.dldst = 3
       :: freshp.dldst = 100
      fi;

      // TODO: Other fields if needed!
      
      printf("NETWORK: Sending %d, %d, %d, %d\n", freshp.locsw, freshp.locpt, freshp.dlsrc, freshp.dldst);
      packets!freshp; 
    } // end atomic 

  // No "else" here; we want to hang until the packets stream is empty
  od;
}

