// TODOs:

// Proper encoding of instantaneous ch imp predicate
// More events?

// Bounds?
// Other packet fields? Packet types? [mac learning: only eth fields matter]
// Simplify checks if possible

///////////////////////////////////////////////
/*

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

  --- per-rule pieces
  P1R1 and not (P2...)
  or ...

FACT: "dubious use of else with i/o" does not refer to printf. It means that
  other branches lead with *channel* IO.


*/
///////////////////////////////////////////////

#undef VERBOSE
//#define CHIMP_ASSERT

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

#define MAX_STEPS 10

//////////////////////////////////////////////////////
//
// Incoming packets channel. Since we're being conservative, can get away with a single packet here.
chan packets = [1] of {Packet}
chan endsignal = [1] of {byte}

// Controller state: learned relation.
// @@@@@@ ***BOUND***
//chan learned = [255] of {Tuple_learned}
chan learned = [MAX_STEPS+1] of {Tuple_learned}
//chan learned = [6] of {Tuple_learned}
//////////////////////////////////////////////////////

//////////////////////////////////////////////////////
// ONLY used for local scratch space inside atomic statement
hidden Tuple_learned atup;
hidden Packet p;
hidden Packet freshp;
hidden byte x;
//////////////////////////////////////////////////////

proctype Controller() {

do
  :: endsignal??x ->
    goto end;
  ::
    // For performance: imperative that entire controller action is in a single d_step
    // Thus we need no non-determinism here.
    d_step {

    packets?p;
    printf("CONTROLLER: Controller saw: locsw=%d, locpt=%d, dlsrc=%d, dldst=%d\n", p.locsw, p.locpt, p.dlsrc, p.dldst);

    ///////////////////////////
    // Change impact
    ///////////////////////////

    // Is the instantaneous change-impact predicate true in the current state, for the current incoming packet?

    #ifdef CHIMP_ASSERT
      if
        :: learned??[eval(p.locsw), x, eval(p.dlsrc)] && x == 2 ->
              assert(false);
        :: else
      fi;
    #endif




    //////////////////////////////////////
    // State Changes
    //////////////////////////////////////

    // removal
    do
      :: learned??eval(p.locsw),x,eval(p.dlsrc);
         #ifdef VERBOSE
           printf("CONTROLLER: Removing from learned: %d, %d, %d...\n", p.locsw, x, p.dlsrc);
         #endif
      :: !learned??[eval(p.locsw),_,eval(p.dlsrc)] -> break;
    od;

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

  ///////////////////////////////////////////////////////////////////

  } // end d_step

  od; // end core do

  end:
} // end process



/////////////////////

// @@@@@@@ TODO: more than just incoming packets? events as well?

active proctype Network() {
  run Controller();

  // @@@@@@@ Unsafe if constants in program @@@@@@@@@
  byte latest_locsw = 1;
  byte latest_locpt = 1;
  byte latest_dlsrc = 1;
  byte latest_dldst = 1;
  byte counter = 0;

  do
    :: counter >= MAX_STEPS ->
      endsignal!0;
      goto end;

    :: empty(packets) ->
      counter++;

    // Random packet on the network WITHIN BOUNDS
    // @@@@@@@ BOUNDS

    atomic { // true nondeterminism

      // Any location (even from a non-endpoint): being conservative
      // since network protocols are not always perfectly behaved in the real world.

      atomic {
        select(x: 1 .. latest_locsw);
        if
          :: x == latest_locsw -> latest_locsw++;
          :: else
        fi;
        freshp.locsw = x;
      }

      atomic {
        select(x: 1 .. latest_locpt);
        if
          :: x == latest_locpt -> latest_locpt++;
          :: else
        fi;
        freshp.locpt = x;
      }

      // Spoofing is allowed. Any combo of fields is allowed.

      // example use of select. can't use freshp.dlsrc here though
      atomic {
        select(x: 1 .. latest_dlsrc);
        if
          :: x == latest_dlsrc -> latest_dlsrc++;
          :: else
        fi;
        freshp.dlsrc = x;
      }

      atomic {
        select(x: 1 .. latest_dldst);
        if
          :: x == latest_dldst -> latest_dldst++;
          :: else
        fi;
        freshp.dldst = x;
      }

      // TODO: Other fields if needed!

      printf("NETWORK: Sending %d, %d, %d, %d\n", freshp.locsw, freshp.locpt, freshp.dlsrc, freshp.dldst);
      packets!freshp;
    } // end atomic

  // No "else" here; we want to hang until the packets stream is empty
  od;

  end:
}

