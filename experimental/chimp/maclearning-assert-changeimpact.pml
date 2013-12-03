
#define VERBOSE

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
// ***BOUND***
chan learned = [9] of {Tuple_learned}
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

    #ifdef VERBOSE
      for(atup in learned) {
        printf("CONTROLLER pre-state: learned(%d %d %d).\n", atup.sw, atup.pt, atup.mac);
      }
    #endif

    //////////////////////////////////////
    // Packet Forwarding
    //////////////////////////////////////

    // Incoming packet: p. Push new packet onto queue when appropriate.
    // EACH RULE SEPARATE 
    // Experiment with send. We can use scratch variables for free (see below) using hidden keyword.
    // Here I experiment with giving field list instead of a Tuple_learned.

// RULE 1

/*
  d_step {
    for(atup in learned) {
      if  // 3sw changed
        :: p.locsw == atup.sw && p.dldst == atup.mac ->
          //printf("CONTROLLER: KNOWN port: %d.\n", atup.pt);
          if :: connections[atup.pt] >= FINAL || connections[atup.pt] == NOCONN -> 
                delivered!0,connections[atup.pt],p.dlsrc,p.dldst;
          :: connections[atup.pt] >= 1 && connections[atup.pt] <= 3 ->
             packets!1,connections[atup.pt],p.dlsrc,p.dldst;
             //printf("CONTROLLER: successfully output <%d, %d, %d, %d>.\n", 1, connections[atup.pt], p.dlsrc, p.dldst);
          :: connections[atup.pt] >= 4 && connections[atup.pt] <= 6 ->
             packets!2,connections[atup.pt],p.dlsrc,p.dldst;
             //printf("CONTROLLER: successfully output <%d, %d, %d, %d>.\n", 2, connections[atup.pt], p.dlsrc, p.dldst);
          :: connections[atup.pt] >= 7 && connections[atup.pt] <= 9 ->
             packets!3,connections[atup.pt],p.dlsrc,p.dldst;
             //printf("CONTROLLER: successfully output <%d, %d, %d, %d>.\n", 3, connections[atup.pt], p.dlsrc, p.dldst);
          fi;
        :: else // do nothing, not a match
      fi;
    }
  }
*/

/*
    // RULE 2
    // Unknown. (Remember: in general both rules can apply.
    // So keep separate even if they are disjoint in THIS program.)
    if
       :: !learned??[eval(p.locsw), _, eval(p.dldst)] ->


        // d_step for rule 2: no REAL non-determinism here. 

          d_step {

          newp.dlsrc = p.dlsrc;
          newp.dldst = p.dldst;
         // printf("CONTROLLER: Beginning to flood.\n");
          flooded = 1;
          if
          :: p.locsw == 1 -> 
             //printf("CONTROLLER: On switch 1\n");
             for(ipt : 1..3) {
               //printf("CONTROLLER: checking port %d, conn=%d\n", ipt, connections[ipt]);
               if 
                 :: (connections[ipt] >= FINAL || connections[ipt] == NOCONN) && p.locpt != ipt -> 
                    printf("Delivering! \n");
                    delivered!0,connections[ipt],newp.dlsrc,newp.dldst;
                    //printf("Delivered to %d\n", connections[ipt]);
                 :: connections[ipt] != NOCONN && connections[ipt] < FINAL && p.locpt != ipt -> 
                    newp.locpt = connections[ipt]; 
		    //printf("Outputting on new port %d\n", newp.locpt);
                    if 
                      :: newp.locpt >= 1 && newp.locpt <= 3 -> newp.locsw = 1; 
                      :: newp.locpt >= 4 && newp.locpt <= 6 -> newp.locsw = 2; 
                      :: newp.locpt >= 7 && newp.locpt <= 9 -> newp.locsw = 3; 
                    fi;

                    packets!newp;
                    //printf("Output <%d,%d,%d,%d>\n", newp.locsw, newp.locpt, newp.dlsrc, newp.dldst);
                 :: else // -> printf("else\n"); skip;
               fi;    
             } // end for
          :: p.locsw == 2 -> 
             //printf("CONTROLLER: On switch 2\n");
             for(ipt : 4..6) {
               //printf("CONTROLLER: checking port %d, conn=%d\n", ipt, connections[ipt]);
               if 
                 :: (connections[ipt] >= FINAL || connections[ipt] == NOCONN) && p.locpt != ipt -> 
                    delivered!0,connections[ipt],newp.dlsrc,newp.dldst;
                   // printf("Delivered to %d\n", connections[ipt]);
                 :: connections[ipt] != NOCONN && connections[ipt] < FINAL && p.locpt != ipt -> 
                    newp.locpt = connections[ipt]; 
		   // printf("Outputting on new port %d\n", newp.locpt);
                    if 
                      :: newp.locpt >= 1 && newp.locpt <= 3 -> newp.locsw = 1; 
                      :: newp.locpt >= 4 && newp.locpt <= 6 -> newp.locsw = 2; 
                      :: newp.locpt >= 7 && newp.locpt <= 9 -> newp.locsw = 3; 
                    fi;

		   // printf("Outputting... <%d,%d,%d,%d>\n", newp.locsw, newp.locpt, newp.dlsrc, newp.dldst);
                    packets!newp;
                   // printf("Output <%d,%d,%d,%d>\n", newp.locsw, newp.locpt, newp.dlsrc, newp.dldst);
                 :: else // -> printf("else\n"); skip;
               fi;   
             } // end for
 
          :: p.locsw == 3 -> 
             printf("CONTROLLER: On switch 3\n");
             for(ipt : 7..9) {
               //printf("CONTROLLER: checking port %d, conn=%d\n", ipt, connections[ipt]);
               if 
                 :: (connections[ipt] >= FINAL || connections[ipt] == NOCONN) && p.locpt != ipt -> 
                    delivered!0,connections[ipt],newp.dlsrc,newp.dldst;
                    //printf("Delivered to %d\n", connections[ipt]);
                 :: connections[ipt] != NOCONN && connections[ipt] < FINAL && p.locpt != ipt -> 
                    newp.locpt = connections[ipt]; 
                    if 
                      :: newp.locpt >= 1 && newp.locpt <= 3 -> newp.locsw = 1; 
                      :: newp.locpt >= 4 && newp.locpt <= 6 -> newp.locsw = 2; 
                      :: newp.locpt >= 7 && newp.locpt <= 9 -> newp.locsw = 3; 
                    fi;
                    packets!newp;
                   // printf("Output <%d,%d,%d,%d>\n", newp.locsw, newp.locpt, newp.dlsrc, newp.dldst);
                 :: else
               fi;    
             } // end for

          :: else -> printf("error in switch number\n"); // never get here
        fi;
       // printf("CONTROLLER: Flooding complete.\n");

} // end d_step for flood

      :: else // already learned, don't apply the rule
      fi;
*/

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
        
        //#ifdef CHECK_ASSERT_1
        //if
        //  :: learned??[eval(p.locsw), _, eval(p.dlsrc)] ->  
        //     assert(false);
        //  :: else
        //fi;
        //#endif

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

    } // end atomic

  od;
}

active proctype Network() {
  run Controller();

  try_again:
  do 
    :: empty(packets) ->
   
    // Random packet on the network WITHIN BOUNDS    

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

