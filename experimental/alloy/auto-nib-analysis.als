// Produced automatically by flowlog -alloy at 18:20:4 on 8 20 2013

pred true[] {}
pred false[] { some none }

abstract sig Event {}

sig FLString {} 
sig FLInt{} 

sig Switchid {}
sig Macaddr {}
sig Ipaddr {}
sig Ethtyp {}
sig Portid {} 
sig Nwprotocol {}

sig EVpacket extends Event 
           {locsw: Switchid, locpt: Portid,
            dlsrc: Macaddr+Switchid, dldst: Macaddr+Portid, dltyp: Ethtyp,  /* ADDED UNIONS FOR PROBE: note one switchid, one portid */
            nwsrc: Ipaddr, nwdst: Ipaddr, nwproto: Nwprotocol } 
sig EVstartup  extends Event {}
sig EVswitch_port  extends Event { sw: Switchid, pt: Portid} 
sig EVswitch_down  extends Event { sw: Switchid } 

lone sig C_tnib extends FLString {} /* HAD TO ADD TYPE */
lone sig C_0x1001 extends Ethtyp {} /* HAD TO ADD TYPE */

fact EVpacketExtensional { all ev1, ev2: EVpacket | 
(ev1.locsw = ev2.locsw && ev1.locpt = ev2.locpt && ev1.dlsrc = ev2.dlsrc && ev1.dldst = ev2.dldst && ev1.dltyp = ev2.dltyp && ev1.nwsrc = ev2.nwsrc && ev1.nwdst = ev2.nwdst && ev1.nwproto = ev2.nwproto) implies ev1 = ev2}

fact EVswitch_portExtensional { all ev1, ev2: EVswitch_port | 
(ev1.sw = ev2.sw && ev1.pt = ev2.pt) implies ev1 = ev2}

fact EVswitch_downExtensional { all ev1, ev2: EVswitch_down | 
(ev1.sw = ev2.sw) implies ev1 = ev2}

sig EVstart_timer extends Event {
    seconds: one FLInt, /* HAD TO ADD TYPE */
    id: one FLString} /* HAD TO ADD TYPE */

fact EVstart_timerExtensional { all ev1, ev2: EVstart_timer | 
(ev1.seconds = ev2.seconds && ev1.id = ev2.id) implies ev1 = ev2}

sig EVtimer_expired extends Event {
    id: one FLString} /* HAD TO ADD TYPE */

fact EVtimer_expiredExtensional { all ev1, ev2: EVtimer_expired | 
(ev1.id = ev2.id) implies ev1 = ev2}

sig EVdummy_down_alert extends Event {
    sw1: one Switchid, /* HAD TO ADD TYPE */
    pt1: one Portid, /* HAD TO ADD TYPE */
    sw2: one Switchid, /* HAD TO ADD TYPE */
    pt2: one Portid} /* HAD TO ADD TYPE */

fact EVdummy_down_alertExtensional { all ev1, ev2: EVdummy_down_alert | 
(ev1.sw1 = ev2.sw1 && ev1.pt1 = ev2.pt1 && ev1.sw2 = ev2.sw2 && ev1.pt2 = ev2.pt2) implies ev1 = ev2}

sig State {
    switch_has_port: Switchid -> Portid,
    ucst: Switchid -> Portid -> Switchid -> Portid,
    switchtopology: Switchid -> Portid -> Switchid -> Portid,
    uctree: Switchid -> Portid,
    spanningtree: Switchid -> Portid,
    uctc: Switchid -> Switchid,
    nonswitchports: Switchid -> Portid,
    macconnectedat: Macaddr -> Switchid -> Portid
}
fact StateExtensional { all st1, st2: State |
(st1.switch_has_port = st2.switch_has_port && st1.ucst = st2.ucst && st1.switchtopology = st2.switchtopology && st1.uctree = st2.uctree && st1.spanningtree = st2.spanningtree && st1.uctc = st2.uctc && st1.nonswitchports = st2.nonswitchports && st1.macconnectedat = st2.macconnectedat) implies st1 = st2}

pred dummy_down_alert[st: State, ev: Event, out0 : univ, out1 : univ, out2 : univ, out3 : univ] {

  (ev in EVtimer_expired && ( (((EVtimer_expired <: ev).id = C_tnib && out0->out1->out2->out3 in st.switchtopology) && not (out0->out1->out2->out3 in st.ucst)))
      && true[] && true[] && true[] && true[])
}

pred emit[st: State, ev: Event, out0 : univ] {

  (ev in EVswitch_port && ( ((((out0.locsw = (EVswitch_port <: ev).sw && out0.locpt = (EVswitch_port <: ev).pt) && out0.dltyp = C_0x1001) && out0.dlsrc = (EVswitch_port <: ev).sw) && out0.dldst = (EVswitch_port <: ev).pt))
      && true[]) ||

  (ev in EVtimer_expired && ( (((((EVtimer_expired <: ev).id = C_tnib && out0.locsw->out0.locpt in st.switch_has_port) && out0.dltyp = C_0x1001) && out0.dlsrc = out0.locsw) && out0.dldst = out0.locpt))
      && true[])
}

pred minus_macconnectedat[st: State, ev: Event, out0 : univ, out1 : univ, out2 : univ] {

  (ev in EVpacket && ( ((not ((EVpacket <: ev).dltyp = C_0x1001) && not (out0 = (EVpacket <: ev).dlsrc)) && out0->(EVpacket <: ev).locsw->(EVpacket <: ev).locpt in st.macconnectedat))
      && true[] && out1 = (EVpacket <: ev).locsw && out2 = (EVpacket <: ev).locpt) ||

  (ev in EVpacket && ( ((not ((EVpacket <: ev).dltyp = C_0x1001) && not (out1 = (EVpacket <: ev).locsw)) && (EVpacket <: ev).dlsrc->out1->(EVpacket <: ev).locpt in st.macconnectedat))
      && out0 = (EVpacket <: ev).dlsrc && true[] && out2 = (EVpacket <: ev).locpt) ||

  (ev in EVpacket && ( ((not ((EVpacket <: ev).dltyp = C_0x1001) && not (out2 = (EVpacket <: ev).locpt)) && (EVpacket <: ev).dlsrc->(EVpacket <: ev).locsw->out2 in st.macconnectedat))
      && out0 = (EVpacket <: ev).dlsrc && out1 = (EVpacket <: ev).locsw && true[])
}

pred minus_nonswitchports[st: State, ev: Event, out0 : univ, out1 : univ] {

  (ev in EVtimer_expired && ( ((EVtimer_expired <: ev).id = C_tnib && out0->out1 in st.nonswitchports))
      && true[] && true[])
}

pred minus_spanningtree[st: State, ev: Event, out0 : univ, out1 : univ] {

  (ev in EVtimer_expired && ( ((EVtimer_expired <: ev).id = C_tnib && out0->out1 in st.spanningtree))
      && true[] && true[])
}

pred minus_switchtopology[st: State, ev: Event, out0 : univ, out1 : univ, out2 : univ, out3 : univ] {

  (ev in EVtimer_expired && ( ((EVtimer_expired <: ev).id = C_tnib && out0->out1->out2->out3 in st.switchtopology))
      && true[] && true[] && true[] && true[])
}

pred minus_ucst[st: State, ev: Event, out0 : univ, out1 : univ, out2 : univ, out3 : univ] {

  (ev in EVtimer_expired && ( ((EVtimer_expired <: ev).id = C_tnib && out0->out1->out2->out3 in st.ucst))
      && true[] && true[] && true[] && true[])
}

pred minus_uctc[st: State, ev: Event, out0 : univ, out1 : univ] {

  (ev in EVtimer_expired && ( ((EVtimer_expired <: ev).id = C_tnib && out0->out1 in st.uctc))
      && true[] && true[])
}

pred minus_uctree[st: State, ev: Event, out0 : univ, out1 : univ] {

  (ev in EVtimer_expired && ( ((EVtimer_expired <: ev).id = C_tnib && out0->out1 in st.uctree))
      && true[] && true[])
}

pred plus_macconnectedat[st: State, ev: Event, out0 : univ, out1 : univ, out2 : univ] {

  (ev in EVpacket && ( ((not ((EVpacket <: ev).dltyp = C_0x1001) && 
       (EVpacket <: ev).locsw->(EVpacket <: ev).locpt in st.nonswitchports) 
&&  not ((EVpacket <: ev).dlsrc->(EVpacket <: ev).locsw->(EVpacket <: ev).locpt in st.macconnectedat)
))
      && out0 = (EVpacket <: ev).dlsrc && out1 = (EVpacket <: ev).locsw && out2 = (EVpacket <: ev).locpt)
}

pred plus_nonswitchports[st: State, ev: Event, out0 : univ, out1 : univ] {
/* EDITED: moved E quants under not to work around compiler bug. will fix. */
  (ev in EVtimer_expired && 
	 (((EVtimer_expired <: ev).id = C_tnib && out0->out1 in st.switch_has_port) 
        && not (some someothersw : univ |  some someotherpt : univ | 
				(out0->out1->someothersw->someotherpt in st.ucst)))
      && true[] && true[])
}

pred plus_spanningtree[st: State, ev: Event, out0 : univ, out1 : univ] {

  (ev in EVtimer_expired && ( ((EVtimer_expired <: ev).id = C_tnib && out0->out1 in st.uctree))
      && true[] && true[])
}

pred plus_switch_has_port[st: State, ev: Event, out0 : univ, out1 : univ] {

  (ev in EVswitch_port && ( true[])
      && out0 = (EVswitch_port <: ev).sw && out1 = (EVswitch_port <: ev).pt)
}

pred plus_switchtopology[st: State, ev: Event, out0 : univ, out1 : univ, out2 : univ, out3 : univ] {

  (ev in EVtimer_expired && ( ((EVtimer_expired <: ev).id = C_tnib && out0->out1->out2->out3 in st.ucst))
      && true[] && true[] && true[] && true[]) 
/* ADDED: uncomment to break timer_expired property */
// && false[]
}

pred plus_ucst[st: State, ev: Event, out0 : univ, out1 : univ, out2 : univ, out3 : univ] {

  (ev in EVpacket && ( (EVpacket <: ev).dltyp = C_0x1001)
      && out0 = (EVpacket <: ev).dlsrc && out1 = (EVpacket <: ev).dldst && out2 = (EVpacket <: ev).locsw && out3 = (EVpacket <: ev).locpt)
}

pred plus_uctc[st: State, ev: Event, out0 : univ, out1 : univ] {

  (ev in EVpacket && ( (EVpacket <: ev).dltyp = C_0x1001)
      && out0 = (EVpacket <: ev).dlsrc && out1 = (EVpacket <: ev).locsw) ||

  (ev in EVpacket && ( ((EVpacket <: ev).dltyp = C_0x1001 && out0->(EVpacket <: ev).dlsrc in st.uctc))
      && true[] && out1 = (EVpacket <: ev).locsw) ||

  (ev in EVpacket && ( ((EVpacket <: ev).dltyp = C_0x1001 && (EVpacket <: ev).locsw->out1 in st.uctc))
      && out0 = (EVpacket <: ev).dlsrc && true[]) 

/* added comment: comment out this last rule to cause TC property to fail*/

||
  (ev in EVpacket && ( (((EVpacket <: ev).dltyp = C_0x1001 && out0->(EVpacket <: ev).dlsrc in st.uctc) && (EVpacket <: ev).locsw->out1 in st.uctc))
      && true[] && true[])
}

pred plus_uctree[st: State, ev: Event, out0 : univ, out1 : univ] {

  (ev in EVpacket && ( (((EVpacket <: ev).dltyp = C_0x1001 && not ((EVpacket <: ev).dlsrc->(EVpacket <: ev).locsw in st.uctc)) && not ((EVpacket <: ev).locsw->(EVpacket <: ev).dlsrc in st.uctc)))
      && out0 = (EVpacket <: ev).dlsrc && out1 = (EVpacket <: ev).dldst) ||

  (ev in EVpacket && ( (((EVpacket <: ev).dltyp = C_0x1001 && not ((EVpacket <: ev).dlsrc->(EVpacket <: ev).locsw in st.uctc)) && not ((EVpacket <: ev).locsw->(EVpacket <: ev).dlsrc in st.uctc)))
      && out0 = (EVpacket <: ev).locsw && out1 = (EVpacket <: ev).locpt)
}

pred start_timer[st: State, ev: Event, out0 : univ, out1 : univ] {

  (ev in EVstartup && ( true[])
      && true[] && true[]) ||

  (ev in EVtimer_expired && ( (EVtimer_expired <: ev).id = out1)
      && true[] && true[])
}

pred transition[st1: State, ev: Event, st2: State] { 
  st2.switch_has_port = (st1.switch_has_port
            )
            + { tup0: Switchid,tup1: Portid | plus_switch_has_port[st1, ev, tup0,tup1]} &&

  st2.ucst = (st1.ucst
            - { tup0: Switchid,tup1: Portid,tup2: Switchid,tup3: Portid | minus_ucst[st1, ev, tup0,tup1,tup2,tup3]})
            + { tup0: Switchid,tup1: Portid,tup2: Switchid,tup3: Portid | plus_ucst[st1, ev, tup0,tup1,tup2,tup3]} &&

  st2.switchtopology = (st1.switchtopology
            - { tup0: Switchid,tup1: Portid,tup2: Switchid,tup3: Portid | minus_switchtopology[st1, ev, tup0,tup1,tup2,tup3]})
            + { tup0: Switchid,tup1: Portid,tup2: Switchid,tup3: Portid | plus_switchtopology[st1, ev, tup0,tup1,tup2,tup3]} &&

  st2.uctree = (st1.uctree
            - { tup0: Switchid,tup1: Portid | minus_uctree[st1, ev, tup0,tup1]})
            + { tup0: Switchid,tup1: Portid | plus_uctree[st1, ev, tup0,tup1]} &&

  st2.spanningtree = (st1.spanningtree
            - { tup0: Switchid,tup1: Portid | minus_spanningtree[st1, ev, tup0,tup1]})
            + { tup0: Switchid,tup1: Portid | plus_spanningtree[st1, ev, tup0,tup1]} &&

  st2.uctc = (st1.uctc
            - { tup0: Switchid,tup1: Switchid | minus_uctc[st1, ev, tup0,tup1]})
            + { tup0: Switchid,tup1: Switchid | plus_uctc[st1, ev, tup0,tup1]} &&

  st2.nonswitchports = (st1.nonswitchports
            - { tup0: Switchid,tup1: Portid | minus_nonswitchports[st1, ev, tup0,tup1]})
            + { tup0: Switchid,tup1: Portid | plus_nonswitchports[st1, ev, tup0,tup1]} &&

  st2.macconnectedat = (st1.macconnectedat
            - { tup0: Macaddr,tup1: Switchid,tup2: Portid | minus_macconnectedat[st1, ev, tup0,tup1,tup2]})
            + { tup0: Macaddr,tup1: Switchid,tup2: Portid | plus_macconnectedat[st1, ev, tup0,tup1,tup2]}
}

///////////////////////////////////////////////////////////////////////////////////////////////////
/* ASSERTS AND CHECKS ADDED MANUALLY */
///////////////////////////////////////////////////////////////////////////////////////////////////

/* FACT ADDED: account for lack of subtyping. goes away with subtyping */
fact probesAndNonProbesWellFormed {
	all ev : EVpacket | 
		(ev.dltyp = C_0x1001 implies
			ev.dlsrc in Switchid and ev.dldst in Portid)
		and
		(ev.dltyp != C_0x1001 implies
			ev.dlsrc in Macaddr and ev.dldst in Macaddr)
}

/* ucTC faithfully computes reachability */
assert isTCReallyTC {
	all st: State, st2: State, ev: EVpacket |
		ev.dltyp = C_0x1001 and transition[st, ev, st2] implies
			(st.uctc = ^(st.uctc)) implies
				st2.uctc =
				^(st.uctc + (ev.dlsrc -> ev.locsw))
}
check isTCReallyTC for 0 but 1 Event, 2 State, 6 Switchid, 2 Portid, 2 Ipaddr, 1 Ethtyp, 1 Nwprotocol

/* Timer truly updates ucX -> X */
/* Don't forget to constrain the timer ID or you'll be very confused. */
assert timerWipesNonUC {
	all st: State, st2: State, ev: EVtimer_expired |
		ev.id = C_tnib and transition[st, ev, st2] implies ( 
			st.ucst = st2.switchtopology and 
			st.uctree = st2.spanningtree and 
			st2.nonswitchports in st2.switch_has_port and
			no (st2.nonswitchports & st2.switchtopology.Portid.Switchid)
  )
}
check timerWipesNonUC for 0 but 1 Event, 2 State, 2 Switchid, 2 Portid, 
                                                          2 Ipaddr, 1 Ethtyp, 1 Nwprotocol, 2 Macaddr,
                                                          1 FLString, 1 FLInt


/* macConnectedAt relation is supposed to hold Macaddr -> Switchid -> Portid 
    of non-switch ports macs are seen at */
assert macConnectedAtOK {
	all st: State, st2: State, ev: EVpacket |	
		not ev.dltyp = C_0x1001 and transition[st, ev, st2] implies
		// NOT an inductive property as we defined it
		(ev.locsw -> ev.locpt) in st.nonswitchports implies 
			(ev.dlsrc -> ev.locsw -> ev.locpt) in st2.macconnectedat
		and
		some xpt : Portid | xpt != ev.locpt and (ev.dlsrc -> ev.locsw -> xpt in st.macconnectedat) implies
			not (ev.dlsrc -> ev.locsw -> xpt in st2.macconnectedat)
		and
		some xsw : Switchid | xsw != ev.locsw and (ev.dlsrc -> xsw -> ev.locpt in st.macconnectedat) implies
			not (ev.dlsrc -> xsw -> ev.locpt in st2.macconnectedat)
		and
		some xmac : Macaddr | xmac != ev.dlsrc and (xmac -> ev.locsw -> ev.locpt in st.macconnectedat) implies
			not (xmac -> ev.locsw -> ev.locpt in st2.macconnectedat)				
}

check macConnectedAtOK for 3 but 1 Event, 2 State

pred probesNotSelf[] {
	all ev : EVpacket | 
		(ev.dltyp = C_0x1001 implies
			ev.dlsrc != ev.locsw)
}

assert spanningTreeOK {
	all st: State, st2: State, ev: EVpacket |	
		probesNotSelf[] and ev.dltyp = C_0x1001 and transition[st, ev, st2] implies
             // no change to overall ST
			st2.spanningtree = st.spanningtree 
			// any change is just this probe's edge (and the reverse): this limits growth to at most 1 edge per probe              
			and (st2.uctree = st.uctree || (st2.uctree - st.uctree) in (ev.locsw -> ev.locpt) + (ev.dlsrc -> ev.dldst))
			// only add if one of the switches is not in the spanning tree yet
			and st2.uctree != st.uctree implies (ev.dlsrc not in (st.uctc.Portid) || ev.locsw not in (st.uctc.Portid))
			// [so far, we've proven "tree", since 1 edge per 1 node of growth]		
}

check spanningTreeOK for 4 but 1 Event, 2 State


// TODO: policy pred so we can verify output events!

/////////////////////////////////////////////////////////
// QUESTION: How can we argue that OSEPL applies to the base types here?
// Obviously it applies to event, state. But not so obvious Switchid etc.
// A: Because unless some relation is required to contain something (via existentials; unis in checks)
// the base types are coming from the event and the followup state
// ... still not an automatic check though.  
