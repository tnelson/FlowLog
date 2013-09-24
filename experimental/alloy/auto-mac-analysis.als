// Produced automatically by flowlog -alloy at 13:51:23 on 8 24 2013

pred true[] {}
pred false[] { some none }

abstract sig Event {}

sig Switchid {}
sig Macaddr {}
sig Ipaddr {}
sig Ethtyp {}
sig Portid {} 
sig Nwprotocol {}

sig FLString {} 
sig FLInt{} 

sig EVpacket extends Event
           {locsw: Switchid, locpt: Portid,
            dlsrc: Macaddr, dldst: Macaddr, dltyp: Ethtyp,
            nwsrc: Ipaddr, nwdst: Ipaddr, nwproto: Nwprotocol }
sig EVstartup extends Event {}
sig EVswitch_port extends Event { sw: Switchid, pt: Portid}
sig EVswitch_down extends Event { sw: Switchid }


fact EVpacketExtensional { all ev1, ev2: EVpacket | 
(ev1.locsw = ev2.locsw && ev1.locpt = ev2.locpt && ev1.dlsrc = ev2.dlsrc && ev1.dldst = ev2.dldst && ev1.dltyp = ev2.dltyp && ev1.nwsrc = ev2.nwsrc && ev1.nwdst = ev2.nwdst && ev1.nwproto = ev2.nwproto) implies ev1 = ev2}

fact EVswitch_portExtensional { all ev1, ev2: EVswitch_port | 
(ev1.sw = ev2.sw && ev1.pt = ev2.pt) implies ev1 = ev2}

fact EVswitch_downExtensional { all ev1, ev2: EVswitch_down | 
(ev1.sw = ev2.sw) implies ev1 = ev2}

sig State {
    learned: set (Switchid -> Portid -> Macaddr),
    switch_has_port: set (Switchid -> Portid)
}
fact StateExtensional { all st1, st2: State |
(st1.learned = st2.learned && st1.switch_has_port = st2.switch_has_port) implies st1 = st2}

pred forward[st: State, ev: Event, out0 : univ] {

  (ev in EVpacket && (some x : univ |  ((EVpacket <: ev).locsw->out0.locpt->(EVpacket <: ev).dldst in st.learned && not ((EVpacket <: ev).locpt = out0.locpt)) || ((not ((EVpacket <: ev).locsw->x->(EVpacket <: ev).dldst in st.learned) && not ((EVpacket <: ev).locpt = out0.locpt)) && (EVpacket <: ev).locsw->out0.locpt in st.switch_has_port))
      && true[])
}

pred minus_learned[st: State, ev: Event, out0 : univ, out1 : univ, out2 : univ] {

  (ev in EVpacket && ( (not (out1 = (EVpacket <: ev).locpt) && not ((EVpacket <: ev).locsw->(EVpacket <: ev).locpt->(EVpacket <: ev).dlsrc in st.learned)))
      && out0 = (EVpacket <: ev).locsw && true[] && out2 = (EVpacket <: ev).dlsrc) ||

  (ev in EVswitch_down && ( true[])
      && out0 = (EVswitch_down <: ev).sw && true[] && true[])
}

pred minus_switch_has_port[st: State, ev: Event, out0 : univ, out1 : univ] {

  (ev in EVswitch_down && ( true[])
      && out0 = (EVswitch_down <: ev).sw && true[])
}

pred plus_learned[st: State, ev: Event, out0 : univ, out1 : univ, out2 : univ] {

  (ev in EVpacket && ( not ((EVpacket <: ev).locsw->(EVpacket <: ev).locpt->(EVpacket <: ev).dlsrc in st.learned))
      && out0 = (EVpacket <: ev).locsw && out1 = (EVpacket <: ev).locpt && out2 = (EVpacket <: ev).dlsrc)
}

pred plus_switch_has_port[st: State, ev: Event, out0 : univ, out1 : univ] {

  (ev in EVswitch_port && ( true[])
      && out0 = (EVswitch_port <: ev).sw && out1 = (EVswitch_port <: ev).pt)
}

pred transition[st1: State, ev: Event, st2: State] { 
  st2.learned = (st1.learned
            - { tup0: Switchid,tup1: Portid,tup2: Macaddr | minus_learned[st1, ev, tup0,tup1,tup2]})
            + { tup0: Switchid,tup1: Portid,tup2: Macaddr | plus_learned[st1, ev, tup0,tup1,tup2]} &&

  st2.switch_has_port = (st1.switch_has_port
            - { tup0: Switchid,tup1: Portid | minus_switch_has_port[st1, ev, tup0,tup1]})
            + { tup0: Switchid,tup1: Portid | plus_switch_has_port[st1, ev, tup0,tup1]}
}

/////////////////////////////////////////////////
// consistency (never >1 in a state. requires prestate condition--->inductive)
assert consistentState {
  all st1, st2: State, ev: Event |
  all sw: Switchid, host:Macaddr |
    (lone (st1.learned.host)[sw] and transition[st1, ev, st2])
     implies
    (lone (st2.learned.host)[sw])
}
// exists 2x State, 1x Event, 1x mac | (0)[lone s] and (2x Sw, 2x Port)[!lone s]
check consistentState for 3 but 1 Event, 2 State
/////////////////////////////////////////////////
// never restart flooding: if you're in a state where an address isnt flooded, it isn't going to start 
// being flooded ever again---unless a switch has come down
assert neverReFlood {
  all st1, st2: State, ev: Event |
  all sw: Switchid, host:Macaddr |
// IN PROGRESS
    (some (st1.learned.host)[sw] and transition[st1, ev, st2] and not ev in EVswitch_down)
     implies
    (some (st2.learned.host)[sw])
	
}
check neverReFlood for 3 but 1 Event, 2 State

/////////////////////////////////////////////////

/////////////////////////////////////////////////
// lone s: FORALL x:s FORALL y:s (x=y)
// one s: EXISTS x:s FORALL y:s  (x=y)
// some s: EXISTS s
// no s: FORALL s

// two exactly: EXISTS x1:s, EXISTS x2:s FORALL y:s (x1 != x2 && (y=x1 || y=x2))
// two or more: EXISTS x1:s, EXISTS x2:s (x1 != x2)
// so the negation of "lone" is just none or two or more (exists x2)





// assuming no movement, eventual silence to controller?

// if not sent on tree or endpoint, ??? [known: that's easy. what else?]
// ^^ this one is not the smart version
