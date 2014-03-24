module examples/demo0311/Stolen
// Produced automatically by flowlog -alloy at 11:15:59 on 2 11 2014

pred true[] {}
pred false[] { some none }

abstract sig Event {}

sig Switchid {}
sig Macaddr {}
sig Ipaddr {}
sig Ethtyp {}
sig Portid {}
sig Nwprotocol {}
// TODO: If a base type is unused, don't declare it.
sig TpPort {} // transport-layer port (TCP or UDP) number

sig FLString {}
sig FLInt{}

sig EVpacket extends Event {
    locsw: one Switchid,
    locpt: one Portid,
    dlsrc: one Macaddr,
    dldst: one Macaddr,
    dltyp: one Ethtyp}

fact EVpacketExtensional { all ev1, ev2: EVpacket | 
(ev1.locsw = ev2.locsw && ev1.locpt = ev2.locpt && ev1.dlsrc = ev2.dlsrc && ev1.dldst = ev2.dldst && ev1.dltyp = ev2.dltyp) implies ev1 = ev2}

sig EVstolen_laptop_report extends Event {
    macr: one Macaddr}

fact EVstolen_laptop_reportExtensional { all ev1, ev2: EVstolen_laptop_report | 
(ev1.macr = ev2.macr) implies ev1 = ev2}

sig EVstolen_laptop_cancel extends Event {
    macc: one Macaddr}

fact EVstolen_laptop_cancelExtensional { all ev1, ev2: EVstolen_laptop_cancel | 
(ev1.macc = ev2.macc) implies ev1 = ev2}

sig EVstolen_laptop_found extends Event {
    macf: one Macaddr,
    swid: one Switchid,
    time: one FLInt}

fact EVstolen_laptop_foundExtensional { all ev1, ev2: EVstolen_laptop_found | 
(ev1.macf = ev2.macf && ev1.swid = ev2.swid && ev1.time = ev2.time) implies ev1 = ev2}

sig State {
    stolen: set (Macaddr),
    get_time: set (Int)
}
fact StateExtensional { all st1, st2: State |
(st1.stolen = st2.stolen && st1.get_time = st2.get_time) implies st1 = st2}


pred forward[st: State, ev: Event, out0 : univ] {

  (ev in EVpacket && out0.locsw = ev.locsw && out0.dlsrc = ev.dlsrc && out0.dldst = ev.dldst && out0.dltyp = ev.dltyp && ( not (out0.locpt = (EVpacket <: ev).locpt))
      && true[])
}

pred minus_stolen[st: State, ev: Event, out0 : univ] {

  (ev in EVstolen_laptop_cancel && ( true[])
      && out0 = (EVstolen_laptop_cancel <: ev).macc)
}

pred notify_police[st: State, ev: Event, out0 : univ] {

  (ev in EVpacket && (some currtime : univ |  ((((out0.macf = (EVpacket <: ev).dlsrc && out0.time = currtime) && out0.swid = (EVpacket <: ev).locsw) && (EVpacket <: ev).dlsrc in st.stolen) && currtime in st.get_time))
      && true[])
}

pred plus_stolen[st: State, ev: Event, out0 : univ] {

  (ev in EVstolen_laptop_report && ( true[])
      && out0 = (EVstolen_laptop_report <: ev).macr)
}

pred transition[st1: State, ev: Event, st2: State] { 
  st2.stolen = (st1.stolen
            - { tup0: Macaddr | minus_stolen[st1, ev, tup0]})
            + { tup0: Macaddr | plus_stolen[st1, ev, tup0]} &&

  st2.get_time = (st1.get_time
            )
            
}

pred outpolicy[st1: State, ev: Event, ev2: Event] { 
  notify_police[st1, ev, ev2]}

pred testPred[] {
  //some st1, st2: State, ev: Event |
  //   transition[st1, ev, st2] &&
  //   st1 != st2 and //no st1.learned &&
  //   no st1.switch_has_port
}
run testPred for 3 but 1 Event, 2 State
