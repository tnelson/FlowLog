// Produced automatically by flowlog -alloy at 17:1:43 on 8 21 2013

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

sig EVstolen_laptop_report extends Event {
    mac: one Macaddr}

fact EVstolen_laptop_reportExtensional { all ev1, ev2: EVstolen_laptop_report | 
(ev1.mac = ev2.mac) implies ev1 = ev2}

sig EVstolen_laptop_cancel extends Event {
    mac: one Macaddr}

fact EVstolen_laptop_cancelExtensional { all ev1, ev2: EVstolen_laptop_cancel | 
(ev1.mac = ev2.mac) implies ev1 = ev2}

sig EVstolen_laptop_found extends Event {
    mac: one Macaddr,
    swid: one Switchid,
    time: one FLInt}

fact EVstolen_laptop_foundExtensional { all ev1, ev2: EVstolen_laptop_found | 
(ev1.mac = ev2.mac && ev1.swid = ev2.swid && ev1.time = ev2.time) implies ev1 = ev2}

sig State {
    stolen: set Macaddr,
    get_time: set FLInt
}
fact StateExtensional { all st1, st2: State |
(st1.stolen = st2.stolen && st1.get_time = st2.get_time) implies st1 = st2}

pred minus_stolen[st: State, ev: Event, out0 : univ] {

  (ev in EVstolen_laptop_cancel && ( true[])
      && out0 = (EVstolen_laptop_cancel <: ev).mac)
}

pred notify_police[st: State, ev: Event, out0 : univ, out1 : univ, out2 : univ] {

  (ev in EVpacket && ( ((EVpacket <: ev).dlsrc in st.stolen && out2 in st.get_time))
      && out0 = (EVpacket <: ev).dlsrc && out1 = (EVpacket <: ev).locsw && true[])
}

pred plus_stolen[st: State, ev: Event, out0 : univ] {

  (ev in EVstolen_laptop_report && ( true[])
      && out0 = (EVstolen_laptop_report <: ev).mac)
}

pred transition[st1: State, ev: Event, st2: State] { 
  st2.stolen = (st1.stolen
            - { tup0: Macaddr | minus_stolen[st1, ev, tup0]})
            + { tup0: Macaddr | plus_stolen[st1, ev, tup0]} &&

  st2.get_time = (st1.get_time
            )
            
}

// Only way for a laptop to stop being flagged stolen is to receive the proper cancellation
assert test {
	all st: State, st2: State, ev: Event |	
		(transition[st, ev, st2] and st.stolen != st2.stolen and st2.stolen in st.stolen) implies 
		 ev in EVstolen_laptop_cancel
}
check test for 4 but 1 Event, 2 State, 3 Macaddr,  // 3 = 2 + 1 (1 from eq)
								1 Switchid, 1 Portid, 
                                   2 Ipaddr, 1 Ethtyp, 1 Nwprotocol,
                                   1 FLInt
// need to allow for all possible event type fields in ev

// that NEQ is insideous: it hides an existential. 
// 	all st: State, st2: State, ev: Event |	 ---> E st, E st, E ev
// ---> [antec. same, conseq. negated] st.stolen != st2.stolen [E] and st2.stolen in st.stolen [A] and not in stolenlaptopreport [A]
// the st2.stolen [E] hides "exists some macaddr that is in 1 but not 2 or 2 but not 1"
// so we need an extra mac addr
