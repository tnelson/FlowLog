open Arp as arp

// Remember to remove the built_ins sig every time we re-generate

fact assumptions {
	all s: State | {
		// p2r is irreflexive, column partitions
		no iden & Switchid.(s.p2r)
		all sw : Switchid | no (s.p2r[sw]).Portid & (s.p2r[sw])[Portid]
		// router_vlan irreflexive
		no iden & s.router_vlan
		// modes are either "access" or "trunk" (no trunk used in this module)
		s.sp_modes[Switchid][Portid] in C_string_access //+ C_string_trunk
	
		/* force these constants to exist, otherwise expressions like 
			C_string_access in st.sp_modes[(EVpacket <: ev).locsw][out0.locpt]
			will be trivially true if the model only makes C_string_access empty
         */
//		some C_string_trunk 
		some C_string_access

	}
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

assert outKnownNeverRequested
{
	all s: State, pin: EVarp_packet, pout: EVarp_packet | 
		(arp/outpolicy[s, pin, pout] and  // policy fwds thusly
          pin.arp_tpa in s.cached.Macaddr) // known 
		implies
		pout.arp_op = C_int_2 // only ever reply
}
check outKnownNeverRequested

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
