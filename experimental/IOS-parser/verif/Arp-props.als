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


		// ARP packets are forced to have ARP ethtyp (this could be produced by the compiler)
		EVarp_packet.dltyp = C_ethtyp_arp
		// Never cache the controller address
		no s.cached[C_ipaddr_10_10_10_1]
		no s.cached.C_macaddr_00_00_ca_fe_ca_fe

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

assert outKnownReplyAndNoQueued
{
	all s: State, pin: EVarp_packet | 
		(pin.arp_op = C_int_1 and // ARP request incoming
         pin.arp_tpa in s.cached.Macaddr) // known     
		implies
		{
			// unbounded universal issue just saying "some pout : ..."
			// instead say: if you've created the right packet in your scenario, it's sent
			(all pout: EVarp_packet | {				
				pout.arp_op = C_int_2
				pout.arp_spa = pin.arp_tpa
				pout.arp_tha = pin.arp_sha
				pout.arp_tpa = pin.arp_spa
				pout.dltyp = pin.dltyp
				pout.dldst = pin.dlsrc
				pout.dlsrc = pout.arp_sha
				pout.locsw = pin.locsw
				pout.locpt = pin.locpt
				pout.dlvlan = pin.dlvlan
				pin.locsw not in s.switches_without_arp
				pin.arp_sha != C_macaddr_00_00_ca_fe_ca_fe
				pin.dlsrc != C_macaddr_00_00_ca_fe_ca_fe
				pin.arp_spa != C_ipaddr_10_10_10_1
				pout.arp_sha = s.cached[pin.arp_tpa]
				pin.arp_tpa != pin.arp_spa // not a senseless query; needed because of XOR

			}
			implies arp/outpolicy[s, pin, pout])

			no { x:Ipaddr, y:Macaddr, z:Ipaddr | plus_queuedrequests[s, pin, x,y,z] }			 
		}
}
check outKnownReplyAndNoQueued

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

