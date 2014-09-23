open Vlans as vlans

// Remember to remove the built_ins sig every time we re-generate

fact assumptions {
	all s: State | {
		// no VLAN configured to use -1 (special ID for no encap)
		no s.sp_vlans.C_int_neg1
		// VLANs configured on physical ports only
		no s.sp_vlans.FLInt & s.virtual_interfaces.FLInt
		// p2r is irreflexive, column partitions
		no iden & Switchid.(s.p2r)
		all sw : Switchid | no (s.p2r[sw]).Portid & (s.p2r[sw])[Portid]
		// never learn a virtual interface as they are never physical
		no (s.virtual_interfaces).FLInt & (s.learned).Macaddr
		// sp_vlans and sp_mode map the same sw/pt pairs
		s.sp_vlans.FLInt = s.sp_modes.FLString
		// modes are either "access" or "trunk"
		s.sp_modes[Switchid][Portid] in C_string_access + C_string_trunk
		// if a port is a vlan physical port, it's in col 1 of p2r
		s.sp_vlans.FLInt in s.p2r.Portid 
		// p2r maps vlan <--> virtual.
		all sw: Switchid, pp, rp: Portid | (pp -> rp) in s.p2r[sw] implies
			{ pp in s.sp_vlans[sw].FLInt iff rp in s.virtual_interfaces[sw].FLInt }

	}
}

// possible error: pkt can enter and exit same interface, if cfg is bad

assert outTrunkTagged 
{
	all s: State, pin, pout: EVpacket | 
		(vlans/outpolicy[s, pin, pout] and  // policy fwds thusly
		 some C_string_trunk and  // (need this otherwise next line is insufficient)
          s.sp_modes[pout.locsw, pout.locpt] = C_string_trunk // out on a trunk
	
		)
		implies
		pout.dlvlan != C_int_neg1
}

// ISSUE:
// in from VI; out trunk. still -1? Yes, because the VI -> host-side forgets to tag.
// Discuss with RF
// missing a tagging block in the routerside->hostside inter-vlan rule

// ISSUE:
// in from trunk, but untagged; out on different trunk: still untagged
// need an sp_modes(p.locSw, p.locPt, "access") protection on the -1 intra-vlan rule
// (really, should be an error if a packet arrives untagged from a trunk)

check outTrunkTagged
