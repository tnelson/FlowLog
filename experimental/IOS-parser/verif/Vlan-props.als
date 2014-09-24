open Vlans as vlans

// Remember to remove the built_ins sig every time we re-generate

fact assumptions {
	all s: State | {
		// no VLAN configured to use -1 (special ID for no encap)
		no s.sp_vlans.C_int_neg1
		no s.virtual_interfaces.C_int_neg1
		// virtual interfaces only configured on VLAN sub-routers
		s.virtual_interfaces.FLInt.Portid in s.router_vlan[Switchid]
		// VLANs configured on physical ports only
		no s.sp_vlans.FLInt & s.virtual_interfaces.FLInt
		// p2r is irreflexive, column partitions
		no iden & Switchid.(s.p2r)
		all sw : Switchid | no (s.p2r[sw]).Portid & (s.p2r[sw])[Portid]
		// router_vlan irreflexive
		no iden & s.router_vlan
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
			{ pp in s.sp_vlans[sw].FLInt iff rp in s.virtual_interfaces[sw].FLInt
				// recall one virtual interface per vlan; if p2r connects, must share vlan id
            	  s.virtual_interfaces[sw][rp] in s.sp_vlans[sw][pp]
			}
	
		/* force these constants to exist, otherwise expressions like 
			C_string_access in st.sp_modes[(EVpacket <: ev).locsw][out0.locpt]
			will be trivially true if the model only makes C_string_access empty
         */
		some C_string_trunk 
		some C_string_access

	}
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

assert outTrunkTagged 
{
	all s: State, pin, pout: EVpacket | 
		(vlans/outpolicy[s, pin, pout] and  // policy fwds thusly
          s.sp_modes[pout.locsw, pout.locpt] = C_string_trunk) // out on a trunk		
		implies
		pout.dlvlan != C_int_neg1
}
check outTrunkTagged for 5
// for 6: 83 atoms; arity 4 is too much
// for 5 is doable (~10sec, no counterexs)

// ISSUE:
// in from VI; out trunk. still -1? Yes, because the VI -> host-side forgets to tag.
// missing a tagging block in the routerside->hostside inter-vlan rule
// ^^ FIXED

// ISSUE:
// in from trunk, but untagged; out on different trunk: still untagged
// need an sp_modes(p.locSw, p.locPt, "access") protection on the -1 intra-vlan rule
// (really, should be an error if a packet arrives untagged from a trunk)
// ^^ Actually this isn't quite right. What else to do but send on ALL vlans for that trunk?

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// TODO: Required concession to analyzer: explicit "no change" for dlvlan field
// This is because the FL->Alloy converter underconstrains ORs; defaults are only
// produced at the RULE level, not the CLAUSE level.

assert outAccessUntagged
{
	all s: State, pin, pout: EVpacket | 
		(vlans/outpolicy[s, pin, pout] and  // policy fwds thusly
          s.sp_modes[pout.locsw, pout.locpt] = C_string_access) // out on an access port
		implies
		pout.dlvlan = C_int_neg1 // untagged
}
check outAccessUntagged

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// coming in tagged, will send out only interfaces with that tag. (weak isolation)
assert outIntra_UseTagIfTagged
{
	all s: State, pin, pout: EVpacket | 
		(vlans/outpolicy[s, pin, pout] and  // policy fwds thusly
		 pin.locpt in s.sp_vlans[pin.locsw].FLInt and // in from vlan physical
		 pout.locpt in s.sp_vlans[pout.locsw].FLInt and // out vlan physical (together w/ above = intra)
		 pin.dlvlan != C_int_neg1) // comes in tagged
		implies
          pin.dlvlan in s.sp_vlans[pout.locsw, pout.locpt] // out on a trunk or access for that tag			
} 
check outIntra_UseTagIfTagged for 4 

