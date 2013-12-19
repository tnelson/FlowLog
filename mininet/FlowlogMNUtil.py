"Utility functions and classes for FlowLog Mininet configurations."

# Based on PaneDemoUtil.py from PANE VM available at http://pane.cs.brown.edu

from time import sleep

from mininet.link import TCIntf
from mininet.node import Controller, OVSSwitch
from mininet.topo import Topo


class FlowlogTopo(Topo):
    def __init__(self, **opts):
        super(FlowlogTopo, self).__init__(opts)


class OVSBaseQosSwitch( OVSSwitch ):
    """A version of OVSSwitch which you can use with both TCIntf and OVS's QoS
       support. Note: this particular class is an abstract base class for
       OVSHtbQosSwitch or OVSHfscQosSwitch, as OVS supports two types of QoS
       disciplines: Hierarchical Token Bucket (HTB) and Hierarchical Fair
       Service Curves (HFSC)."""

    qosType = "__abstract__" # overriden by subclasses below
    minRateCmd = "__abstract__" # overriden by subclasses below

    def TCReapply( self, intf ):
        """The general problem here is that Open vSwitch believes it is the only
           software managing the Linux tc queues on this interface. To maintain
           this illusion, we first create a default queue with a min-rate of 1
           bps via ovs-vsctl. Then, Mininet clears these queues, and creates the
           queues for the TCIntf. We then re-create the queues created by Open
           vSwitch (1:1 and 1:0xfffe), but place them as a leaf under the
           hiearchy created by Mininet's TCInf. With these defaults in place,
           Open vSwith will place new queues under 1:0xfffe as we desire."""
        if type( intf ) is TCIntf:
            assert( self.qosType != "__abstract__" )

            # Get OVS's idea of the interface's speed:
            ifspeed = self.cmd( 'ovs-vsctl get interface ' + intf.name +
                                ' link_speed' ).rstrip()

            # Establish a default configuration for OVS's QoS
            self.cmd( 'ovs-vsctl -- set Port ' + intf.name + ' qos=@newqos'
                      ' -- --id=@newqos create QoS type=linux-' + self.qosType +
                      ' queues=0=@default' +
                      ' -- --id=@default create Queue other-config:min-rate=1' )
            # Reset Mininet's configuration
            res = intf.config( **intf.params )
            parent = res['parent']
            # Re-add qdisc, root, and default classes OVS created, but with
            # new parent, as setup by Mininet's TCIntf
            intf.tc( "%s qdisc add dev %s " + parent +
                     " handle 1: " + self.qosType + " default 1" )
            intf.tc( "%s class add dev %s classid 1:0xfffe parent 1: " +
                     self.qosType + " " + self.minRateCmd + " " + ifspeed )
            intf.tc( "%s class add dev %s classid 1:1 parent 1:0xfffe " +
                     self.qosType + " " + self.minRateCmd + " 1500" )

    def dropOVSqos( self, intf ):
        """Drops any QoS records on this interface kept by Open vSwitch. This
           also deletes the corresponding Linux tc queues."""
        out = self.cmd( 'ovs-vsctl -- get QoS ' + intf.name + ' queues' )
        out = out.rstrip( "}\n" ).lstrip( "{" ).split( "," )
        queues = map( lambda x: x.split("=")[1], out )

        self.cmd( 'ovs-vsctl -- destroy QoS ' + intf.name +
                  ' -- clear Port ' + intf.name + ' qos' )

        for q in queues:
            self.cmd( 'ovs-vsctl destroy Queue ' + q )

    def detach( self, intf ):
        if type( intf ) is TCIntf:
            self.dropOVSqos( intf )
        OVSSwitch.detach( self, intf )

    def stop( self ):
        for intf in self.intfList():
            if type( intf ) is TCIntf:
                self.dropOVSqos( intf )
        OVSSwitch.stop( self )


class OVSHtbQosSwitch( OVSBaseQosSwitch ):
    "Open vSwitch with Hierarchical Token Buckets usable with TCIntf."
    qosType    = "htb"
    minRateCmd = "rate"


class OVSHfscQosSwitch( OVSBaseQosSwitch ):
    "Open vSwitch with Hierarchical Fair Service Curves usable with TCIntf."
    qosType    = "hfsc"
    minRateCmd = "sc rate"


# addDictOption shamelessly stolen from Mininet. thanks Bob & Brandon!
def addDictOption( opts, choicesDict, default, name, helpStr=None ):
    """Convenience function to add choices dicts to OptionParser.
       opts: OptionParser instance
       choicesDict: dictionary of valid choices, must include default
       default: default choice key
       name: long option name
       help: string"""
    if default not in choicesDict:
        raise Exception( 'Invalid  default %s for choices dict: %s' %
                        ( default, name ) )
    if not helpStr:
        helpStr = ( '|'.join( sorted( choicesDict.keys() ) ) +
                   '[,param=value...]' )
    opts.add_option( '--' + name,
                    type='string',
                    default = default,
                    help = helpStr )
