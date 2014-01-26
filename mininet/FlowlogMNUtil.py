"Utility functions and classes for FlowLog Mininet configurations."

# Based on PaneDemoUtil.py from PANE VM available at http://pane.cs.brown.edu

from time import sleep

from mininet.link import TCIntf
from mininet.node import Controller, OVSSwitch
from mininet.topo import Topo


# Mixin for Open vSwitch to make the periodicity of the interactivity probe
# much higher, and to lengthen the maximum backoff time.
# Adds 'backoff=N' & 'probe=N' (both in seconds) params to the --switch option.
def LazyOVSMixin( klass ):
  class Lazy( klass ):
    max_backoff_secs = None
    inactivity_probe_secs = None

    def __init__( self, name, backoff=600, probe=60, **kwargs ):
        super( Lazy, self ).__init__( name, **kwargs )
        self.max_backoff_secs = backoff
        self.inactivity_probe_secs = probe

    def start( self, controllers ):
      super( Lazy, self ).start( controllers )

      for uuid in self.controllerUUIDs():
        if uuid.count( '-' ) != 4:
            # Doesn't look like a UUID
            continue
        uuid = uuid.strip()
        self.cmd( 'ovs-vsctl set Controller', uuid,
                  'max_backoff=%d000 inactivity_probe=%d000'
                  % (self.max_backoff_secs, self.inactivity_probe_secs) )

  return Lazy


# Mixin for any switch to cause Mininet to pause for a specified length
# of time after starting the switch.
# Adds 'sleep=N' (in seconds) param to the --switch option.
def SleepyMixin( klass ):
  class Sleepy( klass ):
    sleep_time = None

    def __init__( self, name, sleep=0, **kwargs ):
        super( Sleepy, self ).__init__( name, **kwargs )
        self.sleep_time = sleep

    def start( self, controllers ):
        super( Sleepy, self ).start( controllers )
        if self.sleep_time > 0:
            print "started. sleeping for %d seconds..." % self.sleep_time
            sleep(self.sleep_time)

  return Sleepy


# The next three classes, defininig QOS-enabled Open vSwitch designs are
# from https://github.com/mininet/mininet/pull/132
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

            if res is None: # link may not have TC parameters
                return

            # Re-add qdisc, root, and default classes OVS created, but with
            # new parent, as setup by Mininet's TCIntf
            parent = res['parent']
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

# created as a function to allow for any necessary
# canonicalization in the future
def subnetStr(ip_str, mask):
  return ip_str + "/" + str(mask)
