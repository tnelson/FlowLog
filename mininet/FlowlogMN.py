#!/usr/bin/python

# Based on PaneDemo.py from PANE VM available at http://pane.cs.brown.edu

import math
import sys
from collections import defaultdict

from google.protobuf import text_format

from optparse import OptionParser

from mininet.net import Mininet
from mininet.cli import CLI
from mininet.log import lg
from mininet.node import Node, UserSwitch, RemoteController, CPULimitedHost
from mininet.link import TCLink
from mininet.util import irange, customConstructor

from FlowlogMNUtil import OVSHtbQosSwitch, addDictOption, subnetStr

import routers_pb2

CONTROLLERDEF = 'remote'
CONTROLLERS = { 'remote': RemoteController }

SWITCHDEF =  'ovshtb'
SWITCHES = { 'ovshtb': OVSHtbQosSwitch,
             'user': UserSwitch }

CREATE_EDGE_DEF = 'true'
CREATE_EDGE = { 'true': True,
                'false': False }

class FlowlogDemo(object):
    "Run a Flowlog demo."

    def __init__(self):
      # 15 Mbps bandwidth and 2 ms delay on each link
      #self.linkopts = dict(bw=15, delay='2ms', loss=0, use_htb=True) # disabled for testing
      self.linkopts = dict()
      self.numHostsPerSubnet = 2

      self.options = None
      self.args = None

      self.subnetRootSwitch = None
      self.subnetRootSwitchBaseDpid = None
      self.subnetHostLastIP = {}
      self.globalHostCount = 0
      self.globalPeerCount = 0
      self.globalEdgeSwCount = 0
      self.networksToLaunch = {}

      self.parseArgs()
      lg.setLogLevel('info')
      self.runDemo()

    def setSubnetRootSwitchBaseDpid(self, routers):
      self.subnetRootSwitchBaseDpid = int(routers.subnet_base_dpid, 16)

    def nextSubnetRootSwitch(self, network):
      num = len(self.subnetRootSwitch) + 1
      dpid = self.subnetRootSwitchBaseDpid + num

      return network.addSwitch('rs' + str(num), dpid=hex(dpid)[2:]) # 0x is implicit

    # TODO(adf): this needs a LOT more error checking:
    #
    # 1) next_ip can increment past the valid IPs in the subnet
    #    (need to check against mask for that)
    # 2) assumes that 'gw' is the *smallest* addr. we should not
    #    do that. we should start from the lowest addr in subnet
    #    and skip gw
    # 3) assumes subnet has only 1 "gateway". however, if two
    #    routers are connected to the same subnet for which we
    #    want to add hosts, we're going to need to change to adding
    #    all hosts at the end so we can skip all router interfaces
    def nextSubnetHost(self, addr, mask, gw):
      try:
        last_ip = self.subnetHostLastIP[subnetStr(addr, mask)]
      except KeyError:
        last_ip = gw

      pieces = last_ip.split('.')
      pieces[3] = str(1 + int(pieces[3]))
      next_ip = '.'.join(pieces)

      self.subnetHostLastIP[subnetStr(addr, mask)] = next_ip
      return next_ip

    def parseArgs(self):
        opts = OptionParser()
        addDictOption(opts, SWITCHES, SWITCHDEF, 'switch')
        addDictOption(opts, CONTROLLERS, CONTROLLERDEF, 'controller')
        addDictOption(opts, CREATE_EDGE, CREATE_EDGE_DEF, 'create_edge')
        self.options, self.args = opts.parse_args()


    # The router is attached to each subnet first via the dlDst translator.
    # The translator is then attached to an ACL table.
    # Then, we attach that ACL table to the subnet's "subnetRootSwitch";
    # if no such switch exists, we create it.
    #
    # So, from the outermost switch (ACL), subnet 1's rootSwitch is at
    # port 1; port 2 connects to port 1 on the translator. Then, port 2
    # on the translator is attached to port 2 on the router, where the
    # pipeline terminates (port 1 on the router is reserved for the NAT)
    #
    # In general, on the ACL switch and the Translator switch, subnet N
    # is on port (2N-1) for incoming from hosts, and port (2N) for
    # outgoing to the next switch in the pipeline.
    # On the router, subnets are attached sequentially start at port 2.

    def buildRouter(self, network, r):
      r.name = r.name.encode('ascii', 'ignore')

      router = network.addSwitch(r.name + '-router', dpid=r.self_dpid)

      nat = network.addSwitch(r.name + '-nat', dpid=r.nat_dpid)
      network.addLink(router, nat, **self.linkopts)

      translator = network.addSwitch(r.name + '-tr', dpid=r.tr_dpid)
      acl_table = network.addSwitch(r.name + '-acl', dpid=r.acl_dpid)

      for (i, s) in enumerate(r.subnets):
        s.gw = s.gw.encode('ascii', 'ignore')

        srs = self.subnetRootSwitch[subnetStr(s.addr, s.mask)]
        network.addLink(srs, acl_table, **self.linkopts)
        network.addLink(acl_table, translator, **self.linkopts)
        network.addLink(translator, router, **self.linkopts)

        # if there's room for hosts, add an edge switch with some hosts!
        if s.mask < 30 and self.options.create_edge:
          self.globalEdgeSwCount += 1
          edge_switch = network.addSwitch('s' + str(self.globalEdgeSwCount))
          network.addLink(srs, edge_switch, **self.linkopts)

          for h in irange(1, self.numHostsPerSubnet):
            self.globalHostCount += 1
            name = "host%d" % self.globalHostCount
            ip = self.nextSubnetHost(s.addr, s.mask, s.gw)

            host = network.addHost(name, ip='%s/%d' % (ip, s.mask),
                                   defaultRoute='dev %s-eth0 via %s' % (name, s.gw))
            network.addLink(host, edge_switch, **self.linkopts)

      # Finally, add hosts which represents our BGP peers

      for p in r.peers:
        self.globalPeerCount += 1
        name = "peer%d" % self.globalPeerCount
        peer = network.addHost(name, ip='%s/%d' % (p.ip, p.mask),
                               mac=p.mac)
        network.addLink(router, peer, **self.linkopts)

        self.networksToLaunch[name] = p.networks

    def buildNetwork(self, network, routers):
      self.subnetRootSwitch = defaultdict(lambda: self.nextSubnetRootSwitch(network))

      for router in routers.routers:
        self.buildRouter(network, router)

    def readProtobuf(self):
      routers = routers_pb2.Routers()

      try:
        f = open(sys.argv[1], "r")
        s = f.read()
        f.close()
        try: # try text format first
          text_format.Merge(s, routers)
        except text_format.ParseError:
          routers.ParseFromString(s)
        return routers
      except IndexError:
        sys.exit("Error: must provide a routers protobuf as first argument.")
      except IOError:
        sys.exit(sys.argv[1] + ": Could not open protobuf file.")

    def launchNetwork(self, network, host_cmd, host_cmd_opts):
        network.start()

        # Increase ARP timeout from 60 seconds to 1 hour
        for host in network.hosts:
            host.cmd("sysctl -w net.ipv4.neigh." + str(host.defaultIntf()) +
                     ".gc_stale_time=3600")

        print
        print "*** Hosts are running sshd at the following addresses:"
        print
        for host in network.hosts:
            host.cmd(host_cmd + ' ' + host_cmd_opts + '&')
            print host.name, host.IP()

    def teardownNetwork(self, network, host_cmd):
        print
        print "*** Shutting-down SSH daemons"
        print
        for host in network.hosts:
            host.cmd( 'kill %' + host_cmd )

        network.stop()

    def launchHttpdOnInternets(self, network, node_name, networks):
        node = network.getNodeByName(node_name)

        for (i, network) in enumerate(networks):
          # TODO(adf): add 'mask' as well. need to convert to dotted-quad
          node.cmd('ifconfig %s-eth0:%d %s' %
                   (node_name, i+1, network.addr.rpartition('.')[0] + '.1'))

        node.cmd('python -mSimpleHTTPServer &')

    def runDemo(self, host_cmd='/usr/sbin/sshd', host_cmd_opts='-D'):
        routers = self.readProtobuf()
        self.setSubnetRootSwitchBaseDpid(routers)

        controller = customConstructor(CONTROLLERS, self.options.controller)
        switch = customConstructor(SWITCHES, self.options.switch)

        network = Mininet(controller=controller, link=TCLink,
                          # host=CPULimitedHost, # seems better without this
                          switch=switch,
                          autoSetMacs=True)

        network.addController('flowlog')
        self.buildNetwork(network, routers)

        self.launchNetwork(network, host_cmd, host_cmd_opts)

        for key in self.networksToLaunch:
          self.launchHttpdOnInternets(network, key, self.networksToLaunch[key])

        self.demo(network)
        self.teardownNetwork(network, host_cmd)

    def demo(self, network):
        # Generally, this will be overriden for more interesting demos
        print
        print "*** Type 'exit' or control-D to shut down network"
        CLI( network )


if __name__ == '__main__':
    FlowlogDemo()
