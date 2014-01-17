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

from FlowlogMNUtil import FlowlogTopo, OVSHtbQosSwitch, addDictOption, subnetStr

import routers_pb2

CONTROLLERDEF = 'remote'
CONTROLLERS = { 'remote': RemoteController }

SWITCHDEF =  'ovshtb'
SWITCHES = { 'ovshtb': OVSHtbQosSwitch,
             'user': UserSwitch }

class FlowlogDemo(object):
    "Run a Flowlog demo."

    def __init__(self):
      # 15 Mbps bandwidth and 2 ms delay on each link
      #self.linkopts = dict(bw=15, delay='2ms', loss=0, use_htb=True) disabled for testing
      self.linkopts = dict()
      self.numHostsPerSubnet = 2

      self.options = None
      self.args = None

      self.subnetRootSwitch = None
      self.subnetHostLastIP = {}
      self.globalHostCount = 0
      self.globalPeerCount = 0
      self.globalEdgeSwCount = 0
      self.networksToLaunch = {}

      self.parseArgs()
      lg.setLogLevel('info')
      self.runDemo()

    def nextSubnetRootSwitch(self, topo):
      num = len(self.subnetRootSwitch) + 1
      dpid = 0x3000000000000000 + num # TODO(adf): make configurable

      return topo.addSwitch('rs' + str(num), dpid=hex(dpid)[2:]) # 0x is implicit

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
        self.options, self.args = opts.parse_args()

    def buildRouter(self, topo, r):
      r.name = r.name.encode('ascii', 'ignore')

      router = topo.addSwitch(r.name + '-router', dpid=r.self_dpid)

      nat = topo.addSwitch(r.name + '-nat', dpid=r.nat_dpid)
      topo.addLink(router, nat, **self.linkopts)

      # Add the dlDst translators for each subnet
      # The router is attached to each subnet first via a dlDst translator.
      # The translator is then attached to an ACL table.
      # Then, we attach that ACL table to the subnet's "subnetRootSwitch";
      # if no such switch exists, we create it.

      for (i, s) in enumerate(r.subnets):
        s.gw = s.gw.encode('ascii', 'ignore')

        translator = topo.addSwitch(r.name + '-t' + str(i + 1),
                                    dpid=s.tr_dpid)
        topo.addLink(router, translator, **self.linkopts)

        acl_table = topo.addSwitch(r.name + '-a' + str(i + 1),
                                   dpid=s.acl_dpid)
        topo.addLink(translator, acl_table, **self.linkopts)

        srs = self.subnetRootSwitch[subnetStr(s.addr, s.mask)]
        topo.addLink(acl_table, srs, **self.linkopts)

        # if there's room for hosts, add an edge switch with some hosts!
        if s.mask < 30:
          self.globalEdgeSwCount += 1
          edge_switch = topo.addSwitch('s' + str(self.globalEdgeSwCount))
          topo.addLink(srs, edge_switch)

          for h in irange(1, self.numHostsPerSubnet):
            self.globalHostCount += 1
            name = "host%d" % self.globalHostCount
            ip = self.nextSubnetHost(s.addr, s.mask, s.gw)

            host = topo.addHost(name, ip='%s/%d' % (ip, s.mask),
                                defaultRoute='dev %s-eth0 via %s' % (name, s.gw))
            topo.addLink(host, edge_switch, **self.linkopts)

      # Finally, add hosts which represents our BGP peers

      for p in r.peers:
        self.globalPeerCount += 1
        name = "peer%d" % self.globalPeerCount
        peer = topo.addHost(name, ip='%s/%d' % (p.ip, p.mask),
                            mac=p.mac)
        topo.addLink(router, peer)

        self.networksToLaunch[name] = p.networks

    def buildTopo(self, routers):
      topo = FlowlogTopo()
      self.subnetRootSwitch = defaultdict(lambda: self.nextSubnetRootSwitch(topo))

      for router in routers.routers:
        self.buildRouter(topo, router)

      return topo

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
        topo = self.buildTopo(routers)
        controller = customConstructor(CONTROLLERS, self.options.controller)
        switch = customConstructor(SWITCHES, self.options.switch)

        network = Mininet(topo, controller=controller, #link=TCLink, # disable for testing
                          # host=CPULimitedHost, # seems better without this
                          switch=switch, ipBase='10.0.0.0/24',
                          autoSetMacs=True)

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
