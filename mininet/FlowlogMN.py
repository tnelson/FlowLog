#!/usr/bin/python

# Based on PaneDemo.py from PANE VM available at http://pane.cs.brown.edu

import math

from optparse import OptionParser

from mininet.net import Mininet
from mininet.cli import CLI
from mininet.log import lg
from mininet.node import Node, UserSwitch, RemoteController, CPULimitedHost
from mininet.link import TCLink
from mininet.util import irange, customConstructor

from FlowlogMNUtil import FlowlogTopo, OVSHtbQosSwitch, addDictOption


CONTROLLERDEF = 'remote'
CONTROLLERS = { 'remote': RemoteController }

SWITCHDEF =  'ovshtb'
SWITCHES = { 'ovshtb': OVSHtbQosSwitch,
             'user': UserSwitch }


class FlowlogDemo(object):
    "Run a Flowlog demo."

    def __init__(self):
        self.options = None
        self.args = None

        self.parseArgs()
        lg.setLogLevel('info')
        self.runDemo()

    def parseArgs(self):
        opts = OptionParser()
        addDictOption(opts, SWITCHES, SWITCHDEF, 'switch')
        addDictOption(opts, CONTROLLERS, CONTROLLERDEF, 'controller')
        self.options, self.args = opts.parse_args()

    def buildTopo(self):
        switch = {}
        translator = {}
        subnet_root = {}
        num_subnets = 2
        num_hosts_per_subnet = 2
        # 15 Mbps bandwidth and 2 ms delay on each link
        linkopts = dict(bw=15, delay='2ms', loss=0, use_htb=True)

        # Create a network with a two switches, each attached to a router.
        topo = FlowlogTopo()
        router = topo.addSwitch('r1', dpid="1000000000000001") # dpid is in hex by default

        # Add the dlDst translators for each subnet

        translator[1] = topo.addSwitch('t1', dpid="2000000000000001")
        translator[2] = topo.addSwitch('t2', dpid="2000000000000002")

        topo.addLink(router, translator[1], **linkopts)
        topo.addLink(router, translator[2], **linkopts)

        # Add the root switches for each subnet

        subnet_root[1] = topo.addSwitch('sr1', dpid="3000000000000001")
        subnet_root[2] = topo.addSwitch('sr2', dpid="3000000000000002")

        topo.addLink(translator[1], subnet_root[1],  **linkopts)
        topo.addLink(translator[2], subnet_root[2], **linkopts)

        # Add some edge switches

        switch[1] = topo.addSwitch('s1')
        switch[2] = topo.addSwitch('s2')

        topo.addLink(subnet_root[1], switch[1], **linkopts)
        topo.addLink(subnet_root[2], switch[2], **linkopts)

        # Add some regular hosts
        for s in irange(1, num_subnets):
          for h in irange(1, num_hosts_per_subnet):
            global_host = (s - 1) * num_hosts_per_subnet + h
            host = topo.addHost('host%s' % global_host, ip='10.0.%s.%s/24' % (s, 1 + h),
                                defaultRoute='dev host%s-eth0 via 10.0.%s.1' % (global_host, s),
                                cpu=0.4/(num_subnets * num_hosts_per_subnet))
            topo.addLink(host, switch[s], **linkopts)

        return topo

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

    def runDemo(self, host_cmd='/usr/sbin/sshd', host_cmd_opts='-D'):
        topo = self.buildTopo()
        controller = customConstructor(CONTROLLERS, self.options.controller)
        switch = customConstructor(SWITCHES, self.options.switch)

        network = Mininet(topo, controller=controller, link=TCLink, 
                          # host=CPULimitedHost, # seems better without this
                          switch=switch, ipBase='10.0.0.0/24',
                          autoSetMacs=True)

        self.launchNetwork(network, host_cmd, host_cmd_opts)
        self.demo(network)
        self.teardownNetwork(network, host_cmd)

    def demo(self, network):
        # Generally, this will be overriden for more interesting demos
        print
        print "*** Type 'exit' or control-D to shut down network"
        CLI( network )


if __name__ == '__main__':
    FlowlogDemo()
