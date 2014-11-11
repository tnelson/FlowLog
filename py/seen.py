# Modified from Pox's learning switch example by Tim

from pox.core import core
import pox.openflow.libopenflow_01 as of

log = core.getLogger()

class LoggerSwitch (object):

  def __init__ (self, connection):
    self.connection = connection
    self.seenTable = set()
    print "...", connection
    connection.addListeners(self)

  def _handle_PacketIn (self, event):
    packet = event.parsed

    def do_flood ():
      msg = of.ofp_packet_out()
      msg.actions.append(of.ofp_action_output(port = of.OFPP_FLOOD))
      msg.data = event.ofp
      msg.buffer_id = None
      msg.in_port = event.port
      self.connection.send(msg)

    def install_nomore ():
      msg = of.ofp_flow_mod()
      msg.match = of.ofp_match(dl_src = packet.src)
      msg.buffer_id = event.ofp.buffer_id
      msg.actions.append(of.ofp_action_output(port = of.OFPP_FLOOD))
      self.connection.send(msg)

    install_nomore()
    do_flood()

    log.info("switch "+str(self.connection.dpid)+" received " + str(packet))
    self.seenTable.add(packet.src)
    log.info("new seenTable for switch "+str(self.connection.dpid)+" is: " + str(self.seenTable))


class seen(object):
  def __init__ (self):
    core.openflow.addListeners(self)

  def _handle_ConnectionUp (self, event):
    LoggerSwitch(event.connection)

def launch ():
  core.registerNew(seen)
