package feh.tec.agents.comm

class DeafUserAgent(val id: UserAgentId, act: DeafUserAgent => Unit) extends UserAgent{
  def start() = {}
  def stop() = {}

  def messageReceived: PartialFunction[Message, Unit] = Map()
  def systemMessageReceived: PartialFunction[SystemMessage, Unit] = Map()

  protected def onMessageReceived(msg: Message, unhandled: Boolean) = {}
  protected def onMessageSent(msg: Message, to: AgentRef) = {}
  protected def unknownSystemMessage(sysMsg: SystemMessage) = {}

  act(this)
}

object DeafUserAgent{
  def creator(role: String, act: DeafUserAgent => Unit) = AgentCreator(UserAgentRole(role)){
    id => new DeafUserAgent(id, act)
  }
}