package feh.tec.agents.comm.agent

import feh.tec.agents.comm.Negotiation.VarUpdated
import feh.tec.agents.comm._

trait Reporting extends MessageSending{
  agent: AgentActor =>

  class ReportingConfig(var messageSent: Boolean = false,
                        var messageReceived: Boolean = false,
                        var messageUnhandled: Boolean = true)
  {
    override def toString =
      s"messageSent: $messageSent, messageReceived: $messageReceived, messageUnhandled: $messageUnhandled"
  }

  val Reporting: ReportingConfig

  val reportTo: SystemAgentRef

  protected def onMessageReceived(msg: Message, unhandled: Boolean): Unit =
         if(unhandled && Reporting.messageUnhandled) reportTo ! Report.MessageReceived(msg, unhandled)
    else if(!unhandled && Reporting.messageReceived) reportTo ! Report.MessageReceived(msg, unhandled)

  protected def onMessageSent(msg: Message, to: AgentRef): Unit =
    if(to != reportTo && Reporting.messageSent) reportTo ! Report.MessageSent(msg, to)
}

trait ReportingNegotiations extends Reporting{
  agent: AgentActor with Negotiating =>

  class ReportingNegotiationsConfig(messageSent: Boolean = false,
                                    messageReceived: Boolean = false,
                                    messageUnhandled: Boolean = true,
                                    var stateChanged: Boolean = true
                                     ) extends ReportingConfig(messageSent, messageReceived, messageUnhandled)
  {
    override def toString = super.toString + s", stateChanged: $stateChanged"
  }

  override val Reporting: ReportingNegotiationsConfig

  protected def onVarChanged(change: VarUpdated[_ <: NegotiationVar]): Unit =
    if(Reporting.stateChanged) reportTo ! Report.StateChanged(change)
}
