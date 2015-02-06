package feh.tec.agents.comm.agent

import feh.tec.agents.comm.Negotiation.VarUpdated
import feh.tec.agents.comm.{Report, Message, AgentId}

trait Reporting{
  agent: AgentActor =>

  val Reporting = new {
    var messageSent       = false
    var messageReceived   = false
    var messageUnhandled  = true
  }

  val reportTo: AgentId

  protected def onMessageReceived(msg: Message, unhandled: Boolean): Unit =
    if(unhandled)
      if(Reporting.messageUnhandled)  reportTo.ref ! Report.MessageReceived(msg, unhandled)
      else
      if(Reporting.messageReceived)   reportTo.ref ! Report.MessageReceived(msg, unhandled)

  protected def onMessageSent(msg: Message, to: AgentId): Unit =
    if(Reporting.messageSent) reportTo.ref ! Report.MessageSent(msg, to)
}

trait ReportingNegotiations extends Reporting{
  agent: AgentActor with Negotiating =>

  override val Reporting = new {
    var messageSent       = false
    var messageReceived   = false
    var messageUnhandled  = true
    var stateChanged      = true
  }

  protected def onStateChanged(change: VarUpdated[_]): Unit =
    if(Reporting.stateChanged) reportTo.ref ! Report.StateChanged(change)
}
