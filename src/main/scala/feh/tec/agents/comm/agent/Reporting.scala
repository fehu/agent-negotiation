package feh.tec.agents.comm.agent

import feh.tec.agents.comm.Negotiation.VarUpdated
import feh.tec.agents.comm._

trait Reporting{
  agent: AgentActor =>

  val Reporting = new {
    var messageSent       = false
    var messageReceived   = false
    var messageUnhandled  = true
  }

  val reportTo: SystemAgentRef

//  def report(msg: Report) = reportTo.ref ! msg

  protected def onMessageReceived(msg: Message, unhandled: Boolean): Unit =
    if(unhandled)
      if(Reporting.messageUnhandled)  reportTo ! Report.MessageReceived(msg, unhandled)
      else
      if(Reporting.messageReceived)   reportTo ! Report.MessageReceived(msg, unhandled)

  protected def onMessageSent(msg: Message, to: AgentRef): Unit =
    if(to != reportTo && Reporting.messageSent) reportTo ! Report.MessageSent(msg, to)
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
    if(Reporting.stateChanged) reportTo ! Report.StateChanged(change)
}
