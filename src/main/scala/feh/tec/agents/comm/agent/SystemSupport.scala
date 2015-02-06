package feh.tec.agents.comm.agent

import feh.tec.agents.comm._

/**
 * Support for basic system messages
 */
trait SystemSupport {
  agent: AgentActor =>

  /** A SystemMessage was sent message by an agent with not a SystemAgentId */
  protected def systemMessageFraud(fraud: SystemMessage)

  def systemMessageReceived: PartialFunction[SystemMessage, Unit] = {
    case fraud if !fraud.sender.isInstanceOf[SystemAgentId] => systemMessageFraud(fraud)
    case _: SystemMessage.Start => start()
    case _: SystemMessage.Stop  => stop()
  }

}

trait NegotiationSystemSupport extends Negotiating with SystemSupport{
  agent: AgentActor =>

  override def systemMessageReceived = super.systemMessageReceived orElse{
    case SystemMessage.SetScope(neg, scope) => negotiation(neg).set(NegotiationVar.Scope, scope)
    case SystemMessage.AddScope(neg, scope) => updateScope(neg, _ ++ scope)
    case SystemMessage.RmScope (neg, scope) => updateScope(neg, _ -- scope)
  }

  private def updateScope(of: NegotiationId, upd: Set[NegotiatingAgentId] => Set[NegotiatingAgentId]) =
    negotiation(of).transform(NegotiationVar.Scope, upd)
}

trait NegotiationReportsSystemSupport extends ReportingNegotiations with NegotiationSystemSupport{
  agent: AgentActor =>

  override def systemMessageReceived = super.systemMessageReceived orElse{
    case msg@SystemMessage.ReportStates(of) => msg.sender.ref ! Report.StatesReport(of, negotiation(of).report)
  }
}
