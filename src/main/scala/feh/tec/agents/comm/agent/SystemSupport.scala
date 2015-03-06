package feh.tec.agents.comm.agent

import feh.tec.agents.comm.Negotiation.VarUpdated
import feh.tec.agents.comm._

/**
 * Support for basic system messages
 */
trait SystemSupport {
  agent: AgentActor =>

  /** A SystemMessage was sent message by an agent with not a SystemAgentId */
  protected def systemMessageFraud(fraud: SystemMessage)

  def systemMessageReceived: PartialFunction[SystemMessage, Unit] = {
    case fraud if !fraud.sender.id.isInstanceOf[SystemAgentId] => systemMessageFraud(fraud)
    case _: SystemMessage.Start => start()
    case _: SystemMessage.Stop  => stop()
    case SystemMessage.Initialize(init@ _*) if state == AgentState.NotInitialized =>
      state = AgentState.Initializing
      init.foreach(systemMessageReceived)
      state = AgentState.Initialized
  }

}

trait ReportingSystemSupport extends Reporting with SystemSupport{
  agent: AgentActor =>

  protected def systemMessageFraud(fraud: SystemMessage) = reportTo ! Report.SystemMessageFraud(fraud)

  protected def unknownSystemMessage(sys: SystemMessage) = reportTo ! Report.UnknownSystemMessage(sys)
}

trait NegotiationSystemSupport extends Negotiating with SystemSupport{
  agent: AgentActor =>

  override def systemMessageReceived = super.systemMessageReceived orElse{
    case SystemMessage.SetScope(neg, scope) => negotiation(neg).set(NegotiationVar.Scope, scope)
    case SystemMessage.AddScope(neg, scope) => updateScope(neg, _ ++ scope)
    case SystemMessage.RmScope (neg, scope) => updateScope(neg, _ -- scope)
    case SystemMessage.SetPriority(neg, priority) => negotiation(neg).set(NegotiationVar.Priority, priority)
  }

  private def updateScope(of: NegotiationId, upd: Set[NegotiatingAgentRef] => Set[NegotiatingAgentRef]) =
    negotiation(of).transformOpt(NegotiationVar.Scope)(
      opt => upd(opt.getOrElse(Set()))
    )
}

trait NegotiationReportsSystemSupport extends ReportingNegotiations
  with ReportingSystemSupport
  with NegotiationSystemSupport
{
  agent: AgentActor =>

  override def systemMessageReceived = super.systemMessageReceived orElse{
    case msg@SystemMessage.ReportStates(of, toSender) =>
      val report = Report.StatesReport(of, negotiation(of).report)
      if(toSender) msg.sender.ref ! report else reportTo ! report
  }
}
