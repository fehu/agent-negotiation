package feh.tec.agents.comm

import feh.util.UUIDed

trait ReportListener extends Agent.AgentActor{

}

sealed abstract class Report extends UUIDed() with Message{
  def by = from
}

object Report{

  case class MessageReceived(msg: Message, unhandled: Boolean) extends Report{
    val tpe = if(unhandled) "MessageUnhandled" else "MessageReceived"
    val asString = msg
    def from = msg.from
  }

  case class MessageSent(msg: Message, to: AgentId) extends Report{
    val tpe = "MessageSent"
    val asString = s"$to : $msg"
    def from = msg.from
  }

  case class StatesReport(negotiation: NegotiationId, states: Map[NegotiationVar[Any], Any], from: AgentId) extends Report{
    val tpe = "StatesReport"
    val asString = negotiation.toString + " -- " + states.map{ case (k, v) => s"$k: $v" }.mkString(", ")
  }

  case class StateChanged(change: Negotiation.VarUpdated[_], from: AgentId) extends Report{
    val tpe = "StateChanged"
    val asString = change.toString
  }
}