package feh.tec.agents.comm

import feh.util.{UUIDed, HasUUID}

trait Message extends HasUUID with Equals{
  val sender: AgentId

  /** Message type */
  val tpe: String

  /** Human readable */
  val asString: String

  def from = sender
  override def toString = s"$tpe($asString)"
}

trait NegotiationMessage extends Message{
  val negotiation: NegotiationId
}

trait SystemMessage extends Message

object SystemMessage{
  protected abstract class NoContentSystemMessage(val tpe: String) extends UUIDed() with SystemMessage{
    val asString = ""
  }
  protected  abstract class NegotiationScopeControl(doWithScope: String) extends UUIDed() with SystemMessage {
    val neg: NegotiationId
    val scope: Set[NegotiatingAgentId]

    val tpe = doWithScope + "Scope"
    val asString = s"$neg: ${scope.mkString(", ")}"
  }

  case class Start(implicit val sender: AgentId) extends NoContentSystemMessage("Start")
  case class Stop (implicit val sender: AgentId) extends NoContentSystemMessage("Stop")

  case class SetScope(neg: NegotiationId, scope: Set[NegotiatingAgentId])
                     (implicit val sender: AgentId) extends NegotiationScopeControl("Set")
  case class AddScope(neg: NegotiationId, scope: Set[NegotiatingAgentId])
                     (implicit val sender: AgentId) extends NegotiationScopeControl("Add")
  case class RmScope (neg: NegotiationId, scope: Set[NegotiatingAgentId])
                     (implicit val sender: AgentId) extends NegotiationScopeControl("Rm")

  case class ReportStates(of: NegotiationId)(implicit val sender: AgentId) extends UUIDed() with SystemMessage{
    val tpe = "ReportStates"
    val asString = s"of $of"
  }
}