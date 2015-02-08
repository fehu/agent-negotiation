package feh.tec.agents.comm

import feh.util.{UUIDed, HasUUID}

trait Message extends HasUUID with Equals{
  val sender: AgentRef

  /** Message type */
  val tpe: String

  /** Human readable */
  val asString: String

  def from = sender
  override def toString = s"$tpe($asString) by $sender"
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
    val scope: Set[NegotiatingAgentRef]

    val tpe = doWithScope + "Scope"
    val asString = s"$neg: ${scope.mkString(", ")}"
  }

  case class Start(implicit val sender: AgentRef) extends NoContentSystemMessage("Start")
  case class Stop (implicit val sender: AgentRef) extends NoContentSystemMessage("Stop")

  case class SetScope(neg: NegotiationId, scope: Set[NegotiatingAgentRef])
                     (implicit val sender: AgentRef) extends NegotiationScopeControl("Set")
  case class AddScope(neg: NegotiationId, scope: Set[NegotiatingAgentRef])
                     (implicit val sender: AgentRef) extends NegotiationScopeControl("Add")
  case class RmScope (neg: NegotiationId, scope: Set[NegotiatingAgentRef])
                     (implicit val sender: AgentRef) extends NegotiationScopeControl("Rm")

  case class ReportStates(of: NegotiationId, toMe: Boolean)(implicit val sender: AgentRef) extends UUIDed() with SystemMessage{
    val tpe = "ReportStates"
    val asString = s"of $of"
  }
}