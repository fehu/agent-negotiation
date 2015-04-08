package feh.tec.agents.comm

import java.util.UUID

import feh.util.{UUIDed, HasUUID}

trait Message extends HasUUID with Equals{
  val sender: AgentRef

  /** Message type */
  val tpe: String

  /** Human readable */
  val asString: String

  override def toString = s"$tpe($asString) by ${sender.id}"

  override def equals(obj: scala.Any): Boolean = (this canEqual obj) && (obj match {
    case that: Message => that.uuid == this.uuid
  })
}

trait NegotiationMessage extends Message{
  val negotiation: NegotiationId

  override val sender: NegotiatingAgentRef
  override def toString = s"$tpe($negotiation: $asString) by ${sender.id}"
}

trait NegotiationResponse extends NegotiationMessage{
  val respondingTo: UUID
}

trait SystemMessage extends Message

object SystemMessage{
  case class Initialize(init: SystemMessage*)(implicit val sender: AgentRef) extends UUIDed() with SystemMessage{
    val tpe = "Initialize"
    val asString = init.mkString(", ")
  }

  protected abstract class NoContentSystemMessage(val tpe: String) extends UUIDed() with SystemMessage{
    val asString = ""
  }
  protected abstract class NegotiationScopeControl(doWithScope: String) extends UUIDed() with SystemMessage {
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

  case class SetPriority(neg: NegotiationId, priority: Int)(implicit val sender: AgentRef) extends UUIDed() with SystemMessage{
    val tpe = "SetPriority"
    val asString = s"$neg: $priority"
  }

  case class ReportStates(of: NegotiationId, toMe: Boolean)(implicit val sender: AgentRef) extends UUIDed() with SystemMessage{
    val tpe = "ReportStates"
    val asString = s"of $of"
  }
}