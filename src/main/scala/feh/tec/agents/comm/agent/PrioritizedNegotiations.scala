package feh.tec.agents.comm.agent

import java.util.UUID
import feh.tec.agents.comm.{Var, NegotiationId, AgentRef, NegotiationMessage}

object PrioritizedNegotiations extends PrioritizedIssueNegotiations with PrioritizedProposalBasedNegotiation

trait PrioritizedMessage extends NegotiationMessage{
  val priority: Int

  override def toString = s"$tpe($negotiation: $asString) by $sender with priority $priority"
}

trait PrioritizedIssueNegotiations{
  object IssueNegotiation{
    trait Action
    case object Add extends Action
    case object Remove extends Action
  }

  case class IssueProposal(negotiation: NegotiationId,
                           action: IssueNegotiation.Action,
                           issues: Seq[Var[_]],
                           priority: Int,
                           uuid: UUID = UUID.randomUUID())
                          (implicit val sender: AgentRef)
    extends PrioritizedMessage
  {
    val tpe = s"IssueProposal[$action]"
    val asString = issues.mkString(", ")
  }

  case class IssueDemand(negotiation: NegotiationId,
                         action: IssueNegotiation.Action,
                         issues: Seq[Var[_]],
                         priority: Int,
                         uuid: UUID = UUID.randomUUID())
                        (implicit val sender: AgentRef)
    extends PrioritizedMessage
  {
    val tpe = s"IssueDemand[$action]"
    val asString = issues.mkString(", ")
  }
}

trait PrioritizedProposalBasedNegotiation{
  case class Proposal(negotiation: NegotiationId,
                      values: Map[Var[_], Any],
                      priority: Int,
                      uuid: UUID = UUID.randomUUID())
                     (implicit val sender: AgentRef)
    extends PrioritizedMessage
  {
    val tpe = "Proposal"
    val asString = values.mkString(", ")
  }

  trait Response extends PrioritizedMessage{
    val respondingTo: UUID
  }
  
  case class Acceptance(negotiation: NegotiationId,
                        respondingTo: UUID,
                        priority: Int,
                        uuid: UUID = UUID.randomUUID())
                       (implicit val sender: AgentRef)
    extends Response
  {
    val asString = "Acceptance"
    val tpe = s"respondingTo $respondingTo"
  }
  
  case class Rejection(negotiation: NegotiationId,
                       respondingTo: UUID,
                       priority: Int,
                       uuid: UUID = UUID.randomUUID())
                      (implicit val sender: AgentRef)
    extends Response
  {
    val asString = "Rejection"
    val tpe = s"respondingTo $respondingTo"
  }
}