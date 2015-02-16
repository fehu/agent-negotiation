package feh.tec.agents.comm.agent

import java.util.UUID
import feh.tec.agents.comm.Negotiation.VarUpdated
import feh.tec.agents.comm._

import scala.collection.mutable

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

  case class IssueRequest(negotiation: NegotiationId,
                          action: IssueNegotiation.Action,
                          issues: Seq[Var[_]],
                          priority: Int,
                          uuid: UUID = UUID.randomUUID())
                         (implicit val sender: AgentRef)
    extends PrioritizedMessage
  {
    val tpe = s"IssueRequest[$action]"
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

object PrioritizedNegotiations extends PrioritizedIssueNegotiations with PrioritizedProposalBasedNegotiation




/** Priorities Registering for [[NegotiatingAgent]]s
 */
trait RegisteringPriorities extends NegotiatingAgent{
  private var _priorityRegister = Map.empty[NegotiationId, mutable.Map[NegotiatingAgentRef, Int]]

  protected def priorityRegister(neg: NegotiationId) =
    if(_priorityRegister.contains(neg)) _priorityRegister(neg)
    else {
      val reg = mutable.HashMap.empty[NegotiatingAgentRef, Int]
      _priorityRegister += neg -> reg
      reg
    }

  /** Some(true/false); None - unknown */
  def isTopPriority(neg: NegotiationId) = {
    val reg = priorityRegister(neg)
    val scope = negotiation(neg) apply NegotiationVar.Scope
    if (reg.size != scope.size + 1) None
    else Some{
      reg.maxBy(_._2)._1 == ref
    }
  }

  override protected def onVarChanged(change: VarUpdated[_]) = {
    super.onVarChanged(change)
    if(change.negVar == NegotiationVar.Priority) {
      val reg = priorityRegister(change.negId)
      reg += ref -> change.newValue.asInstanceOf[Int]
    }
  }

  override protected def onMessageReceived(msg: Message, unhandled: Boolean): Unit = {
    super.onMessageReceived(msg, unhandled)
    if(!unhandled) msg match {
      case hasPriority: PrioritizedMessage =>
        val sender = hasPriority.sender.asInstanceOf[NegotiatingAgentRef]
        val reg = priorityRegister(hasPriority.negotiation)
        if(reg.getOrElse(sender, 0) != hasPriority.priority) reg += sender -> hasPriority.priority
        val scope = negotiation(hasPriority.negotiation) apply NegotiationVar.Scope
        if(reg.size == scope.size + 1) topPriorityIsKnown()
      case _ =>
    }
  }

  def topPriorityIsKnown() = {}
}