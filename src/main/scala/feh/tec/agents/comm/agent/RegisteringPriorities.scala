package feh.tec.agents.comm.agent

import feh.tec.agents.comm.Negotiation.VarUpdated
import feh.tec.agents.comm._
import feh.tec.agents.comm.negotiations.Prioritized.PrioritizedMessage
import feh.tec.agents.comm.negotiations.Prioritized.Vars.Priority

import scala.collection.mutable

/** Priorities Registering for [[NegotiatingAgent]]s
 * TODO: Use NegotiationVar
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

  override protected def onVarChanged(change: VarUpdated[_ <: NegotiationVar]) = {
    super.onVarChanged(change)
    if(change.negVar == Priority) {
      val reg = priorityRegister(change.negId)
      reg += ref -> change.newValue.asInstanceOf[Int]
    }
  }

  private var topPriorityKnown = false // todo: reset
  override protected def onMessageReceived(msg: Message, unhandled: Boolean): Unit = {
    super.onMessageReceived(msg, unhandled)
    if(!unhandled) msg match {
      case hasPriority: PrioritizedMessage =>
        val sender = hasPriority.sender
        val reg = priorityRegister(hasPriority.negotiation)
        if(reg.getOrElse(sender, 0) != hasPriority.priority) reg += sender -> hasPriority.priority
        val scope = negotiation(hasPriority.negotiation) apply NegotiationVar.Scope
        if(!topPriorityKnown && reg.size == scope.size + 1) {
          topPriorityKnown = true
          topPriorityIsKnown()
        }
      case _ =>
    }
  }

  def topPriorityIsKnown() = {}
}






//trait PrioritizedNegotiationsFallback extends PrioritizedProposalBasedNegotiation{
//  case object FallbackState extends NegotiationState
//
//  case class FallbackRequest(negotiation: NegotiationId,
//                             priority: Int,
//                             uuid: UUID = UUID.randomUUID())
//                            (implicit val sender: NegotiatingAgentRef) extends PrioritizedMessage{
//    val tpe = "FallbackRequest"
//    val asString = ""
//  }
//
//  case class IWillChange(negotiation: NegotiationId,
//                         respondingTo: UUID,
//                         priority: Int,
//                         uuid: UUID = UUID.randomUUID())
//                        (implicit val sender: NegotiatingAgentRef) extends NegotiationResponse{
//    val tpe = "IWillChange"
//    val asString = ""
//  }
//}
//
//object PrioritizedNegotiationsFallback extends PrioritizedIssueNegotiations with PrioritizedNegotiationsFallback