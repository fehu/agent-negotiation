package feh.tec.agents.comm.negotiations

import java.util.UUID

import feh.tec.agents.comm.Negotiation.NegotiationBase
import feh.tec.agents.comm._

object Proposals {
  trait ProposalMessage extends NegotiationMessage{
    val myValues: Map[Var[Any], Any]

    lazy val asString = myValues.mkString(", ")
  }

  trait NegotiationProposal extends ProposalMessage
  
  trait Proposal extends NegotiationProposal{   val tpe = "Proposal" }
  
  trait Acceptance extends ProposalMessage with NegotiationResponse{ val tpe = "Acceptance" }
  trait Rejection  extends ProposalMessage with NegotiationResponse{
    val tpe = "Rejection"

    def isUnconditional: Boolean
  }

  trait CounterProposal  extends NegotiationProposal with Rejection {  override val tpe = "CounterProposal" }

  object Vars{
    case object CurrentProposal extends NegotiationVar{ type T = NegotiationMessage }
    case object AwaitingResponse extends NegotiationVar{ type T = Option[UUID] }
    case object ProposalAcceptance extends NegotiationVar{ type T = Map[NegotiatingAgentRef, Boolean] }
  }

  trait Negotiation extends Negotiation.VarsCreation {
    self: NegotiationBase =>

    defineVar(Vars.CurrentProposal)
    defineVar(Vars.AwaitingResponse)
    defineVar(Vars.ProposalAcceptance)
  }
}
