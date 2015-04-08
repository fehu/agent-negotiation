package feh.tec.agents.comm.negotiations

import java.util.UUID

import feh.tec.agents.comm.Negotiation.NegotiationBase
import feh.tec.agents.comm._

object Proposals {
  trait ProposalMessage extends NegotiationMessage{
    val myValues: Map[Var[Any], Any]
  }
  
  trait Proposal extends ProposalMessage
  
  trait Acceptance extends ProposalMessage
  trait Rejection extends ProposalMessage

  trait CounterProposal extends Rejection

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
