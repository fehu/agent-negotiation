package feh.tec.agents.comm.negotiations

import feh.tec.agents.comm.Negotiation.NegotiationBase
import feh.tec.agents.comm.{Negotiation, NegotiationVar, NegotiationMessage}

object Prioritized {
  trait PrioritizedMessage extends NegotiationMessage{
    val priority: Int

    override def toString = s"$tpe($negotiation: $asString) by ${sender.id} with priority $priority"
  }

  object Vars{
    case object Priority extends NegotiationVar{ type T = Int }
  }

  trait Negotiation extends Negotiation.VarsCreation {
    self: NegotiationBase =>

    defineVar(Vars.Priority)
  }
}
