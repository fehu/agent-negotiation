package feh.tec.agents.comm.negotiations

import feh.tec.agents.comm.Negotiation.NegotiationBase
import feh.tec.agents.comm.{Negotiation, Var, NegotiationVar}

object DomainsIterating {

  object Vars{
    case object DomainIterator extends NegotiationVar{ type T = Iterator[Map[Var[Any], Any]] }
  }

  trait Negotiation extends Negotiation.VarsCreation {
    self: NegotiationBase =>

    defineVar(Vars.DomainIterator)
  }
}
