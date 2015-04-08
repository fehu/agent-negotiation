package feh.tec.agents.comm.agent

import feh.tec.agents.comm.{NegotiationVar, NegotiationId, Var}
import scala.language.existentials
import feh.tec.agents.comm.negotiations.Issues.{Vars => IVar}

trait DomainIterators {
  agent: Negotiating =>

  def iteratorFor(negId: NegotiationId, issues: Seq[Var[_]]): Iterator[Map[Var[Any], Any]] = {
    val neg = negotiation(negId)
    val domains = issues.toList.map(issue => issue -> neg(IVar.IssueDomain(issue)).toStream)
    domainValuesStream(Map(), domains).toIterator
  }

  private def domainValuesStream(joinWith: Map[Var[Any], Any], domains: List[(Var[Any], Stream[Any])]): Stream[Map[Var[Any], Any]] =
    domains match {
      case Nil => joinWith #:: Stream.empty
      case (v, stream) :: xs =>
        stream.flatMap{
          dValue =>
            domainValuesStream(joinWith + (v -> dValue), xs)
        }
    }
}
