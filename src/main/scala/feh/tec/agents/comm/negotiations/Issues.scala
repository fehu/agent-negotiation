package feh.tec.agents.comm.negotiations

import feh.tec.agents.comm.Negotiation.NegotiationBase
import feh.tec.agents.comm.{Negotiation, NegotiationVar, NegotiationMessage, Var}

import scala.collection.immutable

object Issues{
  sealed trait Action
    case object Add extends Action
    case object Remove extends Action

  trait IssuesMessage extends NegotiationMessage{
    val action: Action
    val issues: Seq[Var[_]]
  }

  trait IssueRequest extends IssuesMessage{
    val tpe = s"IssueRequest[$action]"
    val asString = issues.mkString(", ")
  }

  trait IssueDemand extends IssuesMessage{
    val tpe = s"IssueDemand[$action]"
    val asString = issues.mkString(", ")
  }

  /** [[feh.tec.agents.comm.NegotiationVar]]s related to Issues*/
  object Vars{
    case class Issue[I](issue: Var[I]) extends NegotiationVar{ type T = I }
    case class IssueDomain[I](issue: Var[I]) extends NegotiationVar{ type T = immutable.Traversable[I] }

    case object CurrentIssues extends NegotiationVar{ type T = Seq[Var[_]] }
  }

  trait Negotiation extends Negotiation.VarsCreation{
    self: NegotiationBase =>

    def issues = negVars.keySet.toSeq.collect{
      case Vars.Issue(nVar) => nVar
    }

    def issueValues = issues.flatMap(iss => get(Vars.Issue(iss)).map(iss -> _)).toMap

    def forIssue[T](v: Var[T], domain: Traversable[T]) = {
      apply(Vars.Issue(v))
      apply(Vars.IssueDomain(v))
      set(Vars.IssueDomain(v))(domain.toIndexedSeq)
    }


    defineVar.withDefault(Vars.CurrentIssues)(Nil)


  }
}