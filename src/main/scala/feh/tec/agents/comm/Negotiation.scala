package feh.tec.agents.comm

import feh.util._

case class NegotiationId(name: String)

abstract class Negotiation(val id: NegotiationId, varUpdated: Negotiation.VarUpdated[_] => Unit)
  extends Negotiation.NegotiationBase with Negotiation.VarsCreation{

  protected def notifyVarUpdated[T](upd: Negotiation.VarUpdated[T]) = varUpdated(upd)
}

object Negotiation{
  case class VarUpdated[T](negId: NegotiationId, negVar: NegotiationVar[T], oldValue: T, newValue: T)

  trait NegotiationBase {
    neg =>

    val id: NegotiationId

    private var negVars: Map[NegotiationVar[_], NegVar[_]] = Map()

    protected class NegVar[T](id: NegotiationVar[T], var valueOpt: Option[T]){
      negVars += id -> this

      def value = valueOpt getOrThrow NegotiationVar.UndefinedException(id, neg)
      def value_=(t: T) = valueOpt = Some(t)
    }

    private def getNegVarOpt[T](nv: NegotiationVar[T]) = negVars.get(nv).map(_.asInstanceOf[NegVar[T]])
    private def getNegVar[T](nv: NegotiationVar[T]) = getNegVarOpt(nv) getOrThrow NegotiationVar.NoSuchVarException(nv, this)

    def get[T](negVar: NegotiationVar[T]): Option[T] = getNegVarOpt(negVar).map(_.value)
    def apply[T](negVar: NegotiationVar[T]): T = getNegVar(negVar).value

    def set[T](negVar: NegotiationVar[T], value: T) = {
      val old = getNegVar(negVar).value
      getNegVar(negVar).value = value
      notifyVarUpdated(id, negVar, old, value)
    }
    def transform[T](negVar: NegotiationVar[T], f: T => T) = {
      val old = getNegVar(negVar).value
      val newVal = f(old)
      getNegVar(negVar).value = newVal
      notifyVarUpdated(id, negVar, old, newVal)
    }

    def report: Map[NegotiationVar[_], Any] = negVars.mapValues(_.valueOpt.orNull)

    protected def notifyVarUpdated[T](upd: Negotiation.VarUpdated[T]): Unit

    protected def notifyVarUpdated[T](negId: NegotiationId, negVar: NegotiationVar[T], oldValue: T, newValue: T): Unit =
      notifyVarUpdated(Negotiation.VarUpdated(negId, negVar, oldValue, newValue))

    //  override def equals(obj: scala.Any) = PartialFunction.cond(obj){
    //    case that: Negotiation => that.id == this.id
    //  }
    override def toString = s"Negotiation(${id.name})"

    // create scope var
    new NegVar(NegotiationVar.Scope, None)
  }

  /** Provides defineVar object for defining negotiation states */
  trait VarsCreation{
    self: NegotiationBase =>

    object defineVar{
      def apply[T](id: NegotiationVar[T]) = new NegVar[T](id, None)

      def priority = apply(NegotiationVar.Priority)
      def forIssue[T](issue: Var[T]) = apply(NegotiationVar.Issue(issue))
    }
  }
}

abstract class NegotiationVar[T] extends Product

trait NegotiationState

object NegotiationState{
  case object NotInitialized extends NegotiationState
  case object Initializing extends NegotiationState
  case object Initialized extends NegotiationState
  case object Negotiating extends NegotiationState
  case object Stopped extends NegotiationState
}

object NegotiationVar{
  case class NoSuchVarException(negVar: NegotiationVar[_], negotiation: Negotiation.NegotiationBase)
    extends Exception(s"$negotiation has no var $negVar")

  case class UndefinedException(negVar: NegotiationVar[_], negotiation: Negotiation.NegotiationBase)
    extends Exception(s"$negVar is undefined in $negotiation")

  case object Scope extends NegotiationVar[Set[NegotiatingAgentRef]]
  case object State extends NegotiationVar[NegotiationState]
  case object Priority extends NegotiationVar[Int]
  case class Issue[T](issue: Var[T]) extends NegotiationVar[T]
}

case class Var[T](name: String)