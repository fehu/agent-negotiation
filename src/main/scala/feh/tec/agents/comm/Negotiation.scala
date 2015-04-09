package feh.tec.agents.comm

import feh.util._
import scala.collection.mutable
import scala.language.existentials

case class NegotiationId(name: String){
  override lazy val toString = s"Negotiation($name)"
}

abstract class Negotiation(val id: NegotiationId, varUpdated: Negotiation.VarUpdated[_ <: NegotiationVar] => Unit)
  extends Negotiation.NegotiationBase with Negotiation.VarsCreation{

  protected def notifyVarUpdated[V <: NegotiationVar](upd: Negotiation.VarUpdated[V]) = varUpdated(upd)
}

object Negotiation{
  case class VarUpdated[V <: NegotiationVar](negId: NegotiationId, negVar: V, oldValue: Option[V#T], newValue: V#T)

  trait NegotiationBase {
    neg =>

    val id: NegotiationId

    protected val negVars = mutable.Map.empty[NegotiationVar, NegVar[_]]

    protected class NegVar[V <: NegotiationVar](id: V)(var valueOpt: Option[V#T]){
      negVars += id -> this

      def value = valueOpt getOrThrow NegotiationVar.UndefinedException(id, neg)
      def value_=(t: V#T) = valueOpt = Some(t)
    }

    def addNegVarDefaults(p: (NegotiationVar, Option[Any])*) = negVarDefaults ++= p
    private var negVarDefaults: Map[NegotiationVar, Option[Any]] = Map()

    private def getNegVarOpt[V <: NegotiationVar](nv: V) =
      negVars.get(nv).map(_.asInstanceOf[NegVar[V]])
        .orElse(negVarDefaults.get(nv).map{
          opt => new NegVar(nv)(opt.asInstanceOf[Option[V#T]]) $$ {negVars += nv -> _}
      })
    private def getNegVar[V <: NegotiationVar](nv: V) =
      getNegVarOpt(nv) getOrThrow NegotiationVar.NoSuchVarException(nv, this)

    def get[V <: NegotiationVar](negVar: V): Option[V#T] =
      getNegVarOpt(negVar).flatMap(_.valueOpt)
    def apply[V <: NegotiationVar](negVar: V): V#T = getNegVar(negVar).value

    def set[V <: NegotiationVar](negVar: V)(value: V#T) = {
      val old = get(negVar)
      getNegVar(negVar).value = value
     notifyVarUpdated(id, negVar, old, value)
    }
//    def update[V <: NegotiationVar](negVar: V, value: V#T) = set(negVar)(value)

    def transformOpt[V <: NegotiationVar](negVar: V)(f: Option[V#T] => V#T) = {
      val old = get(negVar)
      val newVal = f(old)
      getNegVar(negVar).value = newVal
      notifyVarUpdated(id, negVar, old, newVal)
    }
    def transform[V <: NegotiationVar](negVar: V)(f: V#T => V#T) =
      transformOpt(negVar)(_.get |> f)

    def report: Map[NegotiationVar, Any] = negVars.toMap.mapValues(_.valueOpt.orNull)

    protected def notifyVarUpdated[V <: NegotiationVar](upd: Negotiation.VarUpdated[V]): Unit

    private def notifyVarUpdated[V <: NegotiationVar](negId: NegotiationId, negVar: V, oldValue: Option[V#T], newValue: V#T): Unit =
      notifyVarUpdated(Negotiation.VarUpdated[V](negId, negVar, oldValue, newValue))

    //  override def equals(obj: scala.Any) = PartialFunction.cond(obj){
    //    case that: Negotiation => that.id == this.id
    //  }
    override def toString = s"Negotiation(${id.name})"

    // create scope var
    new NegVar(NegotiationVar.Scope)(None)
    // create state var
    new NegVar(NegotiationVar.State)(Some(NegotiationState.Stopped))
  }

  /** Provides defineVar object for defining negotiation states */
  trait VarsCreation{
    self: NegotiationBase =>

    object defineVar{
      def apply[V <: NegotiationVar](id: V) = new NegVar[V](id)(None)
      def withDefault[V <: NegotiationVar](id: V)(default: V#T) = new NegVar[V](id)(Option(default))
    }
  }
}

abstract class NegotiationVar { type T }
abstract class NegotiationVarDefault[V <: NegotiationVar](val default: Option[V#T]) extends NegotiationVar {type T = V#T}

trait NegotiationState

object NegotiationState{
  case object Negotiating extends NegotiationState
  case object Waiting extends NegotiationState
  case object Stopped extends NegotiationState
}

object NegotiationVar{
  case class NoSuchVarException(negVar: NegotiationVar, negotiation: Negotiation.NegotiationBase)
    extends Exception(s"$negotiation has no var $negVar")

  case class UndefinedException(negVar: NegotiationVar, negotiation: Negotiation.NegotiationBase)
    extends Exception(s"$negVar is undefined in $negotiation")

  case object Scope extends NegotiationVar{ type T = Set[NegotiatingAgentRef] }
  case object State extends NegotiationVar{ type T = NegotiationState }
}

case class Var[+T](name: String)
