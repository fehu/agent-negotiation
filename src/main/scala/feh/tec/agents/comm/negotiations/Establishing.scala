package feh.tec.agents.comm.negotiations

import java.util.UUID

import feh.tec.agents.comm.Message.HasValues
import feh.tec.agents.comm.{NegotiatingAgentRef, Message}
import feh.util.UUIDed

object Establishing {

  trait NegotiationEstablishingMessage extends UUIDed with Message{
    val values: Map[Var[Any], Any]
    override val sender: NegotiatingAgentRef

    lazy val asString = values mkString ", "
  }

  implicit def itHasValues = new HasValues[NegotiationEstablishingMessage] {
    def values = _.values
  }

  trait NegotiationProposition extends NegotiationEstablishingMessage{
    val tpe = "NegotiationProposition"
    def canEqual(that: Any) = that.isInstanceOf[NegotiationProposition]
  }

  trait NegotiationAcceptance extends NegotiationEstablishingMessage{
    val tpe = "NegotiationAcceptance"
    def canEqual(that: Any) = that.isInstanceOf[NegotiationAcceptance]
  }

  trait NegotiationRejection extends NegotiationEstablishingMessage{
    val tpe = "NegotiationRejection"
    def canEqual(that: Any) = that.isInstanceOf[NegotiationRejection]
  }

//  object Vars{
//
//  }
}
