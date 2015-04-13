package feh.tec.agents.comm.agent

import feh.tec.agents.comm.Message.HasValues
import feh.tec.agents.comm._
import feh.tec.agents.comm.negotiations.Prioritized.PrioritizedMessage
import feh.tec.agents.comm.negotiations.Prioritized.Vars.Priority
import feh.tec.agents.comm.negotiations.{Var, Proposals}
import feh.util._

/** Helps to create [[PartialFunction]]s for `messageReceived`
 */
trait NegotiationReactionBuilder {
  agent: Negotiating =>

  //TODO: think how to do it!
//  def reactOnMessage(): Agent.OnMessage

//  protected trait MessageCondition

  def getFromMsg[Msg <: Message : HasValues, T](msg: Msg, v: Var[T]) = implicitly[HasValues[Msg]].value(msg)(v)

  implicit class GetFromMsgWrapper[Msg <: Message : HasValues, T](msg: Msg){
    def get(v: Var[T]) = getFromMsg(msg, v)
  }

  object NegMsg{
    def unapply(msg: Message): Option[NegotiationMessage] = PartialFunction.condOpt(msg){
      case nmsg: NegotiationMessage => nmsg
    }
  }

  trait MessageMatchPair{
    def unapply(msg: NegotiationMessage): Option[(NegotiationMessage, NegotiationMessage)] = Some(msg, msg)
  }

//  object suchThat extends MessageMatchPair
  object & extends MessageMatchPair

  object InState{
    def unapply(msg: NegotiationMessage): Option[NegotiationState] = negotiation(msg.negotiation).get(NegotiationVar.State)
  }

  object WithHigherPriority{
    def unapply(msg: NegotiationMessage): Boolean = {
      if(!msg.isInstanceOf[PrioritizedMessage]) sys.error(s"$msg doesn't have priority")
      msg.asInstanceOf[PrioritizedMessage].priority > negotiation(msg.negotiation)(Priority)
    }
  }

  object WithLowerPriority{
    def unapply(msg: NegotiationMessage): Boolean = {
      if(!msg.isInstanceOf[PrioritizedMessage]) sys.error(s"$msg doesn't have priority")
      msg.asInstanceOf[PrioritizedMessage].priority < negotiation(msg.negotiation)(Priority)
    }
  }

  /** msg.priority - my.priority*/
  object WithPriorityDiff{
    def unapply(msg: NegotiationMessage): Option[Int] = {
      if(!msg.isInstanceOf[PrioritizedMessage]) sys.error(s"$msg doesn't have priority")
      Some{
        msg.asInstanceOf[PrioritizedMessage].priority - negotiation(msg.negotiation)(Priority)
      }
    }
  }

  object AwaitingResponse{
    def unapply(msg: NegotiationMessage): Boolean = {
      if(!msg.isInstanceOf[NegotiationResponse]) sys.error(s"$msg isn't response")
      negotiation(msg.negotiation)(Proposals.Vars.AwaitingResponse)
        .contains(msg.asInstanceOf[NegotiationResponse].respondingTo)
    }
  }

  //  case class &(l: MessageCondition, r: MessageCondition) extends MessageCondition{
//    def unapply(msg: NegotiationMessage) = ???
//  }

//  case class InState(state: NegotiationState) extends MessageCondition{
//    def unapply(msg: NegotiationMessage): Boolean = negotiation(msg.ne)
//  }
}
