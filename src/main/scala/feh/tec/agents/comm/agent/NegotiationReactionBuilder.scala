package feh.tec.agents.comm.agent

import java.util.UUID

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
    def unapply(msg: Message): Option[(Message, Message)] = Some(msg, msg)
  }

  object suchThat extends MessageMatchPair
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
    def unapply(msg: NegotiationResponse): Boolean = awaitResponseFor_?(msg.negotiation, msg.respondingTo)
  }

  def awaitResponseFor_?(neg: NegotiationId, id: UUID): Boolean =
    negotiation(neg)(Proposals.Vars.AwaitingResponse).contains(id)

  def awaitResponseFor(msg: NegotiationMessage){
    negotiation(msg.negotiation).set(Proposals.Vars.AwaitingResponse)(Option(msg.uuid))
  }

  def noResponsesExpected(neg: NegotiationId): Unit ={
    negotiation(neg).set(Proposals.Vars.AwaitingResponse)(None)
  }

  //  case class &(l: MessageCondition, r: MessageCondition) extends MessageCondition{
//    def unapply(msg: NegotiationMessage) = ???
//  }

//  case class InState(state: NegotiationState) extends MessageCondition{
//    def unapply(msg: NegotiationMessage): Boolean = negotiation(msg.ne)
//  }
}
