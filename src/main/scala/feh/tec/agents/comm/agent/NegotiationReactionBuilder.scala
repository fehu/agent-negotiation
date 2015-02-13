package feh.tec.agents.comm.agent

import feh.tec.agents.comm._

/** Helps to create [[PartialFunction]]s for `messageReceived`
 */
trait NegotiationReactionBuilder {
  agent: Negotiating =>

  //TODO: think how to do it!
//  def reactOnMessage(): Agent.OnMessage

//  protected trait MessageCondition

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

//  case class &(l: MessageCondition, r: MessageCondition) extends MessageCondition{
//    def unapply(msg: NegotiationMessage) = ???
//  }

//  case class InState(state: NegotiationState) extends MessageCondition{
//    def unapply(msg: NegotiationMessage): Boolean = negotiation(msg.ne)
//  }
}
