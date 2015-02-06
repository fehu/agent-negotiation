package feh.tec.agents.comm.agent

import akka.actor.Actor
import feh.tec.agents.comm._

trait AgentActor extends Actor{
  implicit val id: AgentId

  def systemMessageReceived: PartialFunction[SystemMessage, Unit]
  def messageReceived: PartialFunction[Message, Unit]

  implicit class SendMessage(id: AgentId){
    def !(msg: Message) = {
      onMessageSent(msg, id)
      id.ref ! msg
    }
  }

  def receive: Receive = {
    case sys: SystemMessage =>
      systemMessageReceived.applyOrElse(sys, unknownSystemMessage)
    case msg: Message if messageReceived.isDefinedAt(msg) =>
      onMessageReceived(msg, unhandled = false)
      messageReceived(msg)
    case msg: Message =>
      onMessageReceived(msg, unhandled = true)
    case notAMessage => // ignore it
  }

  protected def onMessageReceived(msg: Message, unhandled: Boolean)
  protected def onMessageSent(msg: Message, to: AgentId)

  protected def unknownSystemMessage(sys: SystemMessage)

  def start()
  def stop()
}

trait Negotiating{
  agent: AgentActor =>

  protected def initNegotiations: Seq[(Negotiation.VarUpdated[_] => Unit) => Negotiation]

  protected def onStateChanged(change: Negotiation.VarUpdated[_])

  val negotiations: Set[Negotiation] = initNegotiations.map(_(onStateChanged)).toSet

  private val negotiationsMap = negotiations.map(n => n.id -> n).toMap
  def negotiation(id: NegotiationId): Negotiation = negotiationsMap(id)
  }