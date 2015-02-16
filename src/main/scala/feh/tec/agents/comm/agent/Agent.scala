package feh.tec.agents.comm.agent

import akka.actor.Actor
import feh.tec.agents.comm._

trait AgentActor extends Actor{
  val id: AgentId
  implicit val ref: AgentRef

  def systemMessageReceived: PartialFunction[SystemMessage, Unit]
  def messageReceived: PartialFunction[Message, Unit]

  protected var state: AgentState = AgentState.NotInitialized

  def receive: Receive = {
    case sys: SystemMessage =>
      systemMessageReceived.applyOrElse(sys, unknownSystemMessage)
    case msg: Message if messageReceived.isDefinedAt(msg) =>
      onMessageReceived(msg, unhandled = false)
      messageReceived(msg)
    case msg: Message =>
      onMessageReceived(msg, unhandled = true)
    case _ => // ignore it
  }

  protected def onMessageReceived(msg: Message, unhandled: Boolean)

  protected def unknownSystemMessage(sysMsg: SystemMessage)

  def start()
  def stop()
}

trait MessageSending {
  agent: AgentActor =>

  protected def onMessageSent(msg: Message, to: AgentRef)

  implicit class SendMessage(id: AgentRef){
    def !(msg: Message) = {
      onMessageSent(msg, id)
      id.ref ! msg
    }
  }

}

trait SystemMessageSending{
  agent: AgentActor =>

  implicit class SendMessage(id: AgentRef){
    def !(msg: SystemMessage) = {
      id.ref ! msg
    }
  }

}

trait AgentState

object AgentState{
  case object NotInitialized extends AgentState
  case object Initializing extends AgentState
  case object Initialized extends AgentState
  case object Stopped extends AgentState
}

trait Negotiating{
  agent: AgentActor =>

  /**should be called only once*/
  protected def initializeNegotiations: Seq[(Negotiation.VarUpdated[_] => Unit) => Negotiation]

  protected def onStateChanged(change: Negotiation.VarUpdated[_])

  val negotiations: Set[Negotiation] = initializeNegotiations.map(_(onStateChanged)).toSet

  private val negotiationsMap = negotiations.map(n => n.id -> n).toMap

  def negotiation(id: NegotiationId): Negotiation = negotiationsMap(id)

  def eachNegotiation(f: (Negotiation => Unit)*) = negotiations.foreach(neg => f.foreach(_(neg)))
}
