package feh.tec.agents.comm.agent

import akka.actor.Actor
import akka.util.Timeout
import feh.tec.agents.comm._

import scala.collection.mutable

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

  implicit class SenAskMessage(id: AgentRef)(implicit timeout: Timeout){
    def ?(msg: Message) = {
      onMessageSent(msg, id)
      akka.pattern.ask(id.ref, msg)
    }
  }


}

/*
trait SystemMessageSending{
  agent: AgentActor =>

  implicit class SendSystemMessage(id: AgentRef){
    def !(msg: SystemMessage) = {
      id.ref ! msg
    }
  }

}
*/

trait AgentState

object AgentState{
  case object NotInitialized extends AgentState
  case object Initializing extends AgentState
  case object Initialized extends AgentState
  case object Stopped extends AgentState
}

trait Negotiating{
  agent: AgentActor =>

//  /**should be called only once*/
//  protected def initializeNegotiations: Seq[(Negotiation.VarUpdated[_ <: NegotiationVar] => Unit) => Negotiation]
//  val negotiations: Set[Negotiation] = initializeNegotiations.map(_(onVarChanged)).toSet

  protected def onVarChanged(change: Negotiation.VarUpdated[_ <: NegotiationVar])

  private[agent] val _negotiations = mutable.Map.empty[NegotiationId, Negotiation]

  def negotiations: Map[NegotiationId, Negotiation] = _negotiations.toMap

  def negotiation(id: NegotiationId): Negotiation = _negotiations(id)

  def eachNegotiation(f: (Negotiation => Unit)*): Unit = negotiations.mapValues(neg => f.foreach(_(neg)))
}

object Negotiating{
  trait PredefinedNegotiations extends Negotiating{
    agent: AgentActor =>

    protected def initializeNegotiations: Seq[(Negotiation.VarUpdated[_ <: NegotiationVar] => Unit) => Negotiation]

    _negotiations ++= initializeNegotiations.map(_(onVarChanged)).map(n => n.id -> n)
  }

  trait DynamicNegotiations extends Negotiating{
    agent: AgentActor =>

    def add(neg: Negotiation): NegotiationId = { _negotiations += neg.id -> neg; neg.id }
    def rm(id: NegotiationId): Unit  = _negotiations.remove(id)
  }

  object DynamicNegotiations{
    trait DynamicNegotiationsMessage // todo
  }
}

