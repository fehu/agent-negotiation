package feh.tec.agents.comm

import akka.actor.{ActorRef, Actor}
import feh.tec.agents.comm.Negotiation.VarUpdated

trait Agent extends Agent.AgentActor with Agent.Reporting
trait NegotiatingAgent extends Agent with Agent.Negotiating with Agent.ReportingNegotiations

sealed trait AgentRole{
  val role: String
}

case class NegotiationRole(role: String) extends AgentRole
case class SystemAgentRole(role: String) extends AgentRole
case class UserAgentRole(role: String) extends AgentRole

/** Name should be unique */
case class AgentId(name: String, role: AgentRole, protected[comm] val ref: ActorRef)

object Agent{
  trait AgentActor extends Actor{
    val id: AgentId

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
//    protected def unknownMessage(msg: Message)
  }

  trait Negotiating{
    agent: AgentActor =>

    protected def initNegotiations: Seq[(Negotiation.VarUpdated[_] => Unit) => Negotiation]

    protected def onStateChanged(change: Negotiation.VarUpdated[_])

    val negotiations: Set[Negotiation] = initNegotiations.map(_(onStateChanged)).toSet

    private val negotiationsMap = negotiations.map(n => n.id -> n).toMap
    def negotiation(id: NegotiationId): Negotiation = negotiationsMap(id)
  }

  trait Reporting{
    agent: AgentActor =>

    val Reporting = new {
      var messageSent       = false
      var messageReceived   = false
      var messageUnhandled  = true
    }

    val reportTo: AgentId

    protected def onMessageReceived(msg: Message, unhandled: Boolean): Unit =
      if(unhandled)
        if(Reporting.messageUnhandled)  reportTo.ref ! Report.MessageReceived(msg, unhandled)
      else
        if(Reporting.messageReceived)   reportTo.ref ! Report.MessageReceived(msg, unhandled)

    protected def onMessageSent(msg: Message, to: AgentId): Unit =
      if(Reporting.messageSent) reportTo.ref ! Report.MessageSent(msg, to)
  }

  trait ReportingNegotiations extends Reporting{
    agent: AgentActor with Negotiating =>

    override val Reporting = new {
      var messageSent       = false
      var messageReceived   = false
      var messageUnhandled  = true
      var stateChanged      = true
    }

    protected def onStateChanged(change: VarUpdated[_]): Unit =
      if(Reporting.stateChanged) reportTo.ref ! Report.StateChanged(change, agent.id)
  }

}