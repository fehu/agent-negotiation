package feh.tec.agents.comm

import akka.actor.{ActorLogging, ActorRef}
import feh.tec.agents.comm.agent.SystemSupport
import feh.util.UUIDed

trait ReportListener extends SystemAgent with SystemSupport{
  def log(entry: String, warn: Boolean)

  def messageReceived: PartialFunction[Message, Unit] = {
    case report: Report => log(report.toString, report.isSevere)
  }

  protected def onMessageReceived(msg: Message, unhandled: Boolean): Unit = {}

  /** A SystemMessage was sent message by an agent with not a SystemAgentId */
  protected def systemMessageFraud(fraud: SystemMessage) = self ! Report.SystemMessageFraud(fraud)
  protected def unknownSystemMessage(sysMsg: SystemMessage) = self ! Report.UnknownSystemMessage(sysMsg) 
}

trait ReportForwarder extends ReportListener{
  def forwardReport: PartialFunction[Report, Set[ActorRef]]

  override def messageReceived: PartialFunction[Message, Unit] = {
    case report: Report =>
      forwardReport.lift(report).foreach(_.foreach(_ ! report))
      log(report.toString, report.isSevere)
  }
}

sealed abstract class Report extends UUIDed() with Message{
  def by = sender
  def isSevere: Boolean
}

object Report{

  case class MessageReceived(msg: Message, unhandled: Boolean) extends Report{
    val tpe = if(unhandled) "MessageUnhandled" else "MessageReceived"
    val asString = msg.toString
    val sender = msg.sender
    def isSevere = false
  }

  case class MessageSent(msg: Message, to: AgentRef) extends Report{
    val tpe = "MessageSent"
    val asString = s"$to : $msg"
    val sender = msg.sender
    def isSevere = false
  }

  case class StatesReport(negotiation: NegotiationId, states: Map[NegotiationVar, Any])
                         (implicit val sender: AgentRef) extends Report{
    val tpe = "StatesReport"
    val asString = negotiation.toString + " -- " + states.map{ case (k, v) => s"$k: $v" }.mkString(", ")
    def isSevere = false
  }

  case class StateChanged(change: Negotiation.VarUpdated[_ <: NegotiationVar])(implicit val sender: AgentRef) extends Report{
    val tpe = "StateChanged"
    val asString = change.toString
    def isSevere = false
  }

  case class SystemMessageFraud(fraud: SystemMessage)(implicit val sender: AgentRef) extends Report{
    val tpe = "SystemMessageFraud"
    val asString = fraud.toString
    def isSevere = true
  }

  case class UnknownSystemMessage(unknown: SystemMessage)(implicit val sender: AgentRef) extends Report{
    val tpe = "UnknownSystemMessage"
    val asString = unknown.toString
    def isSevere = true
  }
}

class ReportStdPrinter(val id: SystemAgentId) extends ReportListener{
  val logger = akka.event.Logging(context.system, this)

  def log(entry: String, warn: Boolean): Unit = if(warn) logger.warning(entry) else logger.info(entry)

  def start(): Unit = {}
  def stop(): Unit = {}
  protected def onMessageSent(msg: Message, to: AgentRef) = {}
}

object ReportStdPrinter{
  def creator(role: String) = AgentCreator(SystemAgentRole(role)){id => new ReportStdPrinter(id)}
}

// TODO
class ReportDistributedPrinter(val id: SystemAgentId) extends ReportListener{
  def log(entry: String, warn: Boolean): Unit = ???

  def start(): Unit = ???
  def stop(): Unit = ???
  protected def onMessageSent(msg: Message, to: AgentRef) = {}
}