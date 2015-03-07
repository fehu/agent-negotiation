package feh.tec.agents.comm

import java.io.File
import akka.util.Timeout
import feh.util.file._
import akka.actor.SupervisorStrategy.{Escalate, Resume}
import akka.actor.ActorDSL._
import akka.actor._
import akka.pattern.ask
import feh.tec.agents.comm.agent.SystemSupport
import feh.util._
import scala.collection.mutable
import scala.concurrent.duration._
import scala.language.postfixOps

trait ReportListener extends SystemAgent with SystemSupport{
  def log(msg: Report)

  def messageReceived: PartialFunction[Message, Unit] = {
    case report: Report => log(report)
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
      log(report)
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

  def log(report: Report): Unit = {
    val s = report.toString
    if(report.isSevere) logger.warning(s) else logger.info(s)
  }

  def start(): Unit = {}
  def stop(): Unit = {}
  protected def onMessageSent(msg: Message, to: AgentRef) = {}
}

object ReportStdPrinter{
  def creator(role: String) = AgentCreator(SystemAgentRole(role)){id => new ReportStdPrinter(id)}
}

// TODO
class ReportDistributedPrinter(val id: SystemAgentId, logDir: Path) extends ReportListener{
  logDir.file $$ {
    f => if(!f.exists) f.mkdirs()
  }

  def log(report: Report): Unit = writerFor(report.by) ! report

  protected def logFile(id: AgentId): File = (logDir / (id.name + ".log")).file
  private val writers = mutable.Map.empty[AgentId, ActorRef]
  protected def writerFor(ag: AgentRef): ActorRef = {
    writers.getOrElseUpdate(ag.id,
      actor(new Act {
        val file = logFile(ag.id)
        if(file.exists()) file.delete()
        file.createNewFile()

        val writer = new java.io.FileWriter(file)

        become{
          case rep: Report =>
            writer.write(rep.toString + "\n")
            writer.flush()
          case SystemMessage.Stop => writer.close(); sender() ! 0
        }
      })
    )
  }

  def start(): Unit = {}
  def stop(): Unit = writers.foreach{
    case (_id, _ref) =>
      implicit val timeout = Timeout(200 millis)
      implicit def cont = context.system.dispatcher
      _ref ? SystemMessage.Stop onSuccess{case 0 => writers -= _id}
  }
  protected def onMessageSent(msg: Message, to: AgentRef) = {}

//  override val supervisorStrategy =
//    OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1 minute){
//      case k: ActorKilledException => writers remove ???
//      case _: java.io.IOException => Resume
//      case _: Throwable => Escalate
//    }
}

object ReportDistributedPrinter{
  def creator(role: String, logDir: String) =
    AgentCreator(SystemAgentRole(role)){id => new ReportDistributedPrinter(id, logDir)}
}