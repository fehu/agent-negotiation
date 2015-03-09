package feh.tec.agents.comm

import java.util.UUID

import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import feh.util.UUIDed

trait NegotiationController extends SystemAgent{
  protected var negotiators: Seq[NegotiatingAgentRef] = Nil

  def initialNegotiatorsCreators: Seq[NegotiatingAgentCreator[_]]

  def nameForAgent(creator: NegotiatingAgentCreator[_], count: Int): String

  def initializeNegotiator(ref: NegotiatingAgentRef)

  private def initialNegotiators(implicit actFactory: ActorRefFactory): Seq[NegotiatingAgentRef] =
    initialNegotiatorsCreators.zipWithIndex map { case (cr, i) => cr.create(nameForAgent(cr, i)) }

  def start() = negotiators ++= initialNegotiators(context)
  def stop() = stopNegotiation()

  def initialize(): Unit = negotiators.foreach(initializeNegotiator)

  def startNegotiation(): Unit = negotiators.foreach(_.ref ! SystemMessage.Start())

  def stopNegotiation(): Unit = negotiators.foreach(_.ref ! SystemMessage.Stop())
}


object ControllerMessage{
  case class Begin(implicit val sender: AgentRef) extends UUIDed() with SystemMessage{
    val tpe = "Begin"
    val asString = ""
  }
}

object NegotiationController{
  object Supervision{
    object StopAll extends AllForOneStrategy(loggingEnabled = true, { case ex: Exception => Stop })

  }
}