package feh.tec.agents.comm

import java.util.UUID

import akka.actor.ActorSystem
import feh.util.UUIDed

trait NegotiationController extends SystemAgent{
  protected var negotiators: Seq[NegotiatingAgentRef] = Nil

  def initialNegotiatorsCreators: Seq[NegotiatingAgentCreator[_]]

  def nameForAgent(creator: NegotiatingAgentCreator[_], count: Int): String

  def initializeNegotiator(ref: NegotiatingAgentRef)

  def initialNegotiators(implicit asys: ActorSystem): Seq[NegotiatingAgentRef] =
    initialNegotiatorsCreators.zipWithIndex map { case (cr, i) => cr.create(nameForAgent(cr, i)) }

  def start() = negotiators ++= initialNegotiators(context.system)
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