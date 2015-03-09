package feh.tec.agents.comm

import java.util.UUID

import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import feh.util.UUIDed
import scala.collection.mutable

trait NegotiationController extends SystemAgent{
  protected var negotiators: Seq[NegotiatingAgentRef] = Nil
  private val negotiatorIndices = mutable.HashMap.empty[NegotiationRole, Int]

  def nameForAgent(role: NegotiationRole, index: Int): String

  def initializeNegotiator(ref: NegotiatingAgentRef)

  def create(creator: NegotiatingAgentCreator[_])
            (implicit actFactory: ActorRefFactory) =  {
    val index = negotiatorIndices.getOrElseUpdate(creator.role, 0)
    negotiatorIndices(creator.role) = index + 1
    creator.create(nameForAgent(creator.role, index))
  }

  def start() = {}
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
  trait InitialAgents {
    controller: NegotiationController =>

    def initialNegotiatorsCreators: Seq[NegotiatingAgentCreator[_]]

    private def initialNegotiators(implicit actFactory: ActorRefFactory): Seq[NegotiatingAgentRef] =
      initialNegotiatorsCreators map create

    override def start(): Unit = negotiators ++= initialNegotiators(context)
  }

  object Supervision{
    object StopAll extends AllForOneStrategy(loggingEnabled = true, { case ex: Exception => Stop })

  }
}