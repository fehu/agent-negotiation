package feh.tec.agents.comm

import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import feh.util.UUIDed
import scala.collection.mutable

trait NegotiationController extends SystemAgent{
  protected val negotiatorsByRole = mutable.Map    .empty[NegotiationRole, Seq[NegotiatingAgentRef]]
  private val negotiatorIndices   = mutable.HashMap.empty[NegotiationRole, Int]

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

  def initialize(): Unit = foreachNegotiator(initializeNegotiator)

  def startNegotiation(): Unit = foreachNegotiator(_.ref ! SystemMessage.Start())

  def stopNegotiation(): Unit = foreachNegotiator(_.ref ! SystemMessage.Stop())

  protected def foreachNegotiator(f: NegotiatingAgentRef => Unit): Unit = negotiatorsByRole.values foreach (_ foreach f)
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

    private def initialNegotiators(implicit actFactory: ActorRefFactory) =
      initialNegotiatorsCreators groupBy (_.role) mapValues (_ map create)

    override def start(): Unit = negotiatorsByRole ++= initialNegotiators(context)
  }

  object Supervision{
    object StopAll extends AllForOneStrategy(loggingEnabled = true, { case ex: Exception => Stop })

  }
}