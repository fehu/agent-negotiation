package feh.tec.agents.comm

import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import feh.util.UUIDed
import scala.collection.mutable
import feh.util._

trait NegotiationController extends SystemAgent{
  protected val negotiatorsByRole_ = mutable.Map    .empty[NegotiationRole, Seq[NegotiatingAgentRef]]
  private val negotiatorIndices    = mutable.HashMap.empty[NegotiationRole, Int]

  def negotiatorsByRole = negotiatorsByRole_.toMap

  def nameForAgent(role: NegotiationRole, index: Int): String

  def initializeNegotiator(ref: NegotiatingAgentRef)

  def create(creator: NegotiatingAgentCreator[_])
            (implicit actFactory: ActorRefFactory) =  {
    val index = negotiatorIndices.getOrElseUpdate(creator.role, 0)
    negotiatorIndices(creator.role) = index + 1
    creator.create(nameForAgent(creator.role, index))
  }

  def mkNegotiators(creators: NegotiatingAgentCreator[_]*)(implicit actFactory: ActorRefFactory) =
    creators groupBy (_.role) mapValues (_ map create)


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

    private def initialNegotiators(implicit actFactory: ActorRefFactory) = mkNegotiators _ $ initialNegotiatorsCreators

    override def start(): Unit = negotiatorsByRole_ ++= initialNegotiators(context)
  }

  trait AgentsManipulation{
    controller: NegotiationController =>

    protected def addAgent(ref: NegotiatingAgentRef) = {
      negotiatorsByRole_ <<= (ref.id.role, _.map(ref +: _) getOrElse Seq(ref))
    }

    def newAgent(creator: NegotiatingAgentCreator[_])(implicit actFactory: ActorRefFactory) = {
      val ag = create(creator)
      addAgent(ag)
      ag
    }

    def findAgent(id: NegotiatingAgentId) = negotiatorsByRole.get(id.role).flatMap(_.find(_.id == id))

    def delAgent(id: NegotiatingAgentId) = findAgent(id) foreach {
      agRef =>
        agRef ! SystemMessage.Stop()
        negotiatorsByRole_(id.role) = negotiatorsByRole(id.role).filter(_.id != id)
    }
  }

  object Supervision{
    object StopAll extends AllForOneStrategy(loggingEnabled = true, { case ex: Exception => Stop })

  }
}