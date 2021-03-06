package feh.tec.agents.comm

import akka.actor.{ActorRefFactory, ActorRef, ActorSystem, Props}
import feh.tec.agents.comm.agent.AgentActor
import scala.reflect.ClassTag

sealed trait AgentCreator[Ag <: AgentActor] { /*[+Ag <: Agent, +Role <: AgentRole, -Id <: AgentId]*/
  type UniqueName = String

  type Id  <: AgentId
  type Ref <: AgentRef

  val role: AgentRole
  protected val creation: Id => Option[AgentRef] => Ag
  implicit val clazz: ClassTag[Ag]

  protected def props(id: Id, createdByOpt: Option[AgentRef]): Props = Props(creation(id)(createdByOpt))

  protected def id(name: UniqueName): Id
  protected def ref(id: Id, actor: ActorRef): Ref

  def create(name: UniqueName)(implicit actFactory: ActorRefFactory, createdByOpt: AgentRef = null): Ref = {
    val agId = id(name)
    val actor = actFactory.actorOf(props(agId, Option(createdByOpt)), name)
    ref(agId, actor)
  }
}

object AgentCreator{
//  def apply[Ag <: NegotiatingAgent](role: NegotiationRole, clazz: ClassTag[Ag])
//                                   (creation: NegotiatingAgentId => Ag): NegotiatingAgentCreation[Ag] =
//    new NegotiatingAgentCreation(role, clazz, creation)

  def apply[Ag <: NegotiatingAgent: ClassTag](role: NegotiationRole)
                                             (creation: NegotiatingAgentId => Option[AgentRef] => Ag): NegotiatingAgentCreator[Ag] =
    new NegotiatingAgentCreator(role, implicitly, creation)

//  def apply[Ag <: SystemAgent](role: SystemAgentRole, clazz: ClassTag[Ag])
//                              (creation: SystemAgentId => Ag): SystemAgentCreation[Ag] =
//    new SystemAgentCreation(role, clazz, creation)

  def apply[Ag <: SystemAgent: ClassTag](role: SystemAgentRole)
                                        (creation: SystemAgentId => Option[AgentRef] => Ag): SystemAgentCreator[Ag] =
    new SystemAgentCreator(role, implicitly, creation)

  def apply[Ag <: UserAgent: ClassTag](role: UserAgentRole)
                                       (creation: UserAgentId => Option[AgentRef] => Ag): UserAgentCreator[Ag] =
    new UserAgentCreator[Ag](role, implicitly, creation)
}

final class NegotiatingAgentCreator[Ag <: NegotiatingAgent](
                                      val role: NegotiationRole,
                                      implicit val clazz: ClassTag[Ag],
                                      protected val creation: NegotiatingAgentId => Option[AgentRef] => Ag) extends AgentCreator[Ag]{
  type Id  = NegotiatingAgentId
  type Ref = NegotiatingAgentRef

  protected def id(name: UniqueName) = NegotiatingAgentId(name, role)
  protected def ref(id: Id, actor: ActorRef) = NegotiatingAgentRef(id, actor)
}

final class SystemAgentCreator[Ag <: SystemAgent](
                                val role: SystemAgentRole,
                                implicit val clazz: ClassTag[Ag],
                                protected val creation: SystemAgentId => Option[AgentRef] => Ag) extends AgentCreator[Ag]{
  type Id  = SystemAgentId
  type Ref = SystemAgentRef

  protected def id(name: UniqueName) = SystemAgentId(name, role)
  protected def ref(id: Id, actor: ActorRef) = SystemAgentRef(id, actor)
}

final class UserAgentCreator[Ag <: UserAgent](
                              val role: UserAgentRole,
                              implicit val clazz: ClassTag[Ag],
                              protected val creation: UserAgentId => Option[AgentRef] => Ag) extends AgentCreator[Ag]{
  type Id = UserAgentId
  type Ref = UserAgentRef

  protected def id(name: UniqueName) = UserAgentId(name, role)
  protected def ref(id: Id, actor: ActorRef) = UserAgentRef(id, actor)
}