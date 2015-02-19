package feh.tec.agents.comm

import akka.actor.ActorRef

trait Agent extends agent.AgentActor with agent.MessageSending with agent.Reporting with agent.ReportingSystemSupport

trait NegotiatingAgent extends Agent with agent.Negotiating with agent.NegotiationReportsSystemSupport{
  val id: NegotiatingAgentId
  final implicit lazy val ref = NegotiatingAgentRef(id, self)
}

trait SystemAgent extends agent.AgentActor with agent.MessageSending{
  val id: SystemAgentId
  final implicit lazy val ref = SystemAgentRef(id, self)
}

trait UserAgent extends agent.AgentActor with agent.MessageSending {
  val id: UserAgentId
  final implicit lazy val ref = UserAgentRef(id, self)
}




sealed trait AgentRole{
  val role: String
}

case class NegotiationRole(role: String) extends AgentRole
case class SystemAgentRole(role: String) extends AgentRole
case class UserAgentRole(role: String) extends AgentRole




/** Name should be unique */
sealed trait AgentId extends Equals{
  val name: String
  val role: AgentRole

  override def equals(obj: scala.Any) = obj match {
    case that: AgentId => (that canEqual this) && this.role == that.role && this.name == that.name
  }
}

case class NegotiatingAgentId(name: String, role: NegotiationRole) extends AgentId
case class SystemAgentId(name: String, role: SystemAgentRole) extends AgentId
case class UserAgentId(name: String, role: UserAgentRole) extends AgentId


sealed trait AgentRef extends Equals{
  val id: AgentId
  protected[comm] val ref: ActorRef

  override def equals(obj: scala.Any) = PartialFunction.cond(obj) {
    case that: AgentRef => (that canEqual this) && that.id == this.id
  }
}

final case class NegotiatingAgentRef(id: NegotiatingAgentId, protected[comm] val ref: ActorRef) extends AgentRef
final case class SystemAgentRef(id: SystemAgentId, protected[comm] val ref: ActorRef) extends AgentRef
final case class UserAgentRef(id: UserAgentId, protected[comm] val ref: ActorRef) extends AgentRef