package feh.tec.agents.comm

import akka.actor.ActorRef

trait Agent extends agent.AgentActor with agent.Reporting with agent.ReportingSystemSupport

trait NegotiatingAgent extends Agent with agent.Negotiating with agent.NegotiationReportsSystemSupport{
  implicit val id: NegotiatingAgentId
}

trait SystemAgent extends agent.AgentActor{
  implicit val id: SystemAgentId
}

trait UserAgent extends agent.AgentActor{
  implicit val id: UserAgentId
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
  protected[comm] val ref: ActorRef

  override def equals(obj: scala.Any) = obj match {
    case that: AgentId => (that canEqual this) && this.role == that.role && this.name == that.name
  }
}

case class NegotiatingAgentId(name: String, role: NegotiationRole, protected[comm] val ref: ActorRef) extends AgentId
case class SystemAgentId(name: String, role: SystemAgentRole, protected[comm] val ref: ActorRef) extends AgentId
case class UserAgentId(name: String, role: UserAgentRole, protected[comm] val ref: ActorRef) extends AgentId
