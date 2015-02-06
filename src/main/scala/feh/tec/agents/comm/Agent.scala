package feh.tec.agents.comm

import akka.actor.ActorRef

trait Agent extends agent.AgentActor with AgentReporting.Reporting
trait NegotiatingAgent extends Agent with agent.Negotiating with AgentReporting.ReportingNegotiations

sealed trait AgentRole{
  val role: String
}

case class NegotiationRole(role: String) extends AgentRole
case class SystemAgentRole(role: String) extends AgentRole
case class UserAgentRole(role: String) extends AgentRole

/** Name should be unique */
case class AgentId(name: String, role: AgentRole, protected[comm] val ref: ActorRef)
