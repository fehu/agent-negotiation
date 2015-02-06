package feh.tec.agents.comm

import feh.util.HasUUID

trait Message extends HasUUID with Equals{
  val from: AgentId

  /** Message type */
  val tpe: String

  /** Human readable */
  val asString: String

  override def toString = s"$tpe($asString)"
}

trait NegotiationMessage extends Message{
  val negotiation: NegotiationId
}

trait SystemMessage extends Message