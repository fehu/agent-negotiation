package feh.tec.agents.comm.agent

import feh.tec.agents.comm.NegotiationMessage

import scala.collection.mutable

trait MessageDelaying extends Negotiating{
  agent: AgentActor =>

  protected val delayedMessages = mutable.Buffer.empty[NegotiationMessage]

  def delayMessage(msg: NegotiationMessage): Unit = delayedMessages += msg
  def resendDelayedMessages(): Unit = {
    delayedMessages foreach(self ! _)
    delayedMessages.clear()
  }
}
