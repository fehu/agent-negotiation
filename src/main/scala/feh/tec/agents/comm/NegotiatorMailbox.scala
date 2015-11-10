package feh.tec.agents.comm

import akka.actor.PoisonPill
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}

/** An abstract mailbox with following priority order:
  *
  *   1.   [[SystemMessage]], or sent by [[UserAgent]]
  *   2,3. Depending on `finishReceptionBeforePoison`: [[PoisonPill]] or any other message.
  *
  * @param finishReceptionBeforePoison establishes the priority of [[PoisonPill]], whether it should be greater or less
  *                                    than for the rest of messages.
  */
abstract class NegotiatorMailbox(finishReceptionBeforePoison: Boolean) extends UnboundedStablePriorityMailbox(
  PriorityGenerator(NegotiatorMailbox.highPriority orElse
                    (if (finishReceptionBeforePoison) NegotiatorMailbox.finishPriority
                                                 else NegotiatorMailbox.poisonPriority)
  )
)


object NegotiatorMailbox{
  class FinishReceptionBeforePoison extends NegotiatorMailbox(true)
  class PoisonAfterSystemProcessed  extends NegotiatorMailbox(false)


  protected lazy val highPriority: PartialFunction[Any, Int] = {
    case sys: SystemMessage                                             => 0
    case msg: Message if msg.sender.id.role.isInstanceOf[UserAgentRole] => 0
  }
  protected lazy val poisonPriority : PartialFunction[Any, Int] = {
    case PoisonPill => 5
    case _          => 9
  }
  protected lazy val finishPriority : PartialFunction[Any, Int] = {
    case PoisonPill => 9
    case _          => 5
  }
}