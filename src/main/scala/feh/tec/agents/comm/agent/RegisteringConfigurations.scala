package feh.tec.agents.comm.agent

import feh.tec.agents.comm.{NegotiationId, Var, NegotiationVar, NegotiatingAgent}

trait RegisteringConfigurations{ // todo
  agent: NegotiatingAgent =>

//  def guardConfigurationInfo(negId: NegotiationId, agPriority: Int, values: Map[Var[Any], Any]): Unit = {
//    val neg = negotiation(negId)
//    val currentConf = neg
//  }
}

object RegisteringConfigurations {
  class PartialNegSpaceConfiguration(var configurations: Map[Int /*Priority*/, Map[Var[Any], Any]])
  case class NegSpaceConfiguration(      configurations: Map[Int /*Priority*/, Map[Var[Any], Any]])

  case object FailedConfigurations extends NegotiationVar[Set[NegSpaceConfiguration]]
  case object CurrentConfigurations extends NegotiationVar[PartialNegSpaceConfiguration]
}
