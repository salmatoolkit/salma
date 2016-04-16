package salma.psl

import salma.model.{Action, Agent}

/**
  * Created by ckroiss on 15.12.15.
  */
trait ActionTemplate[-A <: Agent] {

  def action : Action

  def args : Seq[Expression[Any]]

}
