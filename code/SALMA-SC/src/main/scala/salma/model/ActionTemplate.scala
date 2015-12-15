package salma.model

import salma.psl.{Expression, Act}

/**
  * Created by ckroiss on 15.12.15.
  */
trait ActionTemplate[A <: Agent[_]] {

  def action : Action

  def args : Seq[Expression[Any]]

}
