package salma.model

import salma.psl.Act

/**
  * Created by ckroiss on 15.12.15.
  */
abstract class Agent[A <: Agent[A]](val id : String) {

  def sort : String

  def act[B <: A](a: ActionTemplate[B]) = {
    new Act(a.action, a.args)
  }

}
