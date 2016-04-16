package salma.psl

import salma.model.Agent

/**
  * Created by ckroiss on 20.12.15.
  */
trait AgentExpression[+A <: Agent] extends Expression[A] {

  def act(a: ActionTemplate[A]) = {
    val theArgs = if (a.action.params.length == a.args.length + 1)
      this +: a.args
    else a.args
    new Act(a.action, theArgs)
  }


}
