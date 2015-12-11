package salma.psl

import salma.model.{Action, ActionInstance}
import salma.simulation.{EvaluationResult, SimulationContext}

/**
  * Created by ckroiss on 09.12.15.
  */
class Act(val action: Action, args: Expression[Any]*) extends ControlNode {

  def evaluate(context: SimulationContext): EvaluationResult = {
    val resolvedArgs = args.map(_ evaluate(context))

    EvaluationResult(newContext = context, nextNode = None, actions = List(
      ActionInstance(action, Map((action.params zip resolvedArgs): _*))))
  }
}

object Act {
  def apply(a: Action, p: Expression[Any]*) = {
    if (a.params.length != p.length)
      throw new RuntimeException(s"Wrong number of arguments: expected ${a.params.length} but got ${p.length}")

    new Act(a, p: _*)
  }
}
