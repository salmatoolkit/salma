package salma.psl

import salma.model.ActionInstance
import salma.simulation.{EvaluationResult, SimulationContext}

/**
  * Created by ckroiss on 09.12.15.
  */
class Act(val actionName: String, args: Expression[Any]*) extends ControlNode {

  def evaluate(context: SimulationContext): EvaluationResult = {
    EvaluationResult(newContext = context, nextNode = None, actions = List(ActionInstance(actionName, args.toList)))
  }
}

object Act {
  def apply(a: String, p: Expression[Any]*) = new Act(a, p: _*)
}
