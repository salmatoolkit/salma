package salma.psl

import salma.model.{Action, ActionInstance}
import salma.simulation.{EvaluationResult, SimulationContext}

/**
  * Created by ckroiss on 09.12.15.
  */
private[salma] class Act(val action: Action, val args: Seq[Expression[Any]]) extends ControlNode {

  def evaluate(context: SimulationContext): EvaluationResult = {
    val resolvedArgs = args.map(_ evaluate context)

    EvaluationResult(context.varMapping, nextNodes = Nil, actions = List(
      ActionInstance(action, Map((action.params zip resolvedArgs): _*))))
  }

}
