package salma.psl

import salma.simulation.{EvaluationResult, SimulationContext}

/**
  * Created by ckroiss on 20.12.15.
  */
class While(val condition: BooleanExpression, val body: ControlNode) extends ControlNode {
  def evaluate(context: SimulationContext): EvaluationResult = {
    if (condition.evaluate(context) == true) {
      EvaluationResult(context.varMapping, List(body, this), Nil)
    } else {
      EvaluationResult(context.varMapping, Nil, Nil)
    }
  }
}

object While {

  def apply(condition: BooleanExpression)(bodyNodes: ControlNode*) = {
    if (bodyNodes.length == 1)
      new While(condition, bodyNodes(0))
    else new While(condition, Sequence(bodyNodes: _*))
  }
}