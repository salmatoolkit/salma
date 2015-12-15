package salma.psl

import salma.simulation.{EvaluationResult, SimulationContext}

/**
  * Created by ckroiss on 15.12.15.
  */
class If(val condition: BooleanExpression, val thenBody: ControlNode, val elseBody: Option[ControlNode]) extends ControlNode {

  override def evaluate(context: SimulationContext): EvaluationResult = {
    if (condition.evaluate(context) == true) {
      EvaluationResult(nextNodes = List(thenBody), newContext = context, actions = Nil)
    } else {
      EvaluationResult(nextNodes = elseBody.toList, newContext = context, actions = Nil)
    }

  }

  def Else(eb: ControlNode) = new If(this.condition, this.thenBody, Some(eb))
}

object If {
  def apply(condition: BooleanExpression)(thenBody: ControlNode) = {
    new If(condition, thenBody, None)
  }
}