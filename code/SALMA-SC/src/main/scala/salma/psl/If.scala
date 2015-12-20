package salma.psl

import salma.simulation.{EvaluationResult, SimulationContext}

/**
  * Created by ckroiss on 15.12.15.
  */
class If(val condition: BooleanExpression, val thenBody: ControlNode, val elseBody: Option[ControlNode]) extends ControlNode {

  override def evaluate(context: SimulationContext): EvaluationResult = {
    if (condition.evaluate(context) == true) {
      EvaluationResult(varMapping = context.varMapping, nextNodes = List(thenBody), actions = Nil)
    } else {
      EvaluationResult(varMapping = context.varMapping, nextNodes = elseBody.toList, actions = Nil)
    }

  }

  def Else(eb: ControlNode) = new If(this.condition, this.thenBody, Some(eb))
}

object If {
  def apply(condition: BooleanExpression)(thenNodes: ControlNode*) = {
    if (thenNodes.length == 1)
      new If(condition, thenNodes(0), None)
    else new If(condition, Sequence(thenNodes: _*), None)
  }


}