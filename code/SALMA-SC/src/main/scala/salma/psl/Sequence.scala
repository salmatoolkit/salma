package salma.psl

import salma.simulation.{EvaluationResult, SimulationContext}

/**
  * Created by ckroiss on 10.12.15.
  */
class Sequence(children: ControlNode*) extends ControlNode {
  private var index = 0

  def evaluate(context: SimulationContext) = {
    if (index < children.length) {
      val res = EvaluationResult(nextNode = Some(children(index)), newContext = context, actions = Nil)
      index = index + 1
      res
    }
    else {
      index = 0
      EvaluationResult(context, None, Nil)
    }

  }
}

object Sequence {
  def apply(c : ControlNode*) = new Sequence(c: _*)
}
