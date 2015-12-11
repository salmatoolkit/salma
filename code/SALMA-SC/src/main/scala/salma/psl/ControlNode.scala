package salma.psl

import salma.simulation.{EvaluationResult, SimulationContext}


/**
  * Created by ckroiss on 09.12.15.
  */
trait ControlNode {
  def evaluate(context : SimulationContext) : EvaluationResult
}

