package salma.psl

import salma.simulation.SimulationContext

/**
  * Created by ckroiss on 11.12.15.
  */
class Equals(val left: Expression[Any], val right: Expression[Any]) extends BooleanExpression {

  def evaluate(ctx: SimulationContext) = {
    left.evaluate(ctx) == right.evaluate(ctx)
  }

}


