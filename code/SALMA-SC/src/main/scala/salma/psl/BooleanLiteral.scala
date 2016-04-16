package salma.psl

import salma.simulation.SimulationContext

/**
  * Created by ckroiss on 11.12.15.
  */
class BooleanLiteral(val value : Boolean) extends BooleanExpression {
  def evaluate(ctx: SimulationContext) = value
}
