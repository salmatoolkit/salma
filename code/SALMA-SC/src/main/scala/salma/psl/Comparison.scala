package salma.psl

import salma.simulation.SimulationContext

/**
  * Created by ckroiss on 11.12.15.
  */
class Comparison(val left: NumericExpression, val right: NumericExpression,
                 val op : (Double, Double) => Boolean) extends BooleanExpression {

  def evaluate(ctx: SimulationContext): Boolean = {
    op(left.evaluate(ctx).value, right.evaluate(ctx).value)
  }
}
