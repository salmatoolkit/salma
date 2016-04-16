package salma.psl

import salma.simulation.SimulationContext

/**
  * Created by ckroiss on 10.12.15.
  */
class Sum(val left: NumericExpression, val right: NumericExpression) extends NumericExpression {

  def evaluate(ctx: SimulationContext): Number = {
    new Number(left.evaluate(ctx).value + right.evaluate(ctx).value)
  }

  override def toString: String = s"Sum(${left}, ${right})"
}
