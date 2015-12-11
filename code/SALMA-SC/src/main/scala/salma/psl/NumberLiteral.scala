package salma.psl

import salma.simulation.SimulationContext

/**
  * Created by ckroiss on 10.12.15.
  */
class NumberLiteral(val value: Double) extends NumericExpression {
  def evaluate(ctx: SimulationContext): Number = new Number(value)

  override def toString: String = s"NumberLiteral(${value})"
}
