package salma.psl

import salma.simulation.SimulationContext

/**
  * Created by ckroiss on 11.12.15.
  */
class Conjunction(val left : BooleanExpression, right : BooleanExpression) extends BooleanExpression {

  override def evaluate(ctx: SimulationContext): Boolean = {
    val v1 = left.evaluate(ctx)
    val v2 = right.evaluate(ctx)
    v1 && v2
  }
}
