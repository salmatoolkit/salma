package salma.psl

import salma.simulation.SimulationContext

/**
  * Created by ckroiss on 10.12.15.
  */
object Implicits {

  implicit def wrapInt(x: Int) = new NumberLiteral(x)

  implicit def wrapDouble(x: Double) = new NumberLiteral(x)

  implicit def wrapBoolean(b : Boolean) = new BooleanLiteral(b)
}
