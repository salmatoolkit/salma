package salma.psl

import salma.simulation.SimulationContext

/**
  * Created by ckroiss on 10.12.15.
  */
trait Expression[+T] {
  def evaluate(ctx : SimulationContext) : T
  def =:=[U >: T](other : Expression[U]) : BooleanExpression = new Equals(this, other)
}



