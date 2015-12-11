package salma.psl

import salma.simulation.SimulationContext

/**
  * Created by ckroiss on 10.12.15.
  */
trait Expression[+T] {
  def evaluate(ctx : SimulationContext) : T
}



