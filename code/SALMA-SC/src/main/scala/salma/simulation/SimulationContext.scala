package salma.simulation

import salma.psl.{Var, ControlNode}

/**
  * Created by ckroiss on 09.12.15.
  */
case class SimulationContext(nodeStack: List[ControlNode], varMapping : Map[Symbol, Any])

