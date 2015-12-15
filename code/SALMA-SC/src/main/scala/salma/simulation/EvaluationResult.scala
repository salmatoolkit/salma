package salma.simulation

import salma.model.{ActionInstance, Action}
import salma.psl.ControlNode

/**
  * Created by ckroiss on 10.12.15.
  */
case class EvaluationResult(newContext: SimulationContext, nextNodes: List[ControlNode], actions: List[ActionInstance])
