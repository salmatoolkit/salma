package salma.simulation

import salma.model.{ActionInstance, Action}
import salma.psl.ControlNode

/**
  * Created by ckroiss on 10.12.15.
  */
case class EvaluationResult(varMapping: Map[Symbol, Any],
                            nextNodes: List[ControlNode], actions: List[ActionInstance])
