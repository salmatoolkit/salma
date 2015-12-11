package salma.simulation

import salma.model.ActionInstance
import salma.psl.ControlNode

import scala.annotation.tailrec

/**
  * Created by ckroiss on 10.12.15.
  */
class SimulationEngine {

  case class SimulationResult(ctx: SimulationContext, stepNum: Int)

  def run(procedure: ControlNode, maxSteps: Int): SimulationResult = {
    val ctx = SimulationContext(Map.empty)
    val res = runInternal(ctx, List(procedure), 1, maxSteps)
    SimulationResult(res.ctx, res.step)
  }

  @tailrec
  final def runInternal(ctx: SimulationContext, nodeStack: List[ControlNode], stepNum: Int, maxSteps: Int): StepResult = {
    if (stepNum <= maxSteps && nodeStack.nonEmpty) {
      val res = step(ctx, nodeStack, stepNum, maxSteps)
      if (res.actions.nonEmpty)
        handleActions(res.actions, res.ctx)
      runInternal(res.ctx, res.nodeStack, stepNum + 1, maxSteps)
    } else {
      StepResult(stepNum, ctx, nodeStack, Nil)
    }
  }

  def handleActions(actions: List[ActionInstance], ctx: SimulationContext) = {
    for (a <- actions) {
      println(s"perform: ${a.action.name}(${a.args.mkString(", ")})")
    }
  }

  case class StepResult(step: Int, ctx: SimulationContext, nodeStack: List[ControlNode], actions: List[ActionInstance])

  @tailrec
  final def step(ctx: SimulationContext, nodeStack: List[ControlNode], stepNum: Int, maxSteps: Int): StepResult = {
    if (stepNum <= maxSteps && nodeStack.nonEmpty) {
      val res = nodeStack.head.evaluate(ctx)
      val newStack = if (res.nextNode.isDefined) res.nextNode.get :: nodeStack else nodeStack.tail
      if (res.actions.nonEmpty) {
        StepResult(stepNum, res.newContext, newStack, res.actions)
      } else {
        step(res.newContext, newStack, stepNum + 1, maxSteps)
      }
    } else {
      StepResult(stepNum, ctx, nodeStack, Nil)
    }

  }

}

object SimulationEngine {
  def apply() = new SimulationEngine()
}
