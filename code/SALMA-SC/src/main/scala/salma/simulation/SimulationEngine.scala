package salma.simulation

import salma.model.{Agent, World, ActionInstance}
import salma.psl.ControlNode

import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered
import scala.util.Random

/**
  * Created by ckroiss on 10.12.15.
  */
class SimulationEngine {

  case class SimulationResult(scs: Map[Agent, SimulationContext], stepNum: Int)
  val rnd = new Random()

  def run(world: World, maxSteps: Int): SimulationResult = {
    val scs = for {
      agent <- world.all_agents
      sc = SimulationContext(List(agent.proc), Map('self -> agent))
    } yield (agent -> sc)
    runInternal(Map(scs.toList: _*), 1, maxSteps)
  }

  @tailrec
  final def runInternal(scs: Map[Agent, SimulationContext],
                        stepNum: Int, maxSteps: Int): SimulationResult = {
    if (stepNum <= maxSteps && scs.values.forall(_.nodeStack.nonEmpty)) {
      val step_results = for {
        (agent, sc) <- scs
        res = step(agent, sc)
      // todo: handle immediate action
      } yield (agent -> res)

      val all_actions = rnd.shuffle(step_results.values.flatMap(_.actions))
      val newScs = step_results mapValues (sr => sr.ctx)

      handleActions(all_actions)

      runInternal(newScs, stepNum + 1, maxSteps)
    } else {
      SimulationResult(scs, stepNum)
    }
  }

  def handleActions(actions: Iterable[ActionInstance]) = {
    for (a <- actions) {
      println(s"perform: ${a.action.name}(${a.args.mkString(", ")})")
    }
  }

  case class StepResult(ctx: SimulationContext, actions: List[ActionInstance])

  @tailrec
  final def step(agent: Agent, ctx: SimulationContext): StepResult = {
    if (ctx.nodeStack.nonEmpty) {
      val res = ctx.nodeStack.head.evaluate(ctx)
      val newStack = res.nextNodes ++ ctx.nodeStack.tail
      val newCtx = SimulationContext(newStack, res.varMapping)
      if (res.actions.nonEmpty) {
        StepResult(newCtx, res.actions)
      } else {
        step(agent, newCtx)
      }
    } else {
      StepResult(ctx, Nil)
    }

  }

}

object SimulationEngine {
  def apply() = new SimulationEngine()
}
