package salma.psl

import salma.simulation.{EvaluationResult, SimulationContext}

/**
  * Created by ckroiss on 10.12.15.
  */
class Assign[T, U <: T](val v: Var[T], val exp: Expression[U]) extends ControlNode {
  def evaluate(context: SimulationContext): EvaluationResult = {
    val res = exp.evaluate(context)
    val newContext = context.copy(varMapping = context.varMapping + (v -> res))
    EvaluationResult(newContext = newContext, nextNode = None, Nil)
  }
}

object Assign {
  def apply[T, U <: T](v: Var[T], exp: Expression[U]) = new Assign(v, exp)

}
