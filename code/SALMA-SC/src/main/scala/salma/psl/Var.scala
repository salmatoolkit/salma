package salma.psl

import salma.simulation.SimulationContext

/**
  * Created by ckroiss on 10.12.15.
  */
class Var[T](val id: String) extends Expression[T] {
  def evaluate(ctx: SimulationContext): T = {
    val o = ctx.varMapping.get(this)
    if (!o.isDefined) throw new RuntimeException(s"No assignment for variable ${id}")
    o.get.asInstanceOf[T]
  }

  def :=[U <: T](exp: Expression[U]) = new Assign[T, U](this, exp)

  override def toString: String = s"Var(${id})"
}


object Var {
  private var nextId = 1

  def apply[T]() = {
    val r = new Var[T](s"v${nextId}")
    nextId += 1
    r
  }


}


