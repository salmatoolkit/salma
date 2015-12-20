package salma.psl

import salma.simulation.SimulationContext

/**
  * Created by ckroiss on 10.12.15.
  */
class Var[T](val name: Symbol) extends Expression[T] {

  def evaluate(ctx: SimulationContext): T = {
    val o = ctx.varMapping.get(name)
    if (!o.isDefined) throw new RuntimeException(s"No assignment for variable ${name}")
    o.get.asInstanceOf[T]
  }

  def :=(exp: Expression[T]) = {
    new Assign(this, exp)
  }

  override def toString: String = s"Var(${name})"
}


object Var {
  private var nextId = 1

  def apply[T]() = {
    val r = new Var[T](Symbol(s"v${nextId}"))
    nextId += 1
    r
  }


}


