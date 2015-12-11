package salma.psl

import org.scalatest.FunSuite
import salma.model.{Parameter, Action}
import salma.simulation.SimulationEngine
import Implicits._

/**
  * Created by ckroiss on 09.12.15.
  */
class PSLTest extends FunSuite {
  test("Sequenz") {

    val sim = SimulationEngine()

    val x, y = NumericVar()
    val a1 = Action("act1", Parameter("x", Int.getClass))
    val a2 = Action("act2", Parameter("b", Boolean.getClass))

    val res1 = sim.run(Act(a1, 42), 100)
    println(s"res1: ${res1}")

    println("---\n\n")

    val s = Sequence(
      x := 5,
      y := x + 2 * x,
      Act(a1, x),
      Sequence(
        Act(a1, y + 2),
        Act(a2, true)
      ))

    val res2 = sim.run(s, 100)
    println(s"res2: ${res2}")
    assertResult(Some(Number(15.0)))(res2.ctx.varMapping.get(y))

  }
}
