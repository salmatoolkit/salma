package salma.psl

import org.scalatest.FunSuite
import salma.simulation.SimulationEngine
import Implicits._

/**
  * Created by ckroiss on 09.12.15.
  */
class PSLTest extends FunSuite {
  test("Sequenz") {

    val sim = SimulationEngine()

    val x, y = NumericVar()
    val act1 = Act("act1", 1)
    val act2 = Act("act2", 2)

    val res1 = sim.run(act1, 100)
    println(s"res1: ${res1}")

    println("---\n\n")

    val s = Sequence(
      act1,
      act2,
      x := 5,
      y := x + 2 * x,
      Act("act3", x),
      Sequence(
        Act("act4", y + 2),
        Act("act5")
      ))

    val res2 = sim.run(s, 100)
    println(s"res2: ${res2}")
    assertResult(Some(Number(15.0)))(res2.ctx.varMapping.get(y))

  }
}
