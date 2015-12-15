package salma.psl

import org.scalatest.FunSuite
import salma.model.{Agent, ActionTemplate, Parameter, Action}
import salma.simulation.SimulationEngine
import Implicits._

/**
  * Created by ckroiss on 09.12.15.
  */
class PSLTest extends FunSuite {

  class Robot(id: String) extends Agent[Robot](id) {
    override def sort = "robot"
  }

  val moveAction = Action("move", Parameter("dx", Int.getClass), Parameter("dy", Int.getClass))

  case class move(dx: NumericExpression, dy: NumericExpression) extends ActionTemplate[Robot] {
    def action = moveAction

    def args = Vector(dx, dy)
  }


  test("Sequenz") {

    val sim = SimulationEngine()

    val x, y = NumericVar()
    val a1 = Action("act1", Parameter("x", Int.getClass))
    val a2 = Action("act2", Parameter("b", Boolean.getClass))

    val self = new Robot("rob1")

    val res1 = sim.run(self act move(10, 20), 100)

    println(s"res1: ${res1}")

    println("---\n\n")


    val s = Sequence(
      x := 5,
      y := x + 2 * x,
      self act move(11, 21),
      If(y > 11) {
        Sequence(
          self act move(12, 22),
          self act move(13, 23)
        )
      },
      If(x < 5) {
        self act move(100, 200)
      } Else {
        self act move(200, 100)
      }
    )

    val res2 = sim.run(s, 100)
    println(s"res2: ${res2}")
    assertResult(Some(Number(15.0)))(res2.ctx.varMapping.get(y))

  }


}
