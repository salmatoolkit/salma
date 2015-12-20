package salma.psl

import org.scalatest.FunSuite
import salma.model.{Action, Agent, Parameter, World}
import salma.psl.Implicits._
import salma.simulation.SimulationEngine

/**
  * Created by ckroiss on 09.12.15.
  */
class PSLTest extends FunSuite {


  trait MovableAgent extends Agent {

  }

  trait SwimmingAgent extends Agent {

  }

  class Robot(val id: Symbol) extends MovableAgent {
    def sort = "robot"

    def proc = {
      val x, y, z = NumericVar()
      Sequence(
        x := 5,
        y := x + 2 * x,
        self act move(11, 21),
        If(y > 11)(
          self act move(12, 22),
          self act move(13, 23)
        ),
        If(x < 5)(
          self act move(100, 200)
        ) Else (
          self act move(200, 100)
          )
        //,
        //self act swim(50, 50)
        , z := 0,
        While(z < 10) (
          self act raiseArm(z),
          z := z + 1
        )
      )
    }
  }


  val moveAction = Action("move", Parameter("self", "MovableAgent"), Parameter("dx", "int"), Parameter("dy", "int"))
  val swimAction = Action("swim", Parameter("self", "SwimmingAgent"), Parameter("dx", "int"), Parameter("dy", "int"))
  val raiseArmAction = Action("raiseArm", Parameter("self", "Robot"), Parameter("h", "int"))

  case class move(dx: NumericExpression,
                  dy: NumericExpression) extends ActionTemplate[MovableAgent] {
    def action = moveAction

    def args = Vector(dx, dy)
  }

  case class swim(self: AgentExpression[SwimmingAgent], dx: NumericExpression,
                  dy: NumericExpression) extends ActionTemplate[SwimmingAgent] {
    def action = swimAction

    def args = Vector(dx, dy)
  }

  case class raiseArm(h: NumericExpression) extends ActionTemplate[Robot] {
    def action = raiseArmAction

    def args = Vector(h)
  }


  test("Sequenz") {

    val sim = SimulationEngine()

    val rob1 = new Robot('rob1)
    val rob2 = new Robot('rob2)
    val rob3 = new Robot('rob3)

    val world = World()
    world add rob1
    world add rob2
    world add rob3
    val res2 = sim.run(world, 100)
    println(s"res2: ${res2}")
    //assertResult(Some(Number(15.0)))(res2.scs(rob1).varMapping('y))
    println(res2.scs(rob1).varMapping)
  }


}
