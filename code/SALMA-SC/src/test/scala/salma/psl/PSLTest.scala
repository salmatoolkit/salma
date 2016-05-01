package salma.psl

import org.scalatest.FunSuite
import salma.model.World
import salma.psl.Actions._
import salma.psl.Agents._
import salma.psl.Implicits._
import salma.simulation.SimulationEngine

/**
  * Created by ckroiss on 09.12.15.
  */
class PSLTest extends FunSuite {


  class SimpleRobot(val id: Symbol) extends Robot {

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
        While(z < 10)(
          self act raiseArm(z),
          z := z + 1
        )
      )
    }
  }


  test("Sequence") {

    val sim = SimulationEngine()

    val rob1 = new SimpleRobot('rob1)
    val rob2 = new SimpleRobot('rob2)
    val rob3 = new SimpleRobot('rob3)

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
