package salma.psl

import salma.model.{Parameter, Action}
import salma.psl.Agents._
import scala.reflect.runtime.{universe => ru}

/**
  * Created by ckroiss on 20.12.15.
  */
object Actions {
  val moveAction = Action("move", Parameter("self", ru.typeOf[MovableAgent]), Parameter("dx", ru.typeOf[Int]),
    Parameter("dy", ru.typeOf[Int]))
  val swimAction = Action("swim", Parameter("self", ru.typeOf[SwimmingAgent]), Parameter("dx", ru.typeOf[Int]),
    Parameter("dy", ru.typeOf[Int]))
  val raiseArmAction = Action("raiseArm", Parameter("self", ru.typeOf[Robot]), Parameter("h", ru.typeOf[Int]))

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


}
