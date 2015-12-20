package salma.psl

import salma.model.Agent

/**
  * Created by ckroiss on 20.12.15.
  */
class AgentVar[A <: Agent](id: Symbol) extends Var[A](id) with AgentExpression[A] {

}

object AgentVar {
  private var curId = 1

  def apply[A <: Agent](id: Symbol = null) = {
    val theId: Symbol = if (id == null) {
      curId += 1
      Symbol(s"av${curId}")
    } else id

    val r = new AgentVar[A](theId)
    r
  }


}