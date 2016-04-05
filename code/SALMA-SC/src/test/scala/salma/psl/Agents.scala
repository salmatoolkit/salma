package salma.psl

import salma.model.Agent

/**
  * Created by ckroiss on 20.12.15.
  */
object Agents {

  trait MovableAgent extends Agent {
    def sort: String = "movableAgent"
  }

  trait SwimmingAgent extends Agent {
    def sort: String = "swimmingAgent"
  }

  trait Robot extends MovableAgent {
    override def sort: String = "robot"
  }

}
