package salma.model

import scala.collection.mutable

/**
  * Created by ckroiss on 15.12.15.
  */
class World {
  private val agents : mutable.Map[Symbol, Agent] = mutable.HashMap[Symbol, Agent]()

  def add(agent : Agent) = {
    agents.put(agent.id, agent)
  }

  def all_agents = agents.values

}

object World {
  def apply() = new World()
}
