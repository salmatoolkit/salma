package salma.model

import salma.psl.{AgentVar, ControlNode, Act}
import scala.reflect.runtime.{universe => ru}

/**
  * Created by ckroiss on 15.12.15.
  */
trait Agent {

  def id : Symbol
  def sort : String
  def proc : ControlNode
  def self : AgentVar[this.type] = new AgentVar[this.type]('self)

  override def toString = s"Agent[${id}: ${sort}]"

}
