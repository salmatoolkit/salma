package salma.model

import scala.reflect.runtime.{universe => ru}
/**
  * Created by ckroiss on 11.12.15.
  */
case class Parameter(name : String, sort : ru.Type)
