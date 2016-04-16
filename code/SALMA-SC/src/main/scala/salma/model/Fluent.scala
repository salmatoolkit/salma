package salma.model

import scala.reflect.runtime.{universe => ru}

/**
  * Created by ckroiss on 20.12.15.
  */
case class Fluent (name :String, sort : ru.Type, params : Parameter*)

