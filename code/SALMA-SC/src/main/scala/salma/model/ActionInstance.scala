package salma.model

import salma.psl.Expression

/**
  * Created by ckroiss on 11.12.15.
  */
case class ActionInstance(val actionName : String, val args : List[Expression[Any]])
