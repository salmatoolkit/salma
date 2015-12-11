package salma.psl

/**
  * Created by ckroiss on 10.12.15.
  */
class NumericVar(id: String) extends Var[Number](id) with NumericExpression {

}

object NumericVar {
  private var nextId = 1

  def apply() = {
    val r = new NumericVar(s"nv${nextId}")
    nextId += 1
    r
  }


}