package salma.psl

/**
  * Created by ckroiss on 10.12.15.
  */
trait NumericExpression extends Expression[Number] {
  def +(other : NumericExpression) = new Sum(this, other)
  def *(other : NumericExpression) = new Product(this, other)

  def >(other : NumericExpression) = new Comparison(this, other, _ > _)
  def >=(other : NumericExpression) = new Comparison(this, other, _ >= _)

  def <(other : NumericExpression) = new Comparison(this, other, _ < _)
  def <=(other : NumericExpression) = new Comparison(this, other, _ <= _)

}

