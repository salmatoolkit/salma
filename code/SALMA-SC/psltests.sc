import salma.psl._
import salma.psl.Implicits._
import salma.simulation.SimulationContext

val v = Sequence(Act("test"))

val exp1 : NumericExpression = 42 + 4

val s = 'x

val x, y = NumericVar()

val ctx = SimulationContext(Map(x -> Number(4)))


val sum2 : NumericExpression = exp1 + 5.3 + x

sum2.evaluate(ctx)

val a = Assign(x, 12)
a.evaluate(ctx)

val exp2 : NumericExpression = x + 4 * x

exp2.evaluate(ctx)

(x := 4.0).evaluate(ctx)

for (i <- 1 to 5) {
  println(i)
}

List(1,2,3).mkString(", ")


















