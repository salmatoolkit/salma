import salma.psl._
import salma.psl.Implicits._
import salma.simulation.SimulationContext

//val v = Sequence(Act("test"))

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
(sum2 =:= 55.3).evaluate(ctx)
(sum2 > 4).evaluate(ctx)
val z = Var[Any]
(z := x).evaluate(ctx)

val m = List(1,2,3) zip List(4,5,6)
val m2 = Map(m: _*)

val o = Option(4)
o.toList

val o2 = None

o2.toList
o2 == Nil

val o3 = List(1,2,3)
val o4 = o3 ++ List(5,6) ++ Nil
o4.head















