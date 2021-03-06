import org.virtualized._
import spatial.dsl._

object ArgInOut extends SpatialApp {
  @virtualize
  def main() {
    val x = ArgIn[Int]
    val y = ArgOut[Int]
    val N = args(0).to[Int]

    setArg(x, N)

    Accel {
      y := x + 4
    }

    val result = getArg(y)
    val gold = N + 4
    println("expected: " + gold)
    println("result: " + result)
  }
}







object TextTest extends SpatialApp {
  def test(x: String): String = x

  @virtualize def main(): Unit = {
    val m = "120"
    val i = random[Int]
    val q = m(i)
    val y = test(q)

    println(y.toText)
  }
}