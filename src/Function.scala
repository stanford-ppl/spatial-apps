import org.virtualized._
import spatial._

object Function extends SpatialApp {
  import IR._


  @virtualize
  def main() {
    val x = ArgIn[Int]


    Accel {

    }
    def f(y:Int) =
      y+y+y+y+y

    val test = f(x)
    val fu = fun(f)
    val test2 = fu(x)
    val test3 = fu(x)
    println("result: " + test + test2 + test3)
  }
}
