import spatial.dsl._
import org.virtualized._
import spatial.stdlib._

object SimpleIf extends SpatialApp {

  @virtualize
  def nestedIfTest(x: Int) = {
    val in = ArgIn[Int]
    val out = ArgOut[Int]
    setArg(in, x)
    Accel {
      val sram = SRAM[Int](3)
      Foreach(0 until 3 par 3) { i =>
        if (in >= 42.to[Int]) {     // if (43 >= 42)
          sram(i) = i
        } else {
          sram(i) = i + 1
        }
      }
      out := sram(2)
    }
    getArg(out)
  }
  @virtualize
  def main() {
    val result = nestedIfTest(43)
    println("result:   " + result)
  }
}

