import spatial.dsl._
import virtualized._
import spatial.targets._

object MemReduceTest extends SpatialApp { // Regression (Dense) // Args: 128
  override val target = AWS_F1
                                                                                                  
  val dim = 32

  val ip = 16

  type T = FixPt[TRUE,_16,_16]

  @virtualize
  def main() {
    val dramA = DRAM[T](dim)
    val dramB = DRAM[T](dim)
    val dramC = DRAM[T](dim)

    Accel{

      val sramA = SRAM[T](dim)
      val sramB = SRAM[T](dim)
      val accum = SRAM[T](dim)
      sramA load dramA(0::dim)
      sramB load dramB(0::dim)
      MemReduce(accum par ip)(dim by 1) { i =>
        val sramC = SRAM[T](dim)
        sramC(i) = sramA(i) + sramB(i)
        sramC
      } { _ + _ }
      dramC(0::dim) store accum
    }

  }
}

