import spatial.dsl._
import virtualized._
import spatial.targets._

object MemReduceTest extends SpatialApp { // Regression (Dense) // Args: 128
  override val target = AWS_F1
                                                                                                  
  val dim = 64

  val ts = 16
  val ip = 16

  type T = FixPt[TRUE,_16,_16]

  @virtualize
  def main() {
    val dramA = DRAM[T](dim)
    val dramB = DRAM[T](dim)
    val dramC = DRAM[T](ts)

    Accel{

      Foreach(2 by 1) { k =>
        val sramA = SRAM[T](ts)
        val sramB = SRAM[T](ts)
        val accum = SRAM[T](ts)
        MemReduce(accum par ip)(dim by ts) { j =>
          sramA load dramA(j::j+dim)
          sramB load dramB(j::j+dim)
          val sramC = SRAM[T](ts)
          Foreach(ts by 1 par ip) { i =>
            sramC(i) = sramA(i) * sramB(i)
          }
          sramC
        } { _ + _ }
        dramC(0::ts) store accum
      }
    }

  }
}

