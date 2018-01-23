import spatial.dsl._
import org.virtualized._
import spatial.stdlib._
import spatial.targets._


object DotProductVanilla extends SpatialApp {
  @virtualize
  def main() {
    type T = Int
    val arraySize = 32
    val tileSize = 16
    val vecA = DRAM[Int](arraySize)
    val vecB = DRAM[Int](arraySize)
    setMem(vecA, Array.tabulate(arraySize){i => i})
    setMem(vecB, Array.tabulate(arraySize){i => arraySize * 2 - i})

    val y = ArgOut[T]

    Accel {
      val tileA = SRAM[T](tileSize)
      val tileB = SRAM[T](tileSize)
      val accum = Reg[T](0)
      Foreach(arraySize by tileSize) { i =>
        tileA load vecA(i::i+tileSize)
        tileB load vecB(i::i+tileSize)

        Foreach(tileSize by 1) { j =>
          accum := accum.value + tileA(j) * tileB(j)
        }
      }

      y := accum.value
    }

    println("result = " + getArg(y))
  }
}


object DotProductParMem extends SpatialApp {
  @virtualize
  def main() {
    type T = Int
    val arraySize = 32
    val tileSize = 16
    val vecA = DRAM[Int](arraySize)
    val vecB = DRAM[Int](arraySize)
    setMem(vecA, Array.tabulate(arraySize){i => i})
    setMem(vecB, Array.tabulate(arraySize){i => arraySize * 2 - i})

    val y = ArgOut[T]

    Accel {
      val tileA = SRAM[T](tileSize)
      val tileB = SRAM[T](tileSize)
      val accum = Reg[T](0)
      Foreach(arraySize by tileSize) { i =>
        Parallel {
          tileA load vecA(i::i+tileSize)
          tileB load vecB(i::i+tileSize)
        }

        Foreach(tileSize by 1) { j =>
          accum := accum.value + tileA(j) * tileB(j)
        }
      }

      y := accum.value
    }

    println("result = " + getArg(y))
  }
}
