import spatial.dsl._
import org.virtualized._
import spatial.stdlib._
import spatial.targets._


object SRAMRWTest extends SpatialApp {
  @virtualize
  def main() {
    type T = Int
    val memSize = 16

    val x = ArgIn[Int]
    val y = ArgOut[Int]
    val N = args(0).to[Int]

    setArg(x, N)

    Accel {
      Pipe {
        val fpgaMem = SRAM[T](memSize)
        Foreach(memSize by 1) { i =>
          fpgaMem(i) = i + 1
        }

        // For fixing, not setting the par.
        val sum = Reduce(Reg[T](0))(memSize by 1) { i =>
          fpgaMem(i)
        }{_+_}

        y := sum + x
      }
    }

    val result = getArg(y)
    val gold = N + Array.tabulate(memSize){i => i + 1}.reduce{_+_}
    println("result = " + result)
    println("gold = " + gold)
  }
}


object DRAMWriteTest extends SpatialApp {
  @virtualize
  def main() {
    type T = Int
    val memSize = 16
    val re = DRAM[T](memSize)

    Accel {
      val fpgaMem = SRAM[T](memSize)
      Foreach(memSize by 1) { i =>
        fpgaMem(i) = i
      }

      re(0::0+memSize) store fpgaMem
    }

    val testRe = getMem(re)
    printArray(testRe, " Result is")
  }
}
