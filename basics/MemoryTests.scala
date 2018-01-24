import spatial.dsl._
import org.virtualized._
import spatial.stdlib._
import spatial.targets._


// SRAMTest passed on arria10soc
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


object BasicDRAMReads extends SpatialApp {
  @virtualize
  def main() {
    type T = Int
    val y = ArgOut[T]
    val memSize = 64
    val tileSize = 16
    val data = loadCSV1D[T]("./basic-reads.csv", ",")
    val srcMem = DRAM[T](memSize)
    setMem(srcMem, data)

    Accel {
      val tile = SRAM[T](tileSize)
      tile load srcMem(0+tileSize::0+tileSize*2)
      y := tile(5)
    }

    val re = getArg(y)
    println("y = " + re)
  }
}


object DRAMLoadFirstEle extends SpatialApp {
  @virtualize
  def main() {
    type T = Int
    val y = ArgOut[T]
    val memSize = 64
    val tileSize = 16
    val data = loadCSV1D[T]("./basic-reads.csv", ",")
    val srcMem = DRAM[T](memSize)
    setMem(srcMem, data)

    Accel {
      val tile = SRAM[T](tileSize)
      tile load srcMem(0::0+tileSize)
      y := tile(0)
    }

    val re = getArg(y)
    println("y = " + re)
  }
}


object DRAMLoads extends SpatialApp {
  @virtualize
  def main() {
    type T = Int
    val memSize = 64
    val tileSize = 8
    val y = ArgOut[T]
    val iterSize = memSize / tileSize
    val data = loadCSV1D[T]("./1d.csv", ",")
    val srcMem = DRAM[T](memSize)
    setMem(srcMem, data)

    Accel {
      val accum = Reduce(Reg[T](0))(memSize by tileSize){ i =>
        val fpgaMem = SRAM[T](tileSize)
        fpgaMem load srcMem(i::i+tileSize)
        fpgaMem(1)
      } {_+_}
      y := accum
    }

    val re = getArg(y)
    println("result = " + re)
  }
}


object DRAM0Stores extends SpatialApp {
  @virtualize
  def main() {
    type T = Int
    val memSize = 64
    val tileSize = 16

    val x = ArgIn[T]
    val N = args(0).to[Int]
    setArg(x, N)

    val destMem0 = DRAM[T](memSize)

    Accel {
      val fpgaSrcMem0 = SRAM[T](tileSize)
      Foreach(memSize by tileSize) { i =>
        Foreach(tileSize by 1) { j =>
          fpgaSrcMem0(j) = j + 1 + x
        }

        destMem0(i::i+tileSize) store fpgaSrcMem0
      }
    }

    val re0 = getMem(destMem0)
    printArray(re0, "destMem0 = ")
  }
}


object DRAM2Stores extends SpatialApp {
  @virtualize
  def main() {
    type T = Int
    val memSize = 64
    val tileSize = 16

    val x = ArgIn[T]
    val N = args(0).to[Int]
    setArg(x, N)

    val destMem0 = DRAM[T](memSize)
    val destMem1 = DRAM[T](memSize)

    Accel {
      val fpgaSrcMem0 = SRAM[T](tileSize)
      val fpgaSrcMem1 = SRAM[T](tileSize)
      Foreach(memSize by tileSize) { i =>
        Foreach(tileSize by 1) { j =>
          fpgaSrcMem0(j) = j + 1
          fpgaSrcMem1(j) = j + x + 1
        }

        destMem0(i::i+tileSize) store fpgaSrcMem0
        destMem1(i::i+tileSize) store fpgaSrcMem1
      }
    }

    val re0 = getMem(destMem0)
    val re1 = getMem(destMem1)

    printArray(re0, "destMem0 = ")
    printArray(re1, "destMem1 = ")
  }
}


object DRAMRWTest extends SpatialApp {
  @virtualize
  def main() {
    // type T = FixPt[TRUE, _16, _16]
    type T = Int
    val memSize = 64
    val tileSize = 16

    val x = ArgIn[T]
    val N = args(0).to[Int]
    setArg(x, N)

    val data = loadCSV1D[T]("./1d.csv", ",")
    val srcmem = DRAM[T](memSize)
    val destmem = DRAM[T](memSize)

    setMem(srcmem, data)

    Accel {
      val fpgaSrcMem = SRAM[T](tileSize)
      val fpgaDestMem = SRAM[T](tileSize)
      Foreach(memSize by tileSize) { i =>
        fpgaSrcMem load srcmem(i::i+tileSize)
        Foreach(tileSize by 1) { j =>
          fpgaDestMem(j) = fpgaSrcMem(j) + x
        }

        destmem(i::i+tileSize) store fpgaDestMem
      }
    }

    val testRe = getMem(destmem)
    printArray(testRe, " Result is")
    val gold = data.map{_+N}
    printArray(gold, " Gold is")
  }
}
