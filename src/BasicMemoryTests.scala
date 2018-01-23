import spatial.dsl._
import org.virtualized._
import spatial.stdlib._
import spatial.targets._


// These tests are on 1 stream
// 1 Burst, 1 sram that's the same size of dram.
// Write to the sram and store to dram.
object OneBurstOneStore extends SpatialApp {
  @virtualize
  def main() {
    type T = Int
    val memSize = 16
    val srcMem = DRAM[T](memSize)

    val x = ArgIn[Int]
    setArg(x, args(0).to[Int])

    Accel {
      val fpgaMem = SRAM[T](memSize)
      Foreach(memSize by 1) { i =>
        fpgaMem(i) = i + 1302 + x
      }

      srcMem(0::0+memSize) store fpgaMem
    }

    val result = getMem(srcMem)
    val gold = Array.tabulate(memSize){i => i + 1302 + args(0).to[Int]}

    printArray(result, "result = ")
    printArray(gold, "gold = ")
  }
}


object OneBurstOneLoad extends SpatialApp { // Cannot read the first element in a burst?
  @virtualize
  def main() {
    type T = Int
    val memSize = 16
    val x = ArgIn[T]
    val y0 = ArgOut[T]
    val y1 = ArgOut[T]
    val y2 = ArgOut[T]
    val y3 = ArgOut[T]
    val y4 = ArgOut[T]
    val y5 = ArgOut[T]
    val y6 = ArgOut[T]
    val y7 = ArgOut[T]
    val y8 = ArgOut[T]
    val y9 = ArgOut[T]
    val y10 = ArgOut[T]
    val y11 = ArgOut[T]
    val y12 = ArgOut[T]
    val y13 = ArgOut[T]
    val y14 = ArgOut[T]
    val y15 = ArgOut[T]
    val srcMem = DRAM[T](memSize)
    val data = loadCSV1D[T]("./1burst.csv", ",")
    setMem(srcMem, data)
    setArg(x, args(0).to[T])

    Accel {
      val fpgaMem = SRAM[T](memSize)
      fpgaMem load srcMem(0::0+memSize)
      y0 := fpgaMem(0) + x
      y1 := fpgaMem(1) + x
      y2 := fpgaMem(2) + x
      y3 := fpgaMem(3) + x
      y4 := fpgaMem(4) + x
      y5 := fpgaMem(5) + x
      y6 := fpgaMem(6) + x
      y7 := fpgaMem(7) + x
      y8 := fpgaMem(8) + x
      y9 := fpgaMem(9) + x
      y10 := fpgaMem(10) + x
      y11 := fpgaMem(11) + x
      y12 := fpgaMem(12) + x
      y13 := fpgaMem(13) + x
      y14 := fpgaMem(14) + x
      y15 := fpgaMem(15) + x
    }

    println("y0 = " +  getArg(y0))
    println("y1 = " +  getArg(y1))
    println("y2 = " +  getArg(y2))
    println("y3 = " +  getArg(y3))
    println("y4 = " +  getArg(y4))
    println("y5 = " +  getArg(y5))
    println("y6 = " +  getArg(y6))
    println("y7 = " +  getArg(y7))
    println("y8 = " +  getArg(y8))
    println("y9 = " +  getArg(y9))
    println("y10 = " +  getArg(y10))
    println("y11 = " +  getArg(y11))
    println("y12 = " +  getArg(y12))
    println("y13 = " +  getArg(y13))
    println("y14 = " +  getArg(y14))
    println("y15 = " +  getArg(y15))
  }
}


// This app fills SRAMs with argins and initiate a burst store
object OneBurstOneStoreArgs extends SpatialApp {
  def main() {
    type T = Int
    val memSize = 16
    val srcMem = DRAM[T](memSize)
    val x0 = ArgIn[T]
    // val x1 = ArgIn[T]
    // val x2 = ArgIn[T]
    // val x3 = ArgIn[T]
    // val x4 = ArgIn[T]
    // val x5 = ArgIn[T]
    // val x6 = ArgIn[T]
    // val x7 = ArgIn[T]
    // val x8 = ArgIn[T]
    // val x9 = ArgIn[T]
    // val x10 = ArgIn[T]
    // val x11 = ArgIn[T]
    // val x12 = ArgIn[T]
    // val x13 = ArgIn[T]
    // val x14 = ArgIn[T]
    // val x15 = ArgIn[T]
    setArg(x0, args(0).to[T])
    // setArg(x1, args(1).to[T])
    // setArg(x2, args(2).to[T])
    // setArg(x3, args(3).to[T])
    // setArg(x4, args(4).to[T])
    // setArg(x5, args(5).to[T])
    // setArg(x6, args(6).to[T])
    // setArg(x7, args(7).to[T])
    // setArg(x8, args(8).to[T])
    // setArg(x9, args(9).to[T])
    // setArg(x10, args(10).to[T])
    // setArg(x11, args(11).to[T])
    // setArg(x12, args(12).to[T])
    // setArg(x13, args(13).to[T])
    // setArg(x14, args(14).to[T])
    // setArg(x15, args(15).to[T])

    Accel {
      val fpgaMem = SRAM[T](memSize)
      fpgaMem(x0.value) = x0 + 1200
      srcMem(0::0+memSize) store fpgaMem
    }

    val result = getMem(srcMem)
    printArray(result, "array = ")
  }
}

// Want to check if it's the sram writes that's not getting correct or the burst. If sram writes, then there should be 2 zeros. If burst, then should be one.
object OneBurstTwoStore extends SpatialApp {
  @virtualize
  def main() {
    type T = Int
    val memSize = 16
    val tileSize = 8
    val srcMem = DRAM[T](memSize)

    val x = ArgIn[Int]
    setArg(x, args(0).to[Int])

    Accel {
      val fpgaMem = SRAM[T](tileSize)
      Foreach(memSize by tileSize) { i =>
        Foreach(tileSize by 1) { j =>
          fpgaMem(j) = i + j + 1302 + x
        }

        srcMem(i::i+tileSize) store fpgaMem
      }
    }

    val result = getMem(srcMem)
    val gold = Array.tabulate(memSize){i => i + 1302 + args(0).to[Int]}

    printArray(result, "result = ")
    printArray(gold, "gold = ")
  }
}

// Want to check what happens with the burst boundary
object TwoBurst extends SpatialApp { // Error: Last element in the burst is not shown.
  @virtualize
  def main() {
    type T = Int
    val memSize = 32
    val srcMem = DRAM[T](memSize)

    val x = ArgIn[Int]
    setArg(x, args(0).to[Int])

    Accel {
      val fpgaMem = SRAM[T](memSize)
      Foreach(memSize by 1) { i =>
        fpgaMem(i) = i + 1302 + x
      }

      srcMem(0::0+memSize) store fpgaMem
    }

    val result = getMem(srcMem)
    val gold = Array.tabulate(memSize){i => i + 1302 + args(0).to[Int]}

    printArray(result, "result = ")
    printArray(gold, "gold = ")
  }
}



object OneBurstTwoLoad extends SpatialApp {
  @virtualize
  def main() {
    type T = Int
    val memSize = 16
    val tileSize = 8
    val srcMem = DRAM[T](memSize)

    val x = ArgIn[Int]
    val y0 = ArgOut[Int]
    val y1 = ArgOut[Int]
    setArg(x, args(0).to[Int])
    val data = loadCSV1D[T]("./1burst.csv")
    setMem(srcMem, data)

    Accel {
      val fpgaMem = SRAM[T](tileSize)

      Pipe {
        fpgaMem load srcMem(0::0+tileSize)
        y0 := fpgaMem(x)
        fpgaMem load srcMem(tileSize::memSize)
        y1 := fpgaMem(x)
      }
    }

    val y0_re = getArg(y0)
    val y1_re = getArg(y1)
    println("y0 = " + y0_re)
    println("y1 = " + y1_re)
  }
}


object OneBurstOneLoadRegFile extends SpatialApp { // Try using regfile to isolate the difference
  @virtualize
  def main() {
    type T = Int
    val memSize = 16
    val x = ArgIn[T]
    val y0 = ArgOut[T]
    val y1 = ArgOut[T]
    val y2 = ArgOut[T]
    val y3 = ArgOut[T]
    val y4 = ArgOut[T]
    val y5 = ArgOut[T]
    val y6 = ArgOut[T]
    val y7 = ArgOut[T]
    val y8 = ArgOut[T]
    val y9 = ArgOut[T]
    val y10 = ArgOut[T]
    val y11 = ArgOut[T]
    val y12 = ArgOut[T]
    val y13 = ArgOut[T]
    val y14 = ArgOut[T]
    val y15 = ArgOut[T]
    val srcMem = DRAM[T](memSize)
    val data = loadCSV1D[T]("./1burst.csv", ",")
    setMem(srcMem, data)
    setArg(x, args(0).to[T])

    Accel {
      val fpgaMem = RegFile[T](memSize)
      fpgaMem load srcMem(0::0+memSize)
      y0 := fpgaMem(0) + x
      y1 := fpgaMem(1) + x
      y2 := fpgaMem(2) + x
      y3 := fpgaMem(3) + x
      y4 := fpgaMem(4) + x
      y5 := fpgaMem(5) + x
      y6 := fpgaMem(6) + x
      y7 := fpgaMem(7) + x
      y8 := fpgaMem(8) + x
      y9 := fpgaMem(9) + x
      y10 := fpgaMem(10) + x
      y11 := fpgaMem(11) + x
      y12 := fpgaMem(12) + x
      y13 := fpgaMem(13) + x
      y14 := fpgaMem(14) + x
      y15 := fpgaMem(15) + x
    }

    println("y0 = " +  getArg(y0))
    println("y1 = " +  getArg(y1))
    println("y2 = " +  getArg(y2))
    println("y3 = " +  getArg(y3))
    println("y4 = " +  getArg(y4))
    println("y5 = " +  getArg(y5))
    println("y6 = " +  getArg(y6))
    println("y7 = " +  getArg(y7))
    println("y8 = " +  getArg(y8))
    println("y9 = " +  getArg(y9))
    println("y10 = " +  getArg(y10))
    println("y11 = " +  getArg(y11))
    println("y12 = " +  getArg(y12))
    println("y13 = " +  getArg(y13))
    println("y14 = " +  getArg(y14))
    println("y15 = " +  getArg(y15))
  }
}
// Allocate one piece of memory, and perform two writes using one sram
// object OneBurstTwoStores extends SpatialApp {
//   def main() {
//     type T = Int
//     val x = ArgIn[T]
//     val N = args(0).to[T]
//     val memSize = 16
//     val tileSize = 8
//     val srcMem = DRAM[T](memSize)
//
//     setArg(x, N)
//     Accel {
//       val fpgaMem = SRAM[T](tileSize)
//       Foreach (memSize by tileSize) { i =>
//         Foreach (tileSize by 1) { j =>
//           fpgaMem(j) = (i + 1) * (x + j)
//         }
//
//         srcMem(i::i+tileSize) store fpgaMem
//       }
//     }
//
//     val re = getMem(srcMem)
//     printArray(re, "result = ")
//   }
// }

// object 1Burst2Loads extends SpatialApp {
//
// }
//
//
// object 2BurstsLoads extends SpatialApp {
//
// }
//
// object 2BurstsStores extends SpatialApp {
//
// }
