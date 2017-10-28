import spatial.dsl._
import org.virtualized._


trait BaseDesign extends SpatialApp {
  // println("==========")
  // println(mm)
  // println(nn)
  // println("==========")
  var mm: Int
  var nn: Int
  var M: Int
  var N: Int
  var factor: Float

  def elementwise_matmul[T:Type:Num](a: DRAM2[T], b: DRAM2[T], c: DRAM2[T]) {
    val sram0 = SRAM[T](mm, nn)
    val sram1 = SRAM[T](mm, nn)
    val resram = SRAM[T](mm, nn)
    Foreach (M by mm, N by nn) { (i, j) =>
      sram0 load a(i::i+mm, j::j+nn)
      sram1 load b(i::i+mm, j::j+nn)
      Foreach(mm by 1, nn by 1) { (ii, jj) =>
        resram(ii, jj) = sram0(ii, jj) * sram1(ii, jj) * factor.to[T]
      }

      c(i::i+mm, j::j+nn) store resram
    }
  }
}


trait RealDesign0 extends BaseDesign {
  var mm = 3
  var nn = 4
  var M = 6
  var N = 12
  var factor = 10
}


trait RealDesign1 extends BaseDesign {
  var mm = 2
  var nn = 6
  var M = 6
  var N = 12 
  var factor = 100
}


object RealDesign0Test extends SpatialApp with RealDesign0 {
  @virtualize
  def main() {
    val paramPath = "/home/tianzhao/spatial-lang/apps/parameters/test-params/"
    val aDRAM = DRAM[Float](M, N)
    val bDRAM = DRAM[Float](M, N)
    val reDRAM = DRAM[Float](M, N)
    setMem(aDRAM, loadCSV2D[Float](paramPath+"param0.csv", ",", "\n"))
    setMem(bDRAM, loadCSV2D[Float](paramPath+"param1.csv", ",", "\n"))

    Accel {
      elementwise_matmul[Float](aDRAM, bDRAM, reDRAM)
    }

    writeCSV1D[Float](getMem(reDRAM), "RealDesign0Test.csv")
  }
}


object RealDesign1Test extends SpatialApp with RealDesign1 {
  @virtualize
  def main() {
    val paramPath = "/home/tianzhao/spatial-lang/apps/parameters/test-params/"
    val aDRAM = DRAM[Float](M, N)
    val bDRAM = DRAM[Float](M, N)
    val reDRAM = DRAM[Float](M, N)
    setMem(aDRAM, loadCSV2D[Float](paramPath+"param0.csv", ",", "\n"))
    setMem(bDRAM, loadCSV2D[Float](paramPath+"param1.csv", ",", "\n"))

    Accel {
      elementwise_matmul[Float](aDRAM, bDRAM, reDRAM)
    }

    writeCSV1D[Float](getMem(reDRAM), "RealDesign1Test.csv")
  }    
}


object WriteCSV3DTest extends SpatialApp {
  @virtualize
  def main() {
    val N = 3
    val M = 4
    val P = 5

    val resultDRAM = DRAM[Int](N, M, P)

    Accel {
      val resultSRAM = SRAM[Int](N, M, P)
      val reg = Reg[Int](0)
      Foreach(N by 1, M by 1, P by 1) { (i, j, k) =>
        reg := reg.value + 1
        resultSRAM(i, j, k) = reg
      }

      resultDRAM(0::N, 0::M, 0::P) store resultSRAM
    }

    val re = getMem(resultDRAM)
    writeCSV1D[Int](re, "./resultDRAM.csv")    
  }
}


object ReadCSV3DTest extends SpatialApp {
  @virtualize
  def main() {
    val N = 3
    val M = 4
    val P = 5

    val inputDRAM = DRAM[Int](N, M, P)
    setMem(inputDRAM, loadCSV1D[Int]("./resultDRAM.csv", ","))

    Accel {
      val inputSRAM = SRAM[Int](N, M, P)
      inputSRAM load inputDRAM(0::N, 0::M, 0::P)
      Foreach(N by 1, M by 1, P by 1) { (i, j, k) =>
        println(i)
        println(j)
        println(k)
        println(inputSRAM(i, j, k))
        println("==========")
      }
    }
  }
}


object ActivationTests extends Activations {
  type T = FixPt[TRUE, _8, _8]
  @virtualize
  def main() {
    val x = ArgIn[T]
    val y = ArgOut[T]
    val y1 = ArgOut[T]
    val N = args(0).to[T]
    setArg(x, N)

    // Try: 0.09800561, 0.12712223
    Accel {
      Parallel {
        // y := tanh_(x.value)
        y := tanh_(x.value)   // numpy would give: 0.097693026355715917, 0.126441859911746
        y1 := sigmoid_(x.value) // numpy would give: 0.52448180978452119, 0.53173782856894825
      }
    }

    val yre = getArg(y)
    val yre1 = getArg(y1)
    println(yre)
    println(yre1)
  }
}


// This test multiply each element in a high-dim DRAM with a constant
object DRAM3ConcatTestAugKernel extends SpatialApp with Params {
  type T = FixPt[TRUE, _8, _8] 

  @virtualize
  def main() {
    val (a, hidden, kernel, bias) = (DRAM[T](N, JX, dco), DRAM[T](N, JX, d), 
                                     DRAM[T](dco+d, 4*d), DRAM[T](4*d))
    val drams = List(a, hidden, kernel, bias)
    drams.zipWithIndex.foreach { case(e, idx) =>
      setMem(e, loadCSV1D[T](dataPaths(idx), ","))
    } 

    val rk = DRAM[T](dco+d, 4*d)

    Accel {
      val tileA = SRAM[T](ddco, dd)
      val tileB = SRAM[T](ddco, dd)
      Foreach(dco+d by ddco, 4*d by dd) { (i,j) =>
        tileA load kernel(i::i+ddco, j::j+dd)
        Foreach(ddco by 1, dd by 1) { (ii,jj) =>
          tileB(ii,jj) = tileA(ii,jj) * 2
        }

        rk(i::i+ddco, j::j+dd) store tileB
      }
    } 

    val rkresult = getMem(rk)
    printArray(rkresult, "Result: ")
  }
}