import spatial.dsl._
import org.virtualized._


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