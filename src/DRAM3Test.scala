import spatial.dsl._
import org.virtualized._


trait Params extends SpatialApp {
  val N = 3
  val JX = 16
  val dco = 32
  val d = 8
  val dn = 1
  val dJX = 2
  val ddco = 2
  val dd = 2
  val fogetBias = 1
  val simFileDir = "/home/tianzhao/spatial-lang/apps/np-sims/"
  val dataPaths = List(simFileDir + "/a.csv", simFileDir + "/hidden.csv", 
                    simFileDir + "/kernel.csv", simFileDir + "/bias.csv")
}


object testScalaMath extends SpatialApp {
  @virtualize
  def main() {
    type T = FltPt[_10, _10]
    val x = ArgIn[T]
    val y = ArgOut[T]
    val N = args(0).to[T]
    setArg(x, N)

    Accel {
      y := tanh(x.value)
    }

    val result = getArg(y)
    println(result)
  }
}


// At each data point in the batch:
// This app does np.concatenate([a, hidden], axis=1).dot(kernel) + bias(broadcasted)
// bias needs to broadcast N
//          dco        d                 4d               1
//   +--------------+-----+       +-----------------+    +--+
//   |              |     |       |                 |    |  |
//   |              |     |       |                 |    |  |
//   |              |     |       |                 |    |  |
// N |     a        |hidden   *   |     kernel      | +  |  |4d
//   |              |     |       |                 |    |  |
//   |              |     |       |d + dco          |    |  |
//   |              |     |       |                 |    |  |
//   +--------------+-----+       |                 |    |  |
//                                |                 |    +--+
//                                |                 |
//                                |                 |
//                                |                 |
//                                |                 |
//                                +-----------------+

// For split: i, j, f, o = np.split(linear, 4, axis=1)
object ConfigConcatAffine extends SpatialApp with Params {
  // type T = FixPt[TRUE, _32, _32]
  type T = FltPt[_10, _10]

  @virtualize
  def main() {
    val (a, hidden, kernel, bias) = (DRAM[T](N, JX, dco), DRAM[T](N, JX, d), 
                                     DRAM[T](dco+d, 4*d), DRAM[T](4*d))
    val drams = List(a, hidden, kernel, bias)
    drams.zipWithIndex.foreach { case(e, idx) =>
      setMem(e, loadCSV1D[T](dataPaths(idx), ","))
    }

    val resultDRAM = DRAM[T](N, 4*d)
    val MM = N
    val PP = d + dco
    val NN = 4 * d

    Accel {
      // TODO: Later this JX needs to be replaced by a different val...
      // TODO: for higher dimension matrices, would the alignment matter? 
      //       need to test on this matter. 

      val tileBias = SRAM[T](NN)
      tileBias load bias(0::NN)
      Foreach(MM by dn, NN by dd) { (i, j) =>
        val tileC = SRAM[T](dn, dd)

        Foreach(PP by ddco) { k =>
          val tileA = SRAM[T](dn, ddco)
          val tileB = SRAM[T](ddco, dd)
          Parallel {
            tileB load kernel(k::k+ddco, j::j+dd)
            if (k < dco) {
              tileA load a(i::i+dn, 0, k::k+ddco)
            } else {
              tileA load hidden(i::i+dn, 0, (k-dco)::(k-dco)+ddco)
            }
          }

          Foreach(dn by 1, dd by 1) { (ii, jj) =>
            // TODO: multiply add... can I replace this with shift add?
            val prod = Reduce(Reg[T])(ddco by 1) { kk => 
              tileA(ii, kk) * tileB(kk, jj)
            } {_+_}
            // val prev = mux(k == 0, 0.to[T], tileC(ii, jj))
            val prev = mux(k == 0, tileBias(j+jj), tileC(ii, jj))
            tileC(ii,jj) = prev + prod.value
          }
        }

        resultDRAM(i::i+dn, j::j+dd) store tileC
      }
    }

    val result = getMem(resultDRAM)
    printArray(result, "resultDRAM = ")
    writeCSV1D[T](result, simFileDir + "/DRAM3Test_result_bias.csv")
  }
}

// This test multiply each element in a high-dim DRAM with a constant
object DRAM3ConcatTestAugKernel extends SpatialApp with Params {
  type T = FixPt[TRUE, _16, _16] 

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