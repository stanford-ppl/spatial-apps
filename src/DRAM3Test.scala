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
  val forgetBias = 1
  val simFileDir = "/home/tianzhao/spatial-lang/apps/np-sims/"
  val dataPaths = List(simFileDir + "/a.csv", simFileDir + "/hidden.csv", 
                    simFileDir + "/kernel.csv", simFileDir + "/bias.csv")
}


// TODO: where to put these activation functions seem to be a big pain too...
trait Activations extends SpatialApp {
  type aT = FixPt[TRUE, _32, _32]
  type iT = FixPt[TRUE, _32, _32]
  val projectDir = "/home/tianzhao/spatial-lang/apps/src/"
  val lo = 16
  val spacingShiftBits = 5 // shift down
  val lutN = 512
  val sigF = projectDir + "sigmoid_512_16_-5.0.csv"
  val tanhF = projectDir + "tanh_512_16_-5.0.csv"
  // val tanhDir = projectDir + "512_tanhLUT.csv"


  // TODO: it seems fair to save the LUT space by utilizing the symmetry of sigmoid and tanh
  // TODO: we may not need that many bits for sigmoid and tanh. Need to reduce these.
  def sigmoid_(p: aT) = {
    val halfSigLUT = LUT.fromFile[aT](lutN)(sigF)
    val index = (abs(p).to[iT] << spacingShiftBits).to[Index] + 1.to[Index]
    val valueMux = mux(p < 0.to[aT], 1.to[aT] - halfSigLUT(index), halfSigLUT(index))
    val lowerMux = mux(p <= -lo.to[aT], 0.to[aT], valueMux)
    val upperMux = mux(p >= lo.to[aT], 1.to[aT], lowerMux)
    upperMux
  }


  def sigmoid_old(p: aT) = {
    val halfSigLUT = LUT.fromFile[aT](lutN)(sigF)
    val index = (abs(p).to[iT] << spacingShiftBits).to[Index]
    val valueMux = mux(p < 0.to[aT], 1.to[aT] - halfSigLUT(index), halfSigLUT(index))
    val lowerMux = mux(p <= -lo.to[aT], 0.to[aT], valueMux)
    val upperMux = mux(p >= lo.to[aT], 1.to[aT], lowerMux)
    upperMux
  }


  def tanh_(p: aT) = {
    val halfTanhLUT = LUT.fromFile[aT](lutN)(tanhF)
    val index = (abs(p).to[iT] << spacingShiftBits).to[Index] // + 1.to[Index]
    val valueMux = mux(p < 0.to[aT], 0.to[aT] - halfTanhLUT(index), halfTanhLUT(index))
    val lowerMux = mux(p <= -lo.to[aT], -1.to[aT], valueMux)
    val upperMux = mux(p >= lo.to[aT], 1.to[aT], lowerMux)
    upperMux
  }
}

object ActivationTests extends Activations {
  // TODO: In the real application, the integer bits shouldn't be more than 1.
  type T = FixPt[TRUE, _32, _32]
  @virtualize
  def main() {
    val x = ArgIn[T]
    val y = ArgOut[T]
    val y1 = ArgOut[T]
    val y2 = ArgOut[T]
    val N = args(0).to[T]
    setArg(x, N)

  // TODO: try this number: 0.15623885683914221 seems fail pretty badly... Fix it!
    Accel {
      Parallel {
        // y := tanh_(x.value)
        y := tanh_(x.value)   // numpy would give: 0.15497985487440297
        y1 := sigmoid_(x.value) // numpy would give: 0.53866759904966222
        y2 := sigmoid_old(x.value)
      }
    }

    val yre = getArg(y)
    val yre1 = getArg(y1)
    val yre2 = getArg(y2)
    println(yre)
    println(yre1)
    println(yre2)
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
object BasicLSTMCell extends SpatialApp with Params with Activations {
  type T = FixPt[TRUE, _32, _32]

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
    val splitSize = NN / 4 // i, j, f, o

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
            // TODO: check the paper from bengio. pow2 shift add on matmult
            val nD2 = j + jj
            val prod = Reduce(Reg[T])(ddco by 1) { kk => 
              tileA(ii, kk) * tileB(kk, jj)
            } {_+_}
            // adding in a bias
            val prev = mux(k == 0, tileBias(nD2), tileC(ii, jj))
            val ele = prev + prod.value
            // if the last element, then perform sigmoid overall
            if (k == PP - ddco) {
              if (nD2 < splitSize || nD2 >= splitSize * 3)
                tileC(ii, jj) = sigmoid_(ele)
                // tileC(ii, jj) = ele
              else if (splitSize <= nD2 && nD2 < splitSize * 2)
                tileC(ii, jj) = tanh_(ele)
                // tileC(ii, jj) = ele
              else
                tileC(ii, jj) = sigmoid_(ele + forgetBias)
                // tileC(ii, jj) = ele + forgetBias
            } else
              tileC(ii,jj) = ele
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