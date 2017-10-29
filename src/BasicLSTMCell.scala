import spatial.dsl._
import org.virtualized._


trait TestParams extends SpatialApp {
  val N = 32
  val JX = 10
  val dco = 100
  val d = 20

  val dn = 4
  val ddco = 10
  val dd = 2
  val forgetBias = 1
  val simFileDir = "/home/tianzhao/spatial-lang/apps/np-sims/"
  val dataPaths = List(simFileDir + "/a.csv", simFileDir + "/hidden.csv",
                       simFileDir + "/memory.csv", simFileDir + "/kernel.csv", 
                       simFileDir + "/bias.csv")
}


trait Params extends SpatialApp {
  val N = 60
  val JX = 161
  val dco = 1400
  val d = 100

  val dn = 10
  val ddco = 100
  val dd = 10
  val forgetBias = 1
  val simFileDir = "/home/tianzhao/spatial-lang/apps/np-sims/"
  val dataPaths = List(simFileDir + "/a.csv", simFileDir + "/hidden.csv", 
                       simFileDir + "/memory.csv", simFileDir + "/kernel.csv", 
                       simFileDir + "/bias.csv")
  val n = 2 // iteration steps
}


// The linear layer does np.concatenate([a, hidden], axis=1).dot(kernel) + bias(broadcasted)
// bias needs to broadcast over each batch
//               feature_size   hidden_size     4*hidden_size         1
//              +--------------+-----+       +-----------------+    +--+
//              |              |     |       |                 |    |  |
//              |              |     |       |                 |    |  |
//              |              |     |       |                 |    |  |
//  batch_size  |      x       |hidden   *   |     kernel      | +  |  | 4 * hidden_size
//              |              |     |       |                 |    |  |
//              |              |     |       |feature_size     |    |  |
//              |              |     |       | + hidden_size   |    |  |
//              +--------------+-----+       |                 |    |  |
//                                           |                 |    +--+
//                                           |                 |
//                                           |                 |
//                                           |                 |
//                                           |                 |
//                                           +-----------------+

// mem and hidden states are always in SRAMs. 
trait BasicLSTMCell_NMT extends SpatialApp {
  type lT = FixPt[TRUE, _8, _8] // for LUTs
  type cT = FixPt[TRUE, _8, _8] // for compute precision
  type T = FixPt[TRUE, _8, _8] // for general precision

  val projectDir = "/home/tianzhao/spatial-lang/apps/src/activation-luts/"
  val loSig = 16.to[lT]
  val loTanh = 4.to[lT]
  val spacingShiftBitsSig = 5 // shift down for sig
  val spacingShiftBitsTanh = 7 // shift down for tanh
  val lutNSig = 512
  val lutNTanh = 512
  val sigF = projectDir + "sigmoid_512_16_-5.0.csv"
  val tanhF = projectDir + "tanh_512_4_-7.0.csv"

  def sigmoid_(p: lT) = {
    val halfSigLUT = LUT.fromFile[lT](lutNSig)(sigF)
    val index = (abs(p).to[lT] << spacingShiftBitsSig).to[Index] + 1.to[Index]
    val valueMux = mux(p < 0.to[lT], 1.to[lT] - halfSigLUT(index), halfSigLUT(index))
    val lowerMux = mux(p <= -loSig, 0.to[lT], valueMux)
    val upperMux = mux(p >= loSig, 1.to[lT], lowerMux)
    upperMux
  }


  def tanh_(p: lT) = {
    val halfTanhLUT = LUT.fromFile[lT](lutNTanh)(tanhF)
    val index = (abs(p).to[lT] << spacingShiftBitsTanh).to[Index] // + 1.to[Index]
    val valueMux = mux(p < 0.to[T], 0.to[T] - halfTanhLUT(index), halfTanhLUT(index))
    val lowerMux = mux(p <= -loTanh, -1.to[T], valueMux)
    val upperMux = mux(p >= loTanh, 1.to[T], lowerMux)
    upperMux
  }


  // Result stores in h and c
  // This version assumes that we won't be able to fit kernel on SRAM
  // xh: x and hidden aligned on the second dimension
  def BasicLSTMCell(x: SRAM2[T], h: SRAM2[T], c: SRAM2[T], 
    sigI: SRAM2[T], tanhJ: SRAM2[T], sigF: SRAM2[T], sigO: SRAM2[T],
    kernel: DRAM2[T], bias: SRAM[T]) {
    val forgetBias = 1
    val batch_size = 1
    val feature_size = 32
    val hidden_size = 16
    val dn = 1
    val dp = 4
    val dm = 2

    val linear_output_size = hidden_size * 4
    val reduce_size = feature_size + hidden_size
    val tileKernel = SRAM[T](dp, dm)
    // TODO: Par-able...
    // Linear Layer: linear([x,h], linear_output_size)
    Foreach (batch_size by 1) { rowOffset =>
      Foreach (reduce_size by dp, linear_output_size by dm) { (k,j) =>
        tileKernel load kernel(k::k+dp, j::j+dm)
        Foreach (dp by 1, dm by 1) { (kk, jj) =>
          val prod = Reduce(Reg[T]) (dp by 1) { p =>
            val offset = p // TODO: just put it here to bypass compiler error
            if (offset < feature_size) {
              x(rowOffset, p) * tileKernel(p, jj)
            } else {
              h(rowOffset - feature_size, p) * tileKernel(p, jj)
            }
          } {_+_}

          val colOffset = j + jj
          if (colOffset < hidden_size) { // TODO: how's this line different than the others? 
            val ele = prod.value + mux(k == 0, bias(colOffset), sigI(rowOffset, colOffset))
            sigI(rowOffset, colOffset) = sigmoid_(ele)
          }
          else if (hidden_size <= colOffset && colOffset < hidden_size * 2) {
            val ele = prod.value + mux(k == 0, bias(colOffset), tanhJ(rowOffset, colOffset - hidden_size))
            tanhJ(rowOffset, colOffset - hidden_size) = tanh_(ele)
          }
          else if (2 * hidden_size <= colOffset && colOffset < 3 * hidden_size) {
            val ele = prod.value + mux(k == 0, bias(colOffset), sigF(rowOffset, colOffset - 2 * hidden_size))
            sigF(rowOffset, colOffset - hidden_size * 2) = sigmoid_(ele + forgetBias.to[T])
          }
          else {
            val ele = prod.value + mux(k == 0, bias(colOffset), sigO(rowOffset, colOffset - 3 * hidden_size))
            sigO(rowOffset, colOffset - hidden_size * 3) = sigmoid_(ele)
          }
        }
      }
    }

    // Reduce layer
    // TODO: Par-able...
    Foreach (batch_size by 1, hidden_size by 1) { (i,j) =>
      val new_c = c(i,j) * sigF(i,j) + sigI(i,j) * tanhJ(i,j)
      h(i,j) = sigmoid_(new_c) * sigO(i,j)
      c(i,j) = new_c
    }
  }
}


object BasicLSTMCellNMT_TestTrait extends SpatialApp with BasicLSTMCell_NMT {
  override val forgetBias = 1
  override val batch_size = 2
  override val feature_size = 32
  override val hidden_size = 16
  override val dn = 1
  override val dm = 2
  override val dp = 4

  val linear_output_size = hidden_size * 4

  @virtualize
  def main() {
    val sigI = SRAM[T](batch_size, hidden_size)
    val tanhJ = SRAM[T](batch_size, hidden_size)
    val sigF = SRAM[T](batch_size, hidden_size)
    val sigO = SRAM[T](batch_size, hidden_size)
    val bias = SRAM[T](linear_output_size)
    val paramPath = "/home/tianzhao/spatial-lang/apps/parameters/test-params/"
    val (cDRAM, kernel, bDRAM, hDRAM, xDRAM) = (DRAM[T](batch_size, hidden_size), 
                                                DRAM[T](hidden_size + feature_size, hidden_size),
                                                DRAM[T](linear_output_size),
                                                DRAM[T](batch_size, hidden_size),
                                                DRAM[T](batch_size, feature_size))
    setMem(cDRAM, loadCSV2D[T](paramPath+"c.csv", ",", "\n"))
    setMem(kernel, loadCSV2D[T](paramPath+"kernel.csv", ",", "\n"))
    setMem(hDRAM, loadCSV2D[T](paramPath+"h.csv", ",", "\n"))
    setMem(xDRAM, loadCSV2D[T](paramPath+"x.csv", ",", "\n"))
    setMem(bDRAM, loadCSV1D[T](paramPath+"bias.csv", ","))

    Accel {
      val x = SRAM[T](batch_size, feature_size)
      val h = SRAM[T](batch_size, hidden_size)
      val c = SRAM[T](batch_size, hidden_size)
      val sigI = SRAM[T](batch_size, hidden_size)
      val sigF = SRAM[T](batch_size, hidden_size)
      val sigO = SRAM[T](batch_size, hidden_size)
      val tanhJ = SRAM[T](batch_size, hidden_size)
      val bias = SRAM[T](linear_output_size)

      x load xDRAM(0::batch_size, 0::feature_size)
      h load hDRAM(0::batch_size, 0::hidden_size)
      c load cDRAM(0::batch_size, 0::hidden_size)
      bias load bDRAM(0::linear_output_size)
      BasicLSTMCell(x, h, c, sigI, tanhJ, sigF, sigO, kernel, bias)

      hDRAM(0::batch_size, 0::hidden_size) store h
      cDRAM(0::batch_size, 0::hidden_size) store c
    }

    writeCSV2D(getMatrix(hDRAM), "./h_re.csv", ",", "\n")
    writeCSV2D(getMatrix(cDRAM), "./c_re.csv", ",", "\n")
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
// object BasicLSTMCell_bidaf extends SpatialApp with TestParams with Activations {
//   type T = FixPt[TRUE, _8, _8]

//   @virtualize
//   def main() {
//     val (a, hidden, memory, kernel, bias) = (DRAM[T](N, JX, dco), DRAM[T](N, JX, d), 
//                                              DRAM[T](N, JX, d), DRAM[T](dco+d, 4*d), 
//                                              DRAM[T](4*d))
//     val drams = List(a, hidden, memory, kernel, bias)
//     drams.zipWithIndex.foreach { case(e, idx) =>
//       setMem(e, loadCSV1D[T](dataPaths(idx), ""))
//     }

//     val linearDRAM = DRAM[T](N, 4*d)
//     val resultHiddenDRAM = DRAM[T](N, d)
//     val resultMemoryDRAM = DRAM[T](N, d)
//     val MM = N
//     val PP = d + dco
//     val NN = 4 * d

//     Accel {
//       // TODO: for higher dimension matrices, would the alignment matter? 
//       val tileBias = SRAM[T](NN)
//       tileBias load bias(0::NN)
//       Foreach(MM by dn, NN by dd) { (i, j) =>
//         val tileC = SRAM[T](dn, dd)
//         Foreach(PP by ddco) { k =>
//           val tileA = SRAM[T](dn, ddco)
//           val tileB = SRAM[T](ddco, dd)
//           Parallel {
//             tileB load kernel(k::k+ddco, j::j+dd)
//             if (k < dco) {
//               tileA load a(i::i+dn, 0, k::k+ddco)
//             } else {
//               tileA load hidden(i::i+dn, 0, (k-dco)::(k-dco)+ddco)
//             }
//           }

//           Foreach(dn by 1, dd by 1) { (ii, jj) =>
//             // TODO: multiply add... can I replace this with shift add?
//             // TODO: check the paper from bengio. pow2 shift add on matmult
//             val nD2 = j + jj
//             val prod = Reduce(Reg[T])(ddco by 1) { kk => 
//               tileA(ii, kk) * tileB(kk, jj)
//             } {_+_}
//             // adding in a bias
//             val prev = mux(k == 0, tileBias(nD2), tileC(ii, jj))
//             val ele = prev + prod.value
//             // if the last element, then perform sigmoid overall
//             if (k == PP - ddco) {
//               if (nD2 < d || nD2 >= d * 3)
//                 tileC(ii, jj) = sigmoid_(ele)
//                 // tileC(ii, jj) = ele
//               else if (d <= nD2 && nD2 < d * 2)
//                 tileC(ii, jj) = tanh_(ele)
//                 // tileC(ii, jj) = ele
//               else
//                 tileC(ii, jj) = sigmoid_(ele + forgetBias)
//                 // tileC(ii, jj) = ele + forgetBias
//             } else
//               tileC(ii,jj) = ele
//           }
//         }

//         linearDRAM(i::i+dn, j::j+dd) store tileC
//       }

//       // Reduce the results to hidden and memory
//       //                    4*d
//       //     +---------+---------+---------+----------+
//       //     | sigm(i) | tanh(j) | sigm(f) | sigm(o)  |
//       // N   |         |         |         |          |
//       //     +---------v---------v---------v----------+

//       val (tileI, tileJ, tileF, tileO, tileH, tileM) = (SRAM[T](dn, dd), SRAM[T](dn, dd), 
//                                                         SRAM[T](dn, dd), SRAM[T](dn, dd),
//                                                         SRAM[T](dn, dd), SRAM[T](dn, dd))
//       val tileMM = SRAM[T](dn, dd)
//       Foreach(MM by dn, d by dd) { (il, jl) =>
//         Parallel {
//           tileI load linearDRAM(il::il+dn, jl::jl+dd)
//           tileJ load linearDRAM(il::il+dn, jl+d::jl+d+dd)
//           tileF load linearDRAM(il::il+dn, jl+2*d::jl+2*d+dd)
//           tileO load linearDRAM(il::il+dn, jl+3*d::jl+3*d+dd)
//           tileM load memory(il::il+dn, 0, jl::jl+dd)
//         } 

//         Foreach(dn by 1, dd by 1) { (ild, jld) =>
//           val newC = Reg[T](0)
//           newC := tileM(ild, jld) * tileF(ild, jld) + tileI(ild, jld) * tileJ(ild, jld)
//           tileH(ild, jld) = tanh_(newC.value) * tileO(ild, jld)
//           tileMM(ild, jld) = newC.value
//         }

//         memory(il::il+dn, 0, jl::jl+dd) store tileMM
//         hidden(il::il+dn, 0, jl::jl+dd) store tileH
//       }
//     }

//     val hiddenRe = getMem(hidden)
//     // printArray(result, "resultDRAM = ")
//     writeCSV1D[T](hiddenRe, simFileDir + "/hidden_re.csv")
//     val memRe = getMem(memory)
//     writeCSV1D[T](memRe, simFileDir + "/mem_re.csv")
//   }
// }