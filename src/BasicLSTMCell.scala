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
trait BasicLSTMCellTrait extends SpatialApp with Activations {
  var forgetBias: Int
  var batch_size: Int
  var feature_size: Int
  var hidden_size: Int
  var dp: Int
  var dm: Int

  // Result stores in h and c
  // This version assumes that we won't be able to fit kernel on SRAM
  // xh: x and hidden aligned on the second dimension

  def BasicLSTMCell[T:Type:Num](x: SRAM2[T], h: SRAM2[T], c: SRAM2[T], 
    sigI: SRAM2[T], tanhJ: SRAM2[T], sigF: SRAM2[T], sigO: SRAM2[T],
    kernel: DRAM2[T], bias: SRAM2[T]) {
    val linear_output_size = hidden_size * 4
    val reduce_size = feature_size + hidden_size
    val tileKernel = SRAM[T](dp, dm)
    // TODO: Par-able...
    // Linear Layer: linear([x,h], linear_output_size)
    Foreach (reduce_size by dp, linear_output_size by dm) { (i,j) =>
      tileKernel load kernel(i::i+dp, j::j+dm)
      Foreach (dp by 1, dm by 1) { (ii, jj) =>
        val rowOffset = i + ii
        val colOffset = j + jj
        val prod = Reduce(Reg[T]) (dp by 1) { k =>
          if (offset < feature_size) {
            x(rowOffset, k) * tileKernel(k, jj)
          } else {
            h(rowOffset - feature_size, k) * tileKernel(k, jj)
          }
        } {_+_}
        
      }
        Foreach (dn by 1, dd by 1) { (ii,jj) =>
          val prod = Reduce(Reg[T])(dp by 1) {
            tileA(ii,kk) * tileB(kk,jj)
          } {_+_}

          val prev = mux(k == 0, tileBias(col_offset), tileC(ii,jj))
          val ele = prev + prod.value
          if (col_offset < hidden_size) {
            sigI(ii, col_offset) = sigmoid_[T](ele)
          }
          else if (hidden_size <= col_offset < hidden_size * 2) {
            tanhJ(ii, col_offset - hidden_size) = tanh_[T](ele)
          }
          else if (2 * hidden_size <= col_offset < 3 * hidden_size) {
            sigF(ii, col_offset - hidden_size * 2) = sigmoid_[T](ele + forgetBias.to[T])
          }
          else {
            sigO(ii, col_offset - hidden_size * 3) = sigmoid_[T](ele)
          }
        }
      }
    }

    // Reduce layer
    // TODO: Par-able...
    Foreach (batch_size by 1, hidden_size by 1) { (i,j) =>
      val new_c = c(i,j) * sigF(i,j) + sigI(i,j) * tanhJ(i,j)
      h(i,j) = sigmoid_[T](new_c) * sigO(i,j)
      c(i,j) = new_c
    }
  }
}


object BasicLSTMCell_TestTrait extends SpatialApp with BasicLSTMCellTrait {
  var forgetBias = 1
  var batch_size = 2
  var feature_size = 32
  var hidden_size = 16
  var dn = 1
  var dm = 2
  var dp = 4

  type T = FixPt[TRUE, _8, _8]

  @virtualize
  def main() {
    val sigI = SRAM[T](batch_size, hidden_size)
    val tanhJ = SRAM[T](batch_size, hidden_size)
    val sigF = SRAM[T](batch_size, hidden_size)
    val sigO = SRAM[T](batch_size, hidden_size)
    val bias = SRAM[T](linear_output_size)
    val paramPath = "/home/tianzhao/spatial-lang/apps/parameters/test-params/"
    val (cDRAM, kDRAM, bDRAM, hDRAM, xDRAM) = (DRAM[T](batch_size, hidden_size), 
                                                DRAM[T](feature_size, ))
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
object BasicLSTMCell_bidaf extends SpatialApp with TestParams with Activations {
  type T = FixPt[TRUE, _8, _8]

  @virtualize
  def main() {
    val (a, hidden, memory, kernel, bias) = (DRAM[T](N, JX, dco), DRAM[T](N, JX, d), 
                                             DRAM[T](N, JX, d), DRAM[T](dco+d, 4*d), 
                                             DRAM[T](4*d))
    val drams = List(a, hidden, memory, kernel, bias)
    drams.zipWithIndex.foreach { case(e, idx) =>
      setMem(e, loadCSV1D[T](dataPaths(idx), ","))
    }

    val linearDRAM = DRAM[T](N, 4*d)
    val resultHiddenDRAM = DRAM[T](N, d)
    val resultMemoryDRAM = DRAM[T](N, d)
    val MM = N
    val PP = d + dco
    val NN = 4 * d

    Accel {
      // TODO: for higher dimension matrices, would the alignment matter? 
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
              if (nD2 < d || nD2 >= d * 3)
                tileC(ii, jj) = sigmoid_(ele)
                // tileC(ii, jj) = ele
              else if (d <= nD2 && nD2 < d * 2)
                tileC(ii, jj) = tanh_(ele)
                // tileC(ii, jj) = ele
              else
                tileC(ii, jj) = sigmoid_(ele + forgetBias)
                // tileC(ii, jj) = ele + forgetBias
            } else
              tileC(ii,jj) = ele
          }
        }

        linearDRAM(i::i+dn, j::j+dd) store tileC
      }

      // Reduce the results to hidden and memory
      //                    4*d
      //     +---------+---------+---------+----------+
      //     | sigm(i) | tanh(j) | sigm(f) | sigm(o)  |
      // N   |         |         |         |          |
      //     +---------v---------v---------v----------+

      val (tileI, tileJ, tileF, tileO, tileH, tileM) = (SRAM[T](dn, dd), SRAM[T](dn, dd), 
                                                        SRAM[T](dn, dd), SRAM[T](dn, dd),
                                                        SRAM[T](dn, dd), SRAM[T](dn, dd))
      val tileMM = SRAM[T](dn, dd)
      Foreach(MM by dn, d by dd) { (il, jl) =>
        Parallel {
          tileI load linearDRAM(il::il+dn, jl::jl+dd)
          tileJ load linearDRAM(il::il+dn, jl+d::jl+d+dd)
          tileF load linearDRAM(il::il+dn, jl+2*d::jl+2*d+dd)
          tileO load linearDRAM(il::il+dn, jl+3*d::jl+3*d+dd)
          tileM load memory(il::il+dn, 0, jl::jl+dd)
        } 

        Foreach(dn by 1, dd by 1) { (ild, jld) =>
          val newC = Reg[T](0)
          newC := tileM(ild, jld) * tileF(ild, jld) + tileI(ild, jld) * tileJ(ild, jld)
          tileH(ild, jld) = tanh_(newC.value) * tileO(ild, jld)
          tileMM(ild, jld) = newC.value
        }

        memory(il::il+dn, 0, jl::jl+dd) store tileMM
        hidden(il::il+dn, 0, jl::jl+dd) store tileH
      }
    }

    val hiddenRe = getMem(hidden)
    // printArray(result, "resultDRAM = ")
    writeCSV1D[T](hiddenRe, simFileDir + "/hidden_re.csv")
    val memRe = getMem(memory)
    writeCSV1D[T](memRe, simFileDir + "/mem_re.csv")
  }
}