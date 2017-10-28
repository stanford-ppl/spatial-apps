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


// mem and hidden states are always in SRAMs. 
trait BasicLSTMCellTrait extends SpatialApp with Activations {
  val forgetBias = 1
  def BasicLSTMCell_DRAMIn[T:Type:Num](input: DRAM2[T]) {

  }

  def BasicLSTCell_DRAMOut[T:Type:Num]() {

  }

  def BasicLSTMCell_Intermediate[T:Type:Num] () {

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