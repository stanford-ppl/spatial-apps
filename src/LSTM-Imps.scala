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


// TODO: where to put these activation functions seem to be a big pain too...
trait Activations extends SpatialApp {
  type aT = FixPt[TRUE, _5, _8]
  type iT = FixPt[TRUE, _5, _8]
  val projectDir = "/home/tianzhao/spatial-lang/apps/src/"
  val loSig = 16
  val loTanh = 4
  val spacingShiftBitsSig = 5 // shift down for sig
  val spacingShiftBitsTanh = 7 // shift down for tanh
  val lutNSig = 512
  val lutNTanh = 512
  val sigF = projectDir + "sigmoid_512_16_-5.0.csv"
  val tanhF = projectDir + "tanh_512_4_-7.0.csv"


  // TODO: we may not need that many bits for sigmoid and tanh. Need to reduce these.
  def sigmoid_(p: aT) = {
    val halfSigLUT = LUT.fromFile[aT](lutNSig)(sigF)
    val index = (abs(p).to[iT] << spacingShiftBitsSig).to[Index] + 1.to[Index]
    val valueMux = mux(p < 0.to[aT], 1.to[aT] - halfSigLUT(index), halfSigLUT(index))
    val lowerMux = mux(p <= -loSig.to[aT], 0.to[aT], valueMux)
    val upperMux = mux(p >= loSig.to[aT], 1.to[aT], lowerMux)
    upperMux
  }


  def sigmoid_old(p: aT) = {
    val halfSigLUT = LUT.fromFile[aT](lutNSig)(sigF)
    val index = (abs(p).to[iT] << spacingShiftBitsSig).to[Index]
    val valueMux = mux(p < 0.to[aT], 1.to[aT] - halfSigLUT(index), halfSigLUT(index))
    val lowerMux = mux(p <= -loSig.to[aT], 0.to[aT], valueMux)
    val upperMux = mux(p >= loSig.to[aT], 1.to[aT], lowerMux)
    upperMux
  }


  // Implement tanh using a LUT
  def tanh_(p: aT) = {
    val halfTanhLUT = LUT.fromFile[aT](lutNTanh)(tanhF)
    val index = (abs(p).to[iT] << spacingShiftBitsTanh).to[Index] // + 1.to[Index]
    val valueMux = mux(p < 0.to[aT], 0.to[aT] - halfTanhLUT(index), halfTanhLUT(index))
    val lowerMux = mux(p <= -loTanh.to[aT], -1.to[aT], valueMux)
    val upperMux = mux(p >= loTanh.to[aT], 1.to[aT], lowerMux)
    upperMux
  }


  // A few ideas about implementing tanh: 
  // range doesn't need to go that much. -8 to 8 would be fine.
  // However more precisions need to be provided within 0 to 8.

  // Implement a tanh using: tanh(x) = 2 * sigmoid(2*x) - 1
  def tanh_approx(p: aT) = {
    (sigmoid_(p << 1) << 1) - 1
  }
}


trait CharRNNTestParams extends SpatialApp {
  val N = 50
  val JX = 2
  val dco = 100 // TODO: need to determine on the actual size
  val d = 100
  val dn = 10
  val ddco = 100
  val dd = 10
  // TODO: Need to port the data here
  val simFileDir = "/home/tianzhao/spatial-lang/apps/np-sims/"
  val dataPaths = List(simFileDir + "/a.csv", simFileDir + "/hidden.csv", 
                       simFileDir + "/memory.csv", simFileDir + "/kernel.csv", 
                       simFileDir + "/bias.csv")
}


trait CharRNNParams extends SpatialApp {
  val input_size = 65
  val rnn_size = 128
  val batch_size = 1 // In forward there's no batching
  val seq_length = 3 // This should be 2000

  val charRNNDir = "./weights"
  val dataPaths_2D = List(charRNNDir + "/i2h_1-weights.csv", charRNNDir + "/i2h_2-weights.csv", 
                          charRNNDir + "/h2h_1-weights.csv", charRNNDir + "/h2h_2-weights.csv",
                          charRNNDir + "/decoder-weights.csv", charRNNDir + "/c.csv", 
                          charRNNDir + "/h.csv", charRNNDir + "/input.csv")
  val dataPaths_1D = List(charRNNDir + "/i2h_1-bias.csv", charRNNDir + "/i2h_2-bias.csv",
                          charRNNDir + "/h2h_1-bias.csv", charRNNDir + "/h2h_2-bias.csv",
                          charRNNDir + "/decoder-bias.csv")
}


object CharRNNStandard_Zynq extends SpatialApp with CharRNNParams with Activations {
  type T = FixPt[TRUE, _5, _8] 

  @virtualize
  def main() {
    val lout = rnn_size*4
    val i2h_1w = DRAM[T](input_size, lout)
    val i2h_1b = DRAM[T](lout)
    val i2h_2w = DRAM[T](rnn_size, lout)
    val i2h_2b = DRAM[T](lout)
    val h2h_1w = DRAM[T](rnn_size, lout)
    val h2h_1b = DRAM[T](lout)
    val h2h_2w = DRAM[T](rnn_size, lout)
    val h2h_2b = DRAM[T](lout)
    val decoder_w_ = DRAM[T](rnn_size, input_size)
    val decoder_b_ = DRAM[T](input_size)
    val c_ = DRAM[T](batch_size, rnn_size)
    val h_ = DRAM[T](batch_size, rnn_size)
    val inputs = DRAM[T](seq_length, batch_size, input_size)
    val drams_2D = List(i2h_1w, i2h_2w, h2h_1w, h2h_2w, decoder_w_, c_, h_, inputs)
    drams_2D.zipWithIndex.foreach { case(e, idx) =>
      setMem(e, loadCSV2D[T](dataPaths_2D(idx), ",", "\n"))
    }

    val drams_1D = List(i2h_1b, i2h_2b, h2h_1b, h2h_2b, decoder_b_)
    drams_1D.zipWithIndex.foreach { case(e, idx) =>
      setMem(e, loadCSV1D[T](dataPaths_1D(idx), ","))
    }

    val results = DRAM[T](seq_length, input_size, batch_size)

    Accel {
      val i2h1w = SRAM[T](input_size, lout)
      val i2h1b = SRAM[T](lout)
      val h2h1w = SRAM[T](rnn_size, lout)
      val h2h1b = SRAM[T](lout)
      val i2h2w = SRAM[T](rnn_size, lout)
      val i2h2b = SRAM[T](lout)
      val h2h2w = SRAM[T](rnn_size, lout)
      val h2h2b = SRAM[T](lout)
      val decoder_w = SRAM[T](rnn_size, input_size)
      val decoder_b = SRAM[T](input_size)
      val c = SRAM[T](batch_size, rnn_size)
      val h = SRAM[T](batch_size, rnn_size)
      val x = SRAM[T](batch_size, input_size)
      val in_gate = SRAM[T](batch_size, rnn_size)
      val forget_gate = SRAM[T](batch_size, rnn_size)
      val out_gate = SRAM[T](batch_size, rnn_size)
      val in_transform = SRAM[T](batch_size, rnn_size)

      i2h1w load i2h_1w(0::input_size, 0::lout)
      i2h1b load i2h_1b(0::lout)
      h2h1w load h2h_1w(0::rnn_size, 0::lout)
      h2h1b load h2h_1b(0::lout)
      i2h2w load i2h_2w(0::rnn_size, 0::lout)
      i2h2b load i2h_2b(0::lout)
      h2h2w load h2h_2w(0::rnn_size, 0::lout)
      h2h2b load h2h_2b(0::lout)
      decoder_w load decoder_w_(0::rnn_size, 0::lout)
      decoder_b load decoder_b_(0::lout)
      c load c_ (0::batch_size, 0::rnn_size)
      h load h_ (0::batch_size, 0::rnn_size)

      Sequential.Foreach(seq_length by 1) { idx =>
        x load inputs(idx, 0::batch_size, 0::input_size)

        // stage 1
        Foreach (batch_size by 1, lout by 1) { (i, j) =>
          val i2h_prod = Reduce(Reg[T])(input_size by 1) { k => x(i, k) * i2h1w(k, j) } {_+_}
          val i2h_ele = i2h1b(j) + i2h_prod.value
          val h2h_prod = Reduce(Reg[T])(rnn_size by 1) { k => h(i, k) * h2h1w(k, j) } {_+_}
          val ele = h2h1b(j) + h2h_prod.value + i2h_ele
          if (j < rnn_size)
            in_gate(i, j) = sigmoid_(ele)
          else if (rnn_size <= j && j < rnn_size * 2)
            forget_gate(i, j - rnn_size) = sigmoid_(ele)
          else if (rnn_size * 2 <= j && j < rnn_size * 3)
            out_gate(i, j - rnn_size * 2) = sigmoid_(ele)
          else
            in_transform(i, j - rnn_size * 3) = tanh_(ele)
        }

        Foreach (batch_size by 1, rnn_size by 1) { (i, j) =>
          Pipe {
            c(i, j) = c(i, j) * forget_gate(i, j) + in_gate(i, j) * in_transform(i, j)
            h(i, j) = tanh_(c(i, j)) * out_gate(i, j)
          }
        }

        // stage 2
        Foreach (batch_size by 1, lout by 1) { (i, j) =>
          val i2h_prod = Reduce(Reg[T])(rnn_size by 1) { k => x(i, k) * i2h2w(k, j) } {_+_}
          val i2h_ele = i2h2b(j) + i2h_prod.value
          val h2h_prod = Reduce(Reg[T])(rnn_size by 1) { k => h(i, k) * h2h2w(k, j) } {_+_}
          val ele = h2h2b(j) + h2h_prod.value + i2h_ele
          if (j < rnn_size)
            in_gate(i, j) = sigmoid_(ele)
          else if (rnn_size <= j && j < rnn_size * 2)
            forget_gate(i, j - rnn_size) = sigmoid_(ele)
          else if (rnn_size * 2 <= j && j < rnn_size * 3)
            out_gate(i, j - rnn_size * 2) = sigmoid_(ele)
          else
            in_transform(i, j - rnn_size * 3) = tanh_(ele)
        }

        Foreach (batch_size by 1, rnn_size by 1) { (i, j) =>
          Pipe {
            c(i, j) = c(i, j) * forget_gate(i, j) + in_gate(i, j) * in_transform(i, j)
            h(i, j) = tanh_(c(i, j)) * out_gate(i, j)
          }
        }

        // decoder for h. Don't keep intermediate hidden and memory states
        Foreach(batch_size by 1, input_size by 1) { (i, j) =>
          val proj = Reduce(Reg[T])(rnn_size by 1) { k => h(i, k) * decoder_w(k, j) } {_+_}
          h(i, j) = proj + decoder_b(j)
        }

        results(idx, 0::batch_size, 0::rnn_size) store h
      }
    }

    val decoded_re = getMem(h_)
    // Provides a distribution over each char in the batch
    writeCSV1D[T](decoded_re, "/result.csv")
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
  // TODO: In the real application, the integer bits shouldn't be more than 1.
  type T = FixPt[TRUE, _5, _8]
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


// For split: i, j, f, o = np.split(linear, 4, axis=1)
object CharRNNLarge extends SpatialApp with CharRNNTestParams with Activations {
  type T = FixPt[TRUE, _5, _8]

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
      Sequential.Foreach(JX by 1) { L =>
        Foreach(MM by dn, NN by dd) { (i, j) =>
          val tileC = SRAM[T](dn, dd)
          Foreach(PP by ddco) { k =>
            val tileA = SRAM[T](dn, ddco)
            val tileB = SRAM[T](ddco, dd)
            Parallel {
              tileB load kernel(k::k+ddco, j::j+dd)
              if (k < dco) {
                tileA load a(i::i+dn, L, k::k+ddco)
              } else {
                tileA load hidden(i::i+dn, L, (k-dco)::(k-dco)+ddco)
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
              val prev = mux(k == 0, 0.to[T], tileC(ii, jj))
              val ele = prev + prod.value
              // if the last element, then perform sigmoid overall
              if (k == PP - ddco) {
                if (nD2 < d || nD2 >= d * 3)
                  tileC(ii, jj) = sigmoid_(ele)
                else if (d <= nD2 && nD2 < d * 2)
                  tileC(ii, jj) = tanh_(ele)
                else
                  tileC(ii, jj) = sigmoid_(ele)
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

        // TODO: change it to list tabulate?
        // val (tileI, tileJ, tileF, tileO, tileH, tileM) = (SRAM[T](dn, dd), SRAM[T](dn, dd), 
        //                                                   SRAM[T](dn, dd), SRAM[T](dn, dd),
        //                                                   SRAM[T](dn, dd), SRAM[T](dn, dd))
        val tileMM = SRAM[T](dn, dd)
        val tileI = SRAM[T](dn, dd)
        val tileJ = SRAM[T](dn, dd)
        val tileF = SRAM[T](dn, dd)
        val tileO = SRAM[T](dn, dd)
        val tileM = SRAM[T](dn, dd)
        val tileH = SRAM[T](dn, dd)
        Foreach(MM by dn, d by dd) { (il, jl) =>
          Parallel {
            tileI load linearDRAM(il::il+dn, jl::jl+dd)
            tileJ load linearDRAM(il::il+dn, jl+d::jl+d+dd)
            tileF load linearDRAM(il::il+dn, jl+2*d::jl+2*d+dd)
            tileO load linearDRAM(il::il+dn, jl+3*d::jl+3*d+dd)
            tileM load memory(il::il+dn, L, jl::jl+dd)
          } 

          Foreach(dn by 1, dd by 1) { (ild, jld) =>
            val newC = Reg[T](0)
            newC := tileM(ild, jld) * tileF(ild, jld) + tileI(ild, jld) * tileJ(ild, jld)
            tileH(ild, jld) = tanh_(newC.value) * tileO(ild, jld)
            tileMM(ild, jld) = newC.value
          }

          memory(il::il+dn, L+1, jl::jl+dd) store tileMM
          hidden(il::il+dn, L+1, jl::jl+dd) store tileH
        }
      }
    }

    val hiddenRe = getMem(hidden)
    // printArray(result, "resultDRAM = ")
    writeCSV1D[T](hiddenRe, simFileDir + "/hidden_re.csv")
    val memRe = getMem(memory)
    writeCSV1D[T](memRe, simFileDir + "/mem_re.csv")
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
object BasicLSTMCell extends SpatialApp with TestParams with Activations {
  type T = FixPt[TRUE, _5, _8]

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
      // TODO: Later this JX needs to be replaced by a different val...
      // TODO: currently it's all set to 0
      // TODO: for higher dimension matrices, would the alignment matter? 
      // TODO: I use a fused matrix to follow implementation in tf and pt. 
      // however would this be faster than having four separate ones? 
      // Or was this only for training time speed-up concerns?
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

      // TODO: change it to list tabulate?
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

// This test multiply each element in a high-dim DRAM with a constant
object DRAM3ConcatTestAugKernel extends SpatialApp with Params {
  type T = FixPt[TRUE, _5, _8] 

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