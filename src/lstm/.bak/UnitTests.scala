import spatial.dsl._
import virtualized._

////////////////////////////////////////
////////////////////////////////////////
// LSTM Tests
////////////////////////////////////////
////////////////////////////////////////


// Attention layer of GNMT model
// object BahdanauAtt extends SpatialApp with CellImps with RNNImps {
//   override type T = Float
//
//   // For params
//   var batch_size = 1
//   var hidden_size = 16
//   var dm = 2
//   var dp = 4
// }


// object GateTest extends SpatialApp {
//   // Assumptions: batchSize = 1
//   // featureSize = 32
//   // hiddenSize = 32
//   // only calculate a gate result
//   @virtualize
//   def gate[T:Type:Num](x: SRAM2[T], h: SRAM2[T], kernel: DRAM2[T], bias: SRAM2[T], bKernel: SRAM[T], bTmp: SRAM[T], activation: T => T) {
//
//   }
//
//   @virtualize
//   def sigmoidLinear[T:Type:Num](p: T) = {
//     // sig_pw[T](p)
//     (tanh_synth[T](p / 2.to[T]) + 1.to[T]) / 2.to[T]
//     // sigmoidSim(p)
//   }
//
//   @virtualize
//   def tanhLinear[T:Type:Num](p: T) = {
//     val absp = abs(p)
//     val absre = if (absp > (2.5).to[T]) {
//       1.to[T]
//     } else if (((0.5).to[T] < p) && (p <= (2.5).to[T])) {
//       absp / 4.to[T] + (0.375).to[T]
//     } else {
//       absp
//     }
//
//     val mabsp = 0.to[T] - absp
//     val re = mux((p < 0.to[T]), mabsp, absp)
//     re
//     // tanhSim(p)
//   }
//
//
//   @virtualize
//   def main() {
//     type T = FixPt[TRUE, _4, _4]
//     val hiddenSize = 32
//     val featureSize = 32
//     val paramPath = "/home/tianzhao/spatial-lang-LSTM/apps/models/nmt/test_param_weights/"
//     val dm = 8
//     val dp = 8
//     val seqLen = 4
//     val N = 10
//     val xIn = DRAM[T](N, featureSize, seqLen)
//
//
//     Accel {
//       val (tileI, tileJ, tileF, tileO) = (SRAM[T](dp, dm), SRAM[T](dp, dm), SRAM[T](dp, dm), SRAM[T](dp, dm))
//       val (sigI, tanhJ, sigF, sigO) = (SRAM[T](hiddenSize), SRAM[T](hiddenSize), SRAM[T](hiddenSize), SRAM[T](hiddenSize))
//
//       val h = SRAM[T](hiddenSize)
//       Foreach (hiddenSize by 1) { i =>
//         h(i) = 0
//       }
//
//       // Outer loading
//       Sequential.Foreach (N by 1) { i =>
//         val x = SRAM[T](featureSize, seqLen)
//         x load xIn(i, featureSize, seqLen)
//
//         // Inner sequence
//         Parallel {
//           gate(x, h, dp, dm, sigI, tanhLinear)
//         }
//       }
//     }
//   }
//
// }
//


// Div issue?
object DivTest extends SpatialApp {
  @virtualize
  def main() {
    type T = FixPt[TRUE, _3, _5]
    val in = ArgIn[T]
    setArg(in, args(0).to[T])

    val out = ArgOut[T]
    Accel {
      out := in.value >> 2
    }

    val outVal = getArg(out)
    println("outVal = " + outVal)
    val cksum = outVal == args(0).to[T]
    println("PASS ? " + cksum)
  }
}


// This app aims to test how accurate tanh implementation is going to be.
// The gold result is produced using numpy
// object TanhErrorTestTypeIssue extends SpatialApp {
//   type T = FixPt[TRUE, _3, _5]
//
//   @virtualize
//   def tanhSynth[T:Type:Num](p: T) = {
//     println("==========")
//     println(" p = " + p)
//     val absp = abs(p)
//     println(" abs(p) = " + absp)
//     val absre = if (absp > (2.5).to[T]) {
//       1.to[T]
//     } else if (((0.5).to[T] < p) && (p <= (2.5).to[T])) {
//       println("here")
//       val div4 = absp >> 2
//       println("div4 = " + div4)
//       val div4Offset = div4 + (0.375).to[T]
//       println("div4Offset = " + div4Offset)
//       div4Offset
//     } else {
//       absp
//     }
//
//     val mabre = 0.to[T] - absre
//     val re = mux((p < 0.to[T]), mabre, absre)
//     println(" re = " + re)
//     re
//   }
//
//
//   @virtualize
//   def main() {
//     val N = 32
//     val inputArray = loadCSV1D[T]("/home/tianzhao/lstm/lstm-apps/sims/tanh/tanh_in.csv")
//     val goldArray = loadCSV1D[T]("/home/tianzhao/lstm/lstm-apps/sims/tanh/tanh_out.csv")
//
//     val memIn = DRAM[T](N)
//     val memGold = DRAM[T](N)
//     val memSynthResult = DRAM[T](N)
//     val memOut = DRAM[T](N)
//     setMem(memIn, inputArray)
//
//     setMem(memGold, goldArray)
//
//     Accel {
//       val sram = SRAM[T](N)
//       val sramSynth = SRAM[T](N)
//       val sramGold = SRAM[T](N)
//       val sramError = SRAM[T](N)
//       sram load  memIn(0::N)
//       sramGold load memGold(0::N)
//       Foreach (N by 1) { i =>
//         val in = sram(i)
//         val tanh = tanhSynth[T](in)
//         val gold = sramGold(i)
//         sramError(i) = gold - tanh
//         sramSynth(i) = tanh
//       }
//
//
//       memOut(0::N) store sramError
//       memSynthResult(0::N) store sramSynth
//     }
//
//     val synthout = getMem(memSynthResult)
//     printArray(synthout, "synth result = ")
//     val memout = getMem(memOut)
//     printArray(memout, "error = ")
//   }
// }

object SingleTanhErrorTest extends SpatialApp {
  type T = FixPt[TRUE, _3, _8]
  type FT = Float

  @virtualize
  def tanhSynth(p: T) = {
    val absp = abs(p)
    println("p = " + p + ", absp = " + absp)
    val absre = if (absp > (2.5).to[T]) {
      1.to[T]
    } else if (((0.5).to[T] < absp) && (absp <= (2.5).to[T])) {
      // bug: if replace div4 with the shifted result directly, spatial would infer the type of absp >> 2 as FixPt[TRUE, 0, 3]
      val div4 = absp >> 2
      val div4Offset = div4 + (0.375).to[T]
      div4Offset
    } else {
      absp
    }

    val mabre = 0.to[T] - absre
    val re = mux((p < 0.to[T]), mabre, absre)
    re
  }

  @virtualize
  def tanhSim[FT:Type:Num](t: FT) = (exp(t) - exp(-t)) / (exp(t) + exp(-t))

  @virtualize
  def main() {
    val in = ArgIn[T]
    val fin = ArgIn[FT]

    setArg(in, args(0).to[T])
    setArg(fin, args(0).to[FT])

    val out = ArgOut[T]
    val fout = ArgOut[FT]

    Accel {
      out := tanhSynth(in)
      fout := tanhSim(fin)
    }

    val outval = getArg(out)
    val foutval = getArg(fout)
    println("out = " + outval)
    println("fout = " + foutval)
  }
}


object TanhErrorTest extends SpatialApp {
  type T = FixPt[TRUE, _3, _6]

  @virtualize
  def tanhSynth(p: T) = {
    val absp = abs(p)
    val absre = if (absp > (2.5).to[T]) {
      1.to[T]
    } else if (((0.5).to[T] < absp) && (absp <= (2.5).to[T])) {
      // bug: if replace div4 with the shifted result directly, spatial would infer the type of absp >> 2 as FixPt[TRUE, 0, 3]
      val div4 = absp >> 2
      val div4Offset = div4 + (0.375).to[T]
      div4Offset
    } else {
      absp
    }

    val mabre = 0.to[T] - absre
    val re = mux((p < 0.to[T]), mabre, absre)
    re
  }

  @virtualize
  def main() {
    val N = 64
    val inputArray = loadCSV1D[T]("/home/tianzhao/lstm/lstm-apps/sims/tanh/tanh_in.csv")

    val memIn = DRAM[T](N)
    val memSynthResult = DRAM[T](N)
    setMem(memIn, inputArray)

    Accel {
      val sram = SRAM[T](N)
      val sramSynth = SRAM[T](N)
      sram load  memIn(0::N)
      Foreach (N by 1) { i =>
        val in = sram(i)
        val tanh = tanhSynth(in)
        sramSynth(i) = tanh
      }

      memSynthResult(0::N) store sramSynth
    }

    val synthout = getMem(memSynthResult)
    printArray(synthout, "synth result = ")
    writeCSV1D[T](synthout, "/home/tianzhao/lstm/lstm-apps/sims/tanh/tanh_out_scalasim.csv")
  }
}


object BasicLSTMTest extends SpatialApp with CellImps {
  @virtualize
  def main() {
    type T = FixPt[TRUE, _3, _5]
    val paramPath = "/home/tianzhao/spatial-lang-LSTM/apps/models/nmt/test_param_weights/"

    val batch_size = 1
    val feature_size = 32
    val hidden_size = 32
    val dm = 8
    val dp = 8
    val maxTime = 4
    val xDRAM = DRAM[T](batch_size, maxTime, feature_size)
    val resultDRAM = DRAM[T](batch_size, maxTime, feature_size)

    setMem(xDRAM, loadCSV1D[T](paramPath+"x_3d.csv", ","))

    val fwCellParam = CellParam[T](paramPath, "BiLSTMFw", batch_size, feature_size, hidden_size, dp, dm)

    Accel {
      val fwCell = BasicLSTMCell[T](fwCellParam, SRAM[T](batch_size, hidden_size))
      fwCell.init()

      Sequential.Foreach(maxTime by 1) { timeStep =>
        fwCell.x load xDRAM(0::batch_size, timeStep, 0::feature_size)
        fwCell.forward()
        resultDRAM(0::batch_size, timeStep, 0::feature_size) store fwCell.h
      }
    }

    val re = getMem(resultDRAM)
    writeCSV1D[T](re, "./resultDRAM_BasicLSTMCell.csv")
  }
}


// Activation tests
object SigTanhTest extends SpatialApp {
  @virtualize
  def sigmoid_synth[T:Type:Num](p: T) = {
    // sig_pw[T](p)
    (tanh_synth[T](p / 2.to[T]) + 1.to[T]) / 2.to[T]
  }

  @virtualize
  def tanh_synth[T:Type:Num](p: T) = {
    val absp = abs(p)
    val absre = if (absp > (2.5).to[T]) {
      1.to[T]
    } else if (((0.5).to[T] < p) && (p <= (2.5).to[T])) {
      absp / 4.to[T] + (0.375).to[T]
    } else {
      absp
    }

    val mabsp = 0.to[T] - absp
    val re = mux((p < 0.to[T]), mabsp, absp)
    re
    // tanh_pw[T](p)
    // if (p > (2.5).to[T]) {
    //   1.to[T]
    // } else if (((0.5).to[T] < p) && (p <= (2.5).to[T])) {
    //   p / 4.to[T] + (0.375).to[T]
    // } else if (((-2.5).to[T] < p) && (p <= (-0.5).to[T])) {
    //   p / 4.to[T] - (0.375).to[T]
    // } else if (p <= (-2.5).to[T]) {
    //   -1.to[T]
    // } else {
    //   p
    // }


    // mux((p > (2.5).to[T]), 1.to[T], mux((((0.5).to[T] < p) && (p <= (2.5).to[T])), mux( (((-2.5).to[T] < p) && (p <= (-0.5).to[T])), p / 4.to[T] - (0.375).to[T], mux((p <= (-2.5).to[T]), -1.to[T], p) ) ) )
  }

  @virtualize
  def sigmoidSim[T:Type:Num](t: T) = 1.to[T]/(exp(-t) + 1.to[T])

  @virtualize
  def tanhSim[T:Type:Num](t: T) = (exp(t) - exp(-t)) / (exp(t) + exp(-t))

  @virtualize
  def main() {
    type T = FixPt[TRUE, _5, _8]
    val a = ArgIn[T]
    val N = args(0).to[T]
    val M = args(0).to[Float]
    setArg(a, N)
    val b_sig_synth = ArgOut[T]
    val b_tanh_synth = ArgOut[T]
    Accel {
      b_sig_synth := sigmoid_synth(a.value)
      b_tanh_synth := tanh_synth(a.value)
    }

    val b_sig_re = getArg(b_sig_synth)
    val b_tanh_re = getArg(b_tanh_synth)
    val b_sig_gold = sigmoidSim(M)
    val b_tanh_gold = tanhSim(M)
    println("sig: Gold = " + b_sig_gold + ", FixPt = " + b_sig_re)
    println("tanh: Gold = " + b_tanh_gold + ", FixPt = " + b_tanh_re)
  }

}



// Decoder of GNMT model
object NMTDec extends SpatialApp with CellImps with RNNImps {
  type T = FixPt[TRUE, _8, _8]

  // For cells
  var forgetBias = 1
  var batch_size = 1
  var feature_size = 1024
  var hidden_size = 1024
  var dm = 16
  var dp = 16

  // For rnn
  var maxTime = 4

  // For debug mode
  // override val debug = true

  @virtualize
  def main() {
    val paramPath = "/home/tianzhao/spatial-lang/apps/models/nmt/test_param_weights/"
    val xDRAM = DRAM[T](batch_size, maxTime, feature_size)
    setMem(xDRAM, loadCSV1D[T](paramPath+"x_3d.csv", ",")) // 3d mat: [batch_size, max_time, feature_size]

    val deBasicCell0Param = CellParam[T](paramPath, "DeCell0", batch_size, feature_size * 2, hidden_size, dp, dm)
    val deBasicCell1Param = CellParam[T](paramPath, "DeCell1", batch_size, feature_size * 2, hidden_size, dp, dm)
    val deResCell0Param = CellParam[T](paramPath, "ResCell0", batch_size, feature_size * 2, hidden_size, dp, dm)
    val deResCell1Param = CellParam[T](paramPath, "ResCell1", batch_size, feature_size * 2, hidden_size, dp, dm)

    Accel {
      // TODO: need to connect this to the seq2seq implementation
      val deBasicCell0 = BasicLSTMCell(deBasicCell0Param, SRAM[T](batch_size, hidden_size))
      val deBasicCell1 = BasicLSTMCell(deBasicCell1Param, deBasicCell0.h)
      val deResCell0 = ResidualBasicLSTMCell(deResCell0Param, deBasicCell1.h)
      val deResCell1 = ResidualBasicLSTMCell(deResCell1Param, deResCell0.h)


      Parallel {
        deBasicCell0.init()
        deBasicCell1.init()
        deResCell0.init()
        deResCell1.init()
      }

      Sequential.Foreach (maxTime by 1) { timeStep =>
        deBasicCell0.x load xDRAM(0::batch_size, timeStep, 0::feature_size)

        deBasicCell0.forward()
        deBasicCell1.forward()
        deResCell0.forward()
        deResCell1.forward()
      }

      deResCell1.flush()
    }

    writeCSV2D(getMatrix(deResCell1Param.hDRAM), "./decoder_re.csv")
  }
}


// Encoder of GNMT model
object NMTEnc extends SpatialApp with CellImps with RNNImps {
  type T = FixPt[TRUE, _8, _8]

  // For cells
  var forgetBias = 1
  var batch_size = 1
  var feature_size = 512
  var hidden_size = 512
  var dm = 32
  var dp = 16

  // For rnn
  var maxTime = 4

  // For debug mode
  // override val debug = true

  @virtualize
  def main() {
    val paramPath = "/home/tianzhao/spatial-lang/apps/models/nmt/test_param_weights/"
    val xDRAM = DRAM[T](batch_size, maxTime, feature_size)
    setMem(xDRAM, loadCSV1D[T](paramPath+"x_3d.csv", ",")) // 3d mat: [batch_size, max_time, feature_size]

    val hidStateBufFw = DRAM[T](batch_size, maxTime, feature_size)
    val hidStateBufBw = DRAM[T](batch_size, maxTime, feature_size)

    val fwCellParam = CellParam[T](paramPath, "BiLSTMFw", batch_size, feature_size, hidden_size, dp, dm)
    val bwCellParam = CellParam[T](paramPath, "BiLSTMBw", batch_size, feature_size, hidden_size, dp, dm)
    val fusedCellParam = CellParam[T](paramPath, "FusedLSTM", batch_size, feature_size, hidden_size, dp, dm, isFusedInputs=true)
    val resCellParam0 = CellParam[T](paramPath, "ResCell0", batch_size, feature_size, hidden_size, dp, dm)
    val resCellParam1 = CellParam[T](paramPath, "ResCell1", batch_size, feature_size, hidden_size, dp, dm)

    Accel {
      val fwCell = BasicLSTMCell(fwCellParam, SRAM[T](batch_size, hidden_size))
      val bwCell = BasicLSTMCell(bwCellParam, SRAM[T](batch_size, hidden_size))
      val lstmCell = FusedReadsBasicLSTMCell(fusedCellParam, fwCell.h, bwCell.h)
      val resCell0 = ResidualBasicLSTMCell(resCellParam0, lstmCell.h)
      val resCell1 = ResidualBasicLSTMCell(resCellParam1, resCell0.h)

      Parallel {
        fwCell.init()
        bwCell.init()
        lstmCell.init()
        resCell0.init()
        resCell1.init()
      }

      // TODO: it seems that the output of bi-directional rnn and fused lstm is right. However the outputs of
      // rescell0 and rescell1 doesn't match. Why is that?
      Sequential.Foreach(maxTime by 1) { timeStep =>
        Parallel {
          Pipe {
            fwCell.x load xDRAM(0::batch_size, timeStep, 0::feature_size)
            fwCell.forward()
            hidStateBufFw(0::batch_size, timeStep, 0::feature_size) store fwCell.h
          }

          Pipe {
            bwCell.x load xDRAM(0::batch_size, maxTime - timeStep - 1, 0::feature_size)
            bwCell.forward()
            hidStateBufBw(0::batch_size, maxTime - timeStep - 1, 0::feature_size) store bwCell.h
          }
        }
      }

      // TODO: debugging
      // Parallel {
      //   fwCell.flush()
      //   bwCell.flush()
      // }

      // TODO: is there a better way to encode the sequence from a backward orientation?
      // Maybe use it in a systolic array fashion?
      // Check the paper that talks about opt-ing bilstm
      // TODO: check the paper that implements efficient activations
      Sequential.Foreach(maxTime by 1) { timeStep =>
        Parallel {
          fwCell.h load hidStateBufFw(0::batch_size, timeStep, 0::feature_size)
          bwCell.h load hidStateBufBw(0::batch_size, timeStep, 0::feature_size)
        }

        lstmCell.forward()

        // TODO: debugging
        resCell0.forward()
        resCell1.forward()
      }

      // Parallel {
      //   lstmCell.flush()

      //   // TODO: debugging
      //   resCell0.flush()
      //   resCell1.flush()
      // }
      resCell1.flush() //technically I only need the parameter from resCell1
    }

    writeCSV2D(getMatrix(resCellParam1.hDRAM), "./res1_re.csv")
    // TODO: debugging
    // writeCSV2D(getMatrix(fwCellParam.hDRAM), "./fw_re.csv")
    // writeCSV2D(getMatrix(bwCellParam.hDRAM), "./bw_re.csv")
    // writeCSV2D(getMatrix(fusedCellParam.hDRAM), "./lstm_re.csv")
    // writeCSV2D(getMatrix(resCellParam0.hDRAM), "./res0_re.csv")
  }
}


// This function is basically regular basic LSTM. The input is splitted into 2.
object FusedReadsBasicLSTM extends SpatialApp with CellImps with RNNImps {
  type T = FixPt[TRUE, _8, _8] // for simulation

  // For cells
  var forgetBias = 1
  var batch_size = 1
  var feature_size = 16
  var hidden_size = 16
  var dm = 2
  var dp = 4

  var linear_output_size = hidden_size * 4
  var reduce_size = hidden_size + feature_size

  // For rnn
  var maxTime = 4

  @virtualize
  def main() {
    val paramPath = "/home/tianzhao/spatial-lang/apps/models/nmt/test_param_weights/"
    val xDRAM = DRAM[T](batch_size, maxTime, feature_size * 2)
    setMem(xDRAM, loadCSV1D[T](paramPath+"x_3d.csv", ",")) // 3d mat: [batch_size, max_time, feature_size]
    val fusedLSTMCellParam = CellParam[T](paramPath, "FusedLSTM", batch_size, feature_size, hidden_size, dp, dm, isFusedInputs=true)

    Accel {
      val cell = new FusedReadsBasicLSTMCell(fusedLSTMCellParam,
                                              SRAM[T](batch_size, feature_size),
                                              SRAM[T](batch_size, feature_size))
      cell.init()
      Sequential.Foreach (maxTime by 1) { timeStep =>
        cell.x load xDRAM(0::batch_size, timeStep, 0::feature_size)
        cell.x1 load xDRAM(0::batch_size, timeStep, feature_size::feature_size*2)
        cell.forward()
      }
      cell.flush()
    }

    writeCSV2D(getMatrix(fusedLSTMCellParam.hDRAM), "./h_re.csv", ",", "\n")
    writeCSV2D(getMatrix(fusedLSTMCellParam.cDRAM), "./c_re.csv", ",", "\n")
  }
}


object ResidualLSTM extends SpatialApp with CellImps with RNNImps {
  type T = FixPt[TRUE, _8, _8]

  // For cells
  var forgetBias = 1
  var batch_size = 1
  var feature_size = 16
  var hidden_size = 16
  var dm = 2
  var dp = 4

  var linear_output_size = hidden_size * 4
  var reduce_size = hidden_size + feature_size

  // For rnn
  var maxTime = 4

  @virtualize
  def main() {
    val paramPath = "/home/tianzhao/spatial-lang/apps/models/nmt/test_param_weights/"
    val xDRAM = DRAM[T](batch_size, maxTime, feature_size)
    setMem(xDRAM, loadCSV1D[T](paramPath+"x_3d.csv", ",")) // 3d mat: [batch_size, max_time, feature_size]
    val resCellParam = CellParam[T](paramPath, "ResLSTM", batch_size, feature_size, hidden_size, dp, dm)

    Accel {
      val cell = ResidualBasicLSTMCell[T](resCellParam, SRAM[T](batch_size, feature_size))
      val rnn = new RNN(xDRAM, cell, maxTime=maxTime)
      rnn.forward()
    }

    writeCSV2D(getMatrix(resCellParam.hDRAM), "./h_re.csv", ",", "\n")
    writeCSV2D(getMatrix(resCellParam.cDRAM), "./c_re.csv", ",", "\n")
  }
}


// This examples sets up a bi-directional rnn with maxTime = 4
object BiLSTM extends SpatialApp with CellImps with RNNImps {
  type T = FixPt[TRUE, _8, _8]
  var forgetBias = 1
  var batch_size = 1
  var feature_size = 16
  var hidden_size = 16
  var dn = 1
  var dm = 2
  var dp = 4

  var linear_output_size = hidden_size * 4
  var reduce_size = hidden_size + feature_size

  var maxTime = 4

  @virtualize
  def main() {
    val paramPath = "/home/tianzhao/spatial-lang/apps/models/nmt/test_param_weights/"
    // input data
    val xDRAM = DRAM[T](batch_size, maxTime, feature_size)
    setMem(xDRAM, loadCSV1D[T](paramPath+"x_3d.csv", ",")) // 3d mat: [batch_size, max_time, feature_size]

    val fwCellParam = CellParam[T](paramPath, "BiLSTMFw", batch_size, feature_size, hidden_size, dp, dm)
    val bwCellParam = CellParam[T](paramPath, "BiLSTMBw", batch_size, feature_size, hidden_size, dp, dm)

    Accel {
      val fwCell = BasicLSTMCell(fwCellParam, SRAM[T](batch_size, feature_size))
      val bwCell = BasicLSTMCell(bwCellParam, SRAM[T](batch_size, feature_size))

      Parallel {
        fwCell.init()
        bwCell.init()
      }

      // val rnn = new BiRNN(xDRAM, fwCell, bwCell, maxTime=maxTime)
      // rnn.forward()
      Sequential.Foreach(maxTime by 1) { timeStep =>
        Parallel {
          Pipe {
            fwCell.x load xDRAM(0::fwCell.batch_size, timeStep, 0::fwCell.feature_size)
            fwCell.forward()
          }

          Pipe {
            bwCell.x load xDRAM(0::bwCell.batch_size, maxTime - timeStep - 1, 0::bwCell.feature_size)
            bwCell.forward()
          }
        }
      }

      Parallel {
        fwCell.flush()
        bwCell.flush()
      }
    }

    // fw write results
    writeCSV2D(getMatrix(fwCellParam.hDRAM), "./fw_h_re.csv", ",", "\n")
    writeCSV2D(getMatrix(fwCellParam.cDRAM), "./fw_c_re.csv", ",", "\n")

    // bw write results
    writeCSV2D(getMatrix(bwCellParam.hDRAM), "./bw_h_re.csv", ",", "\n")
    writeCSV2D(getMatrix(bwCellParam.cDRAM), "./bw_c_re.csv", ",", "\n")
  }
}


object BasicLSTM extends SpatialApp with CellImps with RNNImps {
  type T = FixPt[TRUE, _8, _8]
  // For cells
  var forgetBias = 1
  var batch_size = 1
  var feature_size = 16
  var hidden_size = 16
  var dn = 1
  var dm = 2
  var dp = 4

  var linear_output_size = hidden_size * 4
  var reduce_size = hidden_size + feature_size

  // For rnn
  var maxTime = 4

  @virtualize
  def main() {
    val paramPath = "/home/tianzhao/spatial-lang/apps/models/nmt/test_param_weights/"
    val xDRAM = DRAM[T](batch_size, maxTime, feature_size)
    setMem(xDRAM, loadCSV1D[T](paramPath+"x_3d.csv", ",")) // 3d mat: [batch_size, max_time, feature_size]
    val lstmCellParam = new CellParam[T](paramPath, "BasicLSTMCell", batch_size, feature_size, hidden_size, dp, dm)

    Accel {
      val cell = BasicLSTMCell(lstmCellParam, SRAM[T](batch_size, feature_size))
      val rnn = new RNN(xDRAM, cell, maxTime=maxTime)
      rnn.forward()
    }

    writeCSV2D(getMatrix(lstmCellParam.hDRAM), "./h_re.csv", ",", "\n")
    writeCSV2D(getMatrix(lstmCellParam.cDRAM), "./c_re.csv", ",", "\n")
  }
}

////////////////////////////////////////
////////////////////////////////////////
// Function Tests
////////////////////////////////////////
////////////////////////////////////////

object TypedActivationTest_0 extends SpatialApp with ActivationFunctions {
  @virtualize
  def main() {
    type T = FixPt[TRUE, _8, _8]
    val re = activation[T](args(0).to[T])
    println("result = " + re)
  }
}

trait ActivationFunctions extends SpatialApp with Activations {
  @virtualize
  def activation[T:Type:Num] (a: T) = {
    val aA = ArgIn[T]
    setArg(aA, a)
    val b = ArgOut[T]
    Accel {
      type activationType = FixPt[TRUE, _8, _8]
      val act = aA.value.to[activationType]
      b := tanh_pw(act).to[T]
    }

    getArg(b)
  }
}

object ActivationTest extends SpatialApp with Activations {
  type FixT = FixPt[TRUE, _8, _8]
  type FloatT = Float

  @virtualize
  def main() {
    val num = args(0)
    val a = ArgIn[FixT]
    val b = ArgIn[FloatT]
    setArg(a, num.to[FixT])
    setArg(b, num.to[FloatT])

    val y0_tanh = ArgOut[FixT]
    val y1_tanh = ArgOut[FloatT]
    val y0_sig = ArgOut[FixT]
    val y1_sig = ArgOut[FloatT]

    Accel {
      y0_tanh := tanh_pw(a)
      y1_tanh := tanhSim(b)
      y0_sig := sig_pw(a)
      y1_sig := sigmoidSim(b)
    }

    println("y0_tanh = " + getArg(y0_tanh))
    println("y1_tanh = " + getArg(y1_tanh))
    println("y0_sig = " + getArg(y0_sig))
    println("y1_sig = " + getArg(y1_sig))
  }
}


object Basic extends SpatialApp {
  @virtualize
  def main() {
    val x = ArgIn[Int]
    val y = ArgOut[Int]
    val N = args(0).to[Int]

    setArg(x, N)

    Accel {
      y := x + 4
    }

    val result = getArg(y)
    val gold = N + 4
    println("expected: " + gold)
    println("result: " + result)
  }
}

// trait PassClassTrait extends SpatialApp {
//   override type T = FixPt[TRUE, _8, _8]
//   class testClass(xc: Float) {
//     var x: T = xc

//     @virtualize
//     def addX(xReg: Reg[Float]) {
//       xReg := xReg.value + x
//     }
//   }
// }


// object PassClassTraitTest extends SpatialApp with PassClassTrait {
//   override type T = FixPt[TRUE, _8, _8]
//   @virtualize
//   def main() {
//     val a = 0.35.to[T]
//     val k = ArgIn[T]
//     val y = ArgOut[T]
//     setArg(k, a)
//     Accel {
//       val dut = new testClass(0.45.to[T])
//       val testReg = Reg[Float](0)
//       dut.addX(testReg)
//       y := testReg.value + k.value
//     }

//     println(getArg(y))
//   }
// }


// object ActivationSimTest extends SpatialApp {
//   type T = FixPt[TRUE, _8, _8]
//   def sigmoidSim[T:Type:Num](t: T) = 1.to[T]/(exp(-t) + 1.to[T])
//   def tanhSim[T:Type:Num](t: T) = (exp(t) - exp(-t)) / (exp(t) + exp(-t))
//   @virtualize
//   def main() {
//     val a = 0.35.to[T]
//     val x = ArgIn[T]
//     val ySig = ArgOut[T]
//     val yTanh = ArgOut[T]
//     setArg(x, a)
//     Accel {
//       val k = x.value
//       ySig := sigmoidSim[T](k)
//       yTanh := tanhSim[T](k)
//     }

//     println(getArg(ySig))
//     println(getArg(yTanh))
//   }
// }


// object BitSlicingTest extends SpatialApp {
//   type T = FixPt[TRUE, _8, _8]
//   // type integer = FixPt[TRUE, _8, _0]
//   // type
//   @virtualize
//   def main() {
//     val a = 7.375.to[T]
//     val x = ArgIn[T]
//     val y = ArgOut[T]
//     setArg(x, a)
//     Accel {
//       val k = x.value
//       val FIVE = (5.to[T]).apply(14::8)
//       val p = Vector.concatN(Seq(k(7::0), FIVE, k(15::15))).as[T]
//       y := p
//     }

//     println(getArg(y))
//   }
// }


// // This test trys to pack kernel and bias into one pack
// trait WeightsTrait extends SpatialApp {
//   val paramPath = "/home/tianzhao/spatial-lang/apps/models/nmt/test_param_weights/"

//   type T = FixPt[TRUE, _8, _8]
//   val batch_size = 2
//   val feature_size = 32
//   val hidden_size = 16

//   @struct class Weights(kernel: DRAM2[T], bias: SRAM2[T])

//   @virtualize
//   // def elementwise_addOne(kernelW: DRAM2[T], biasW: SRAM2[T]) {
//   def elementwise_addOne(weights: Weights) {
//     val kernelW = weights.kernel
//     val biasW = weights.bias
//     val kernelS = SRAM[T](feature_size + hidden_size, hidden_size*4)
//     kernelS load kernelW(0::(feature_size + hidden_size), 0::(hidden_size*4))
//     Foreach((feature_size + hidden_size) by 1, (hidden_size*4) by 1) { (i, j) =>
//       kernelS(i,j) = kernelS(i,j) + 1.to[T]
//     }

//     Foreach((feature_size + hidden_size) by 1) { i =>
//       biasW(0,i) = biasW(0,i) + 3.to[T]
//     }

//     kernelW(0::batch_size, 0::(hidden_size + feature_size)) store kernelS
//   }
// }


// object TestStructClass extends SpatialApp with WeightsTrait {

//   @virtualize
//   def main() {
//     val kernel = DRAM[T](feature_size + hidden_size, 4 * hidden_size)
//     val biasD = DRAM[T](1, feature_size + hidden_size)
//     setMem(kernel, loadCSV2D[T](paramPath+"kernel.csv", ",", "\n"))
//     setMem(biasD, loadCSV2D[T](paramPath+"bias.csv", ",", "\n"))

//     Accel {
//       val bias = SRAM[T] (1, feature_size + hidden_size)
//       bias load biasD(0::1, 0::feature_size + hidden_size)
//       val weightInst = Weights(kernel, bias)
//       elementwise_addOne(weightInst)
//     }

//     writeCSV2D(getMatrix(kernel), "./testKernel.csv", ",", "\n")
//     writeCSV2D(getMatrix(biasD), "./testBias.csv", ",", "\n")
//   }
// }


// // This test trys to pass functions
// trait CallbacksTest extends SpatialApp with Activations {
//   type T = FixPt[TRUE, _8, _8]

//   @virtualize
//   def activates(callback: T => T, value: T): T = {
//     callback(value)
//   }
// }


// object TestCallBacks extends SpatialApp with CallbacksTest {

//   @virtualize
//   def main() {
//     val a = (0.12).to[T]
//     val b = (0.12).to[T]
//     val x = ArgIn[T]
//     val y = ArgIn[T]
//     val xx = ArgOut[T]
//     val yy = ArgOut[T]
//     setArg(x, a)
//     setArg(y, b)

//     Accel {
//       Parallel {
//         xx := activates(sigmoid_, x.value)
//         yy := activates(tanh_, x.value)
//       }
//     }

//     val xxre = getArg(xx)
//     val yyre = getArg(yy)
//     println(xx)
//     println(yy)
//   }
// }


// trait BaseDesign extends SpatialApp {
//   // println("==========")
//   // println(mm)
//   // println(nn)
//   // println("==========")
//   var mm: Int
//   var nn: Int
//   var M: Int
//   var N: Int
//   val factor = 3

//   def elementwise_matmul[T:Type:Num](a: DRAM2[T], b: DRAM2[T], c: DRAM2[T]) {
//     val sram0 = SRAM[T](mm, nn)
//     val sram1 = SRAM[T](mm, nn)
//     val resram = SRAM[T](mm, nn)
//     Foreach (M by mm, N by nn) { (i, j) =>
//       sram0 load a(i::i+mm, j::j+nn)
//       sram1 load b(i::i+mm, j::j+nn)
//       Foreach(mm by 1, nn by 1) { (ii, jj) =>
//         resram(ii, jj) = sram0(ii, jj) * sram1(ii, jj) * factor.to[T]
//       }

//       c(i::i+mm, j::j+nn) store resram
//     }
//   }
// }


// trait RealDesign0 extends BaseDesign {
//   var mm = 3
//   var nn = 4
//   var M = 6
//   var N = 12
//   override val factor = 10
// }


// trait RealDesign1 extends BaseDesign {
//   var mm = 2
//   var nn = 6
//   var M = 6
//   var N = 12
//   override val factor = 100
// }


// object RealDesign0Test extends SpatialApp with RealDesign0 {
//   @virtualize
//   def main() {
//     val paramPath = "/home/tianzhao/spatial-lang/apps/parameters/test-params/"
//     val aDRAM = DRAM[Float](M, N)
//     val bDRAM = DRAM[Float](M, N)
//     val reDRAM = DRAM[Float](M, N)
//     setMem(aDRAM, loadCSV2D[Float](paramPath+"param0.csv", ",", "\n"))
//     setMem(bDRAM, loadCSV2D[Float](paramPath+"param1.csv", ",", "\n"))

//     Accel {
//       elementwise_matmul[Float](aDRAM, bDRAM, reDRAM)
//     }

//     writeCSV1D[Float](getMem(reDRAM), "RealDesign0Test.csv")
//   }
// }


// object RealDesign1Test extends SpatialApp with RealDesign1 {
//   @virtualize
//   def main() {
//     val paramPath = "/home/tianzhao/spatial-lang/apps/parameters/test-params/"
//     val aDRAM = DRAM[Float](M, N)
//     val bDRAM = DRAM[Float](M, N)
//     val reDRAM = DRAM[Float](M, N)
//     setMem(aDRAM, loadCSV2D[Float](paramPath+"param0.csv", ",", "\n"))
//     setMem(bDRAM, loadCSV2D[Float](paramPath+"param1.csv", ",", "\n"))

//     Accel {
//       elementwise_matmul[Float](aDRAM, bDRAM, reDRAM)
//     }

//     writeCSV1D[Float](getMem(reDRAM), "RealDesign1Test.csv")
//   }
// }


// object WriteCSV3DTest extends SpatialApp {
//   @virtualize
//   def main() {
//     val N = 3
//     val M = 4
//     val P = 5

//     val resultDRAM = DRAM[Int](N, M, P)

//     Accel {
//       val resultSRAM = SRAM[Int](N, M, P)
//       val reg = Reg[Int](0)
//       Foreach(N by 1, M by 1, P by 1) { (i, j, k) =>
//         reg := reg.value + 1
//         resultSRAM(i, j, k) = reg
//       }

//       resultDRAM(0::N, 0::M, 0::P) store resultSRAM
//     }

//     val re = getMem(resultDRAM)
//     writeCSV1D[Int](re, "./resultDRAM.csv")
//   }
// }


// object ReadCSV3DTest extends SpatialApp {
//   @virtualize
//   def main() {
//     val N = 3
//     val M = 4
//     val P = 5

//     val inputDRAM = DRAM[Int](N, M, P)
//     setMem(inputDRAM, loadCSV1D[Int]("./resultDRAM.csv", ","))

//     Accel {
//       val inputSRAM = SRAM[Int](N, M, P)
//       inputSRAM load inputDRAM(0::N, 0::M, 0::P)
//       Foreach(N by 1, M by 1, P by 1) { (i, j, k) =>
//         println(i)
//         println(j)
//         println(k)
//         println(inputSRAM(i, j, k))
//         println("==========")
//       }
//     }
//   }
// }


// object ActivationTests extends Activations {
//   type T = FixPt[TRUE, _8, _8]
//   override type targetT = FixPt[TRUE, _8, _8]
//   override type LUTInT = FixPt[TRUE, _8, _8]
//   @virtualize
//   def main() {
//     val x = ArgIn[T]
//     val y = ArgOut[T]
//     val y1 = ArgOut[T]
//     val N = args(0).to[T]
//     setArg(x, N)

//     // Try: 0.09800561, 0.12712223
//     Accel {
//       Parallel {
//         // y := tanh_(x.value)
//         y := tanh_(x.value)   // numpy would give: 0.097693026355715917, 0.126441859911746
//         y1 := sigmoid_(x.value) // numpy would give: 0.52448180978452119, 0.53173782856894825
//       }
//     }

//     val yre = getArg(y)
//     val yre1 = getArg(y1)
//     println(yre)
//     println(yre1)
//   }
// }


// trait Params extends SpatialApp {
//   val N = 60
//   val JX = 161
//   val dco = 1400
//   val d = 100

//   val dn = 10
//   val ddco = 100
//   val dd = 10
//   val forgetBias = 1
//   val simFileDir = "/home/tianzhao/spatial-lang/apps/np-sims/"
//   val dataPaths = List(simFileDir + "/a.csv", simFileDir + "/hidden.csv",
//                        simFileDir + "/memory.csv", simFileDir + "/kernel.csv",
//                        simFileDir + "/bias.csv")
//   val n = 2 // iteration steps
// }


// // This test multiply each element in a high-dim DRAM with a constant
// object DRAM3ConcatTestAugKernel extends SpatialApp with Params {
//   type T = FixPt[TRUE, _8, _8]

//   @virtualize
//   def main() {
//     val (a, hidden, kernel, bias) = (DRAM[T](N, JX, dco), DRAM[T](N, JX, d),
//                                      DRAM[T](dco+d, 4*d), DRAM[T](4*d))
//     val drams = List(a, hidden, kernel, bias)
//     drams.zipWithIndex.foreach { case(e, idx) =>
//       setMem(e, loadCSV1D[T](dataPaths(idx), ","))
//     }

//     val rk = DRAM[T](dco+d, 4*d)

//     Accel {
//       val tileA = SRAM[T](ddco, dd)
//       val tileB = SRAM[T](ddco, dd)
//       Foreach(dco+d by ddco, 4*d by dd) { (i,j) =>
//         tileA load kernel(i::i+ddco, j::j+dd)
//         Foreach(ddco by 1, dd by 1) { (ii,jj) =>
//           tileB(ii,jj) = tileA(ii,jj) * 2
//         }

//         rk(i::i+ddco, j::j+dd) store tileB
//       }
//     }

//     val rkresult = getMem(rk)
//     printArray(rkresult, "Result: ")
//   }
// }
