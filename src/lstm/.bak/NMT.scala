import spatial.dsl._
import virtualized._

object NMTEncoder_64_64_8_8_2res extends SpatialApp with NMTEncoderBaseTrait {
  @virtualize
  def main() {
    type T = FixPt[TRUE, _8, _8]
    val feature_size = 64
    val hidden_size = 64
    val dm = 5
    val dp = 11
    val batch_size = 1
    val maxTime = 4
    val paramPath = "/home/tianzhao/spatial-lang/apps/models/nmt/test_param_weights/"

    val output_hidden = NMTEncoderBase[T](paramPath.asInstanceOf[java.lang.String], batch_size, feature_size, hidden_size, maxTime, dm, dp)
    writeCSV2D(getMatrix(output_hidden), "./res1_re.csv")
  }
}

object NMTEncoder_512_512_32_16_2res extends SpatialApp with NMTEncoderBaseTrait {
  @virtualize
  def main() {
    type T = FixPt[TRUE, _8, _8]
    val feature_size = 512
    val hidden_size = 512
    val dm = 32
    val dp = 16
    val batch_size = 1
    val maxTime = 4

    val paramPath = "/home/tianzhao/spatial-lang/apps/models/nmt/test_param_weights/"
    val output_hidden = NMTEncoderBase[T](paramPath.asInstanceOf[java.lang.String], batch_size, feature_size, hidden_size, maxTime, dm, dp)
    writeCSV2D(getMatrix(output_hidden), "./res1_re.csv")
  }
}

object NMTEncoder_1024_1024_4_12_aws extends SpatialApp with NMTEncoderBaseTrait {
  @virtualize
  def main() {
    type T = FixPt[TRUE, _4, _12]
    val feature_size = 1024
    val hidden_size = 1024
    val dm = 32
    val dp = 32
    val batch_size = 1
    val maxTime = 4

    val paramPath = "/home/tianzhao/spatial-lang/apps/models/nmt/test_param_weights/"
    val output_hidden = NMTEncoderBase[T](paramPath.asInstanceOf[java.lang.String], batch_size, feature_size, hidden_size, maxTime, dm, dp)
    writeCSV2D(getMatrix(output_hidden), "./res1_re.csv")
  }
}


object NMTEncoder_1024_1024_16_16_2res extends SpatialApp with NMTEncoderBaseTrait {
  @virtualize
  def main() {
    type T = FixPt[TRUE, _8, _8]
    val feature_size = 1024
    val hidden_size = 1024
    val dm = 8
    val dp = 8
    val batch_size = 1
    val maxTime = 4

    val paramPath = "/home/tianzhao/spatial-lang/apps/models/nmt/test_param_weights/"
    val output_hidden = NMTEncoderBase[T](paramPath.asInstanceOf[java.lang.String], batch_size, feature_size, hidden_size, maxTime, dm, dp)
    writeCSV2D(getMatrix(output_hidden), "./res1_re.csv")
  }
}

object NMTEncoder_1024_1024_3_5 extends SpatialApp with NMTEncoderBaseTrait {
  override val target = spatial.targets.AWS_F1
  @virtualize
  def main() {
    type T = FixPt[TRUE, _3, _5]
    val feature_size = 1024
    val hidden_size = 1024
    val dm = 8
    val dp = 8
    val batch_size = 1
    val maxTime = 4

    val paramPath = "/home/tianzhao/spatial-lang/apps/models/nmt/test_param_weights/"
    val output_hidden = NMTEncoderBase[T](paramPath.asInstanceOf[java.lang.String], batch_size, feature_size, hidden_size, maxTime, dm, dp)
    writeCSV2D(getMatrix(output_hidden), "./res1_re.csv")
  }
}

object NMTEncoder_1024_1024_3_5_no_instrumentation extends SpatialApp with NMTEncoderBaseTrait {
  @virtualize
  def main() {
    type T = FixPt[TRUE, _3, _5]
    val feature_size = 1024
    val hidden_size = 1024
    val dm = 8
    val dp = 8
    val batch_size = 1
    val maxTime = 4

    val paramPath = "/home/tianzhao/spatial-lang/apps/models/nmt/test_param_weights/"
    val output_hidden = NMTEncoderBase[T](paramPath.asInstanceOf[java.lang.String], batch_size, feature_size, hidden_size, maxTime, dm, dp)
    writeCSV2D(getMatrix(output_hidden), "./res1_re.csv")
  }
}


trait NMTEncoderBaseTrait extends SpatialApp with CellImps with RNNImps {
  // override type T = FixPt[TRUE, _8, _8]
  @virtualize
  def NMTEncoderBase[T:Type:Num](paramPath: java.lang.String, batch_size: Int, feature_size: Int, hidden_size: Int, maxTime: Int, dm: Int, dp: Int) (implicit convert : Cast[String, T]) = {
    val xDRAM = DRAM[T](batch_size, maxTime, feature_size)
    setMem(xDRAM, loadCSV1D[T](paramPath+"x_3d.csv", ","))

    val hidStateBufFw = DRAM[T](batch_size, maxTime, feature_size)
    val hidStateBufBw = DRAM[T](batch_size, maxTime, feature_size)

    val fwCellParam = CellParam[T](paramPath, "BiLSTMFw", batch_size, feature_size, hidden_size, dp, dm)
    val bwCellParam = CellParam[T](paramPath, "BiLSTMBw", batch_size, feature_size, hidden_size, dp, dm)
    val fusedCellParam = CellParam[T](paramPath, "FusedLSTM", batch_size, feature_size, hidden_size, dp, dm, isFusedInputs=true)
    val resCellParam0 = CellParam[T](paramPath, "ResCell0", batch_size, feature_size, hidden_size, dp, dm)
    val resCellParam1 = CellParam[T](paramPath, "ResCell1", batch_size, feature_size, hidden_size, dp, dm)

    Accel {
      val fwCell = BasicLSTMCell[T](fwCellParam, SRAM[T](batch_size, hidden_size))
      val bwCell = BasicLSTMCell[T](bwCellParam, SRAM[T](batch_size, hidden_size))
      val lstmCell = FusedReadsBasicLSTMCell[T](fusedCellParam, fwCell.h, bwCell.h)
      val resCell0 = ResidualBasicLSTMCell[T](resCellParam0, lstmCell.h)
      val resCell1 = ResidualBasicLSTMCell[T](resCellParam1, resCell0.h)

      Parallel {
        fwCell.init()
        bwCell.init()
        lstmCell.init()
        resCell0.init()
        resCell1.init()
      }

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

      Sequential.Foreach(maxTime by 1) { timeStep =>
        Parallel {
          fwCell.h load hidStateBufFw(0::batch_size, timeStep, 0::feature_size)
          bwCell.h load hidStateBufBw(0::batch_size, timeStep, 0::feature_size)
        }

        lstmCell.forward()
        resCell0.forward()
        resCell1.forward()
      }

      resCell1.flush() //technically I only need the parameter from resCell1
    }

    resCellParam1.hDRAM // result from the residual cell
//    writeCSV2D(getMatrix(resCellParam1.hDRAM), "./res1_re.csv")
  }
}


// Encoder of GNMT model
// object NMTEnc_synth_fit_64_64 extends SpatialApp with CellImps with RNNImps {
//   override type T = FixPt[TRUE, _8, _8]

//   // For cells
//   var forgetBias = 1
//   var batch_size = 1
//   var feature_size = 64
//   var hidden_size = 64
//   var dm = 4
//   var dp = 2

//   // For rnn
//   var maxTime = 4

//   // For debug mode
//   // override val debug = true

//   @virtualize
//   def main() {
//     val paramPath = "/home/tianzhao/spatial-lang/apps/models/nmt/test_param_weights/"
//     val xDRAM = DRAM[T](batch_size, maxTime, feature_size)
//     setMem(xDRAM, loadCSV1D[T](paramPath+"x_3d.csv", ",")) // 3d mat: [batch_size, max_time, feature_size]

//     val hidStateBufFw = DRAM[T](batch_size, maxTime, feature_size)
//     val hidStateBufBw = DRAM[T](batch_size, maxTime, feature_size)

//     val fwCellParam = CellParam[T](paramPath, "BiLSTMFw", batch_size, feature_size, hidden_size, dp, dm)
//     val bwCellParam = CellParam[T](paramPath, "BiLSTMBw", batch_size, feature_size, hidden_size, dp, dm)
//     val fusedCellParam = CellParam[T](paramPath, "FusedLSTM", batch_size, feature_size, hidden_size, dp, dm, isFusedInputs=true)
//     val resCellParam0 = CellParam[T](paramPath, "ResCell0", batch_size, feature_size, hidden_size, dp, dm)
//     val resCellParam1 = CellParam[T](paramPath, "ResCell1", batch_size, feature_size, hidden_size, dp, dm)

//     Accel {
//       val fwCell = BasicLSTMCell(fwCellParam, SRAM[T](batch_size, hidden_size))
//       val bwCell = BasicLSTMCell(bwCellParam, SRAM[T](batch_size, hidden_size))
//       val lstmCell = FusedReadsBasicLSTMCell(fusedCellParam, fwCell.h, bwCell.h)
//       val resCell0 = ResidualBasicLSTMCell(resCellParam0, lstmCell.h)
//       val resCell1 = ResidualBasicLSTMCell(resCellParam1, resCell0.h)

//       Parallel {
//         fwCell.init()
//         bwCell.init()
//         lstmCell.init()
//         resCell0.init()
//         resCell1.init()
//       }

//       // TODO: it seems that the output of bi-directional rnn and fused lstm is right. However the outputs of
//       // rescell0 and rescell1 doesn't match. Why is that?
//       Sequential.Foreach(maxTime by 1) { timeStep =>
//         Parallel {
//           Pipe {
//             fwCell.x load xDRAM(0::batch_size, timeStep, 0::feature_size)
//             fwCell.forward()
//             hidStateBufFw(0::batch_size, timeStep, 0::feature_size) store fwCell.h
//           }

//           Pipe {
//             bwCell.x load xDRAM(0::batch_size, maxTime - timeStep - 1, 0::feature_size)
//             bwCell.forward()
//             hidStateBufBw(0::batch_size, maxTime - timeStep - 1, 0::feature_size) store bwCell.h
//           }
//         }
//       }

//       // TODO: debugging
//       // Parallel {
//       //   fwCell.flush()
//       //   bwCell.flush()
//       // }

//       // TODO: is there a better way to encode the sequence from a backward orientation?
//       // Maybe use it in a systolic array fashion?
//       // Check the paper that talks about opt-ing bilstm
//       // TODO: check the paper that implements efficient activations
//       Sequential.Foreach(maxTime by 1) { timeStep =>
//         Parallel {
//           fwCell.h load hidStateBufFw(0::batch_size, timeStep, 0::feature_size)
//           bwCell.h load hidStateBufBw(0::batch_size, timeStep, 0::feature_size)
//         }

//         lstmCell.forward()

//         // TODO: debugging
//         resCell0.forward()
//         resCell1.forward()
//       }

//       // Parallel {
//       //   lstmCell.flush()

//       //   // TODO: debugging
//       //   resCell0.flush()
//       //   resCell1.flush()
//       // }
//       resCell1.flush() //technically I only need the parameter from resCell1
//     }

//     writeCSV2D(getMatrix(resCellParam1.hDRAM), "./res1_re.csv")
//     // TODO: debugging
//     // writeCSV2D(getMatrix(fwCellParam.hDRAM), "./fw_re.csv")
//     // writeCSV2D(getMatrix(bwCellParam.hDRAM), "./bw_re.csv")
//     // writeCSV2D(getMatrix(fusedCellParam.hDRAM), "./lstm_re.csv")
//     // writeCSV2D(getMatrix(resCellParam0.hDRAM), "./res0_re.csv")
//   }
// }




// object NMT extends SpatialApp with CellImps
//                                   with RNNImps {
//   override type T = FixPt[TRUE, _8, _8]
//   var forgetBias = 1
//   var batch_size = 1
//   var feature_size = 32
//   var hidden_size = 16
//   var dn = 1
//   var dm = 2
//   var dp = 4

//   var linear_output_size = hidden_size * 4
//   var reduce_size = hidden_size + feature_size

//   var maxTime = 4

//   @virtualize
//   def main() {
//     val paramPath = "/home/tianzhao/spatial-lang/apps/models/nmt/test_param_weights/"
//     // input data
//     val xDRAM = DRAM[T](batch_size, maxTime, feature_size)
//     setMem(xDRAM, loadCSV1D[T](paramPath+"x_3d.csv", ",")) // 3d mat: [batch_size, max_time, feature_size]

//     val fwCellParam = new CellParam(paramPath, "BiLSTMFw", batch_size, feature_size, hidden_size, dp, dm)
//     // val bwCellParam = new CellParam(paramPath, "BiLSTMBw")
//     val lstmCellParam = new CellParam(paramPath, "BasicLSTMCell", batch_size, feature_size, hidden_size, dp, dm)
//     val resCellParam0 = new CellParam(paramPath, "ResLSTMCell_0", batch_size, feature_size, hidden_size, dp, dm)
//     val resCellParam1 = new CellParam(paramPath, "ResLSTMCell_1", batch_size, feature_size, hidden_size, dp, dm)

//     Accel {
//       // first bidirectional encoder
//       val fwCell = new BasicLSTMCell(fwCellParam, SRAM[T](batch_size, feature_size))
//       // val bwCell = new BasicLSTMCell(batch_size, feature_size, hidden_size, bwCellParam, SRAM[T](batch_size, feature_size))
//       // intermediate lstm
//       // TODO: how can I pass fwcell and bwcell inputs to the next stage?
//       // TODO: size of this lstm cell isn't correct. need to modify it...
//       val lstmCell = new BasicLSTMCell(lstmCellParam, fwCell.h)
//       // two residual layers for the encoder
//       val resCell0 = new ResidualBasicLSTMCell(resCellParam0, lstmCell.h)
//       val resCell1 = new ResidualBasicLSTMCell(resCellParam1, resCell0.h)

//       val fw_bDRAM = fwCell.bDRAM
//       // val bw_bDRAM = bwCell.bDRAM

//       Parallel {
//         fwCell.bI load fwCell.bDRAM(0::1, 0::hidden_size)
//         fwCell.bJ load fwCell.bDRAM(0::1, hidden_size::2*hidden_size)
//         fwCell.bF load fwCell.bDRAM(0::1, 2*hidden_size::3*hidden_size)
//         fwCell.bO load fwCell.bDRAM(0::1, 3*hidden_size::linear_output_size)
//         // bwCell.bI load bw_bDRAM(0::1, 0::hidden_size)
//         // bwCell.bJ load bw_bDRAM(0::1, hidden_size::2*hidden_size)
//         // bwCell.bF load bw_bDRAM(0::1, 2*hidden_size::3*hidden_size)
//         // bwCell.bO load bw_bDRAM(0::1, 3*hidden_size::linear_output_size)

//         lstmCell.bI load lstmCell.bDRAM(0::1, 0::hidden_size)
//         lstmCell.bJ load lstmCell.bDRAM(0::1, hidden_size::2*hidden_size)
//         lstmCell.bF load lstmCell.bDRAM(0::1, 2*hidden_size::3*hidden_size)
//         lstmCell.bO load lstmCell.bDRAM(0::1, 3*hidden_size::linear_output_size)

//         resCell0.bI load resCell0.bDRAM(0::1, 0::hidden_size)
//         resCell0.bJ load resCell0.bDRAM(0::1, hidden_size::2*hidden_size)
//         resCell0.bF load resCell0.bDRAM(0::1, 2*hidden_size::3*hidden_size)
//         resCell0.bO load resCell0.bDRAM(0::1, 3*hidden_size::linear_output_size)

//         resCell1.bI load resCell1.bDRAM(0::1, 0::hidden_size)
//         resCell1.bJ load resCell1.bDRAM(0::1, hidden_size::2*hidden_size)
//         resCell1.bF load resCell1.bDRAM(0::1, 2*hidden_size::3*hidden_size)
//         resCell1.bO load resCell1.bDRAM(0::1, 3*hidden_size::linear_output_size)
//       }

//       Sequential.Foreach(maxTime by 1) { timeStep =>
//         Pipe {
//           // stage 1: bidirectional encoding
//           Parallel {
//             Pipe {
//               fwCell.x load xDRAM(0::batch_size, timeStep, 0::feature_size)
//               fwCell.forward()
//             }

//             // Pipe {
//             //   bwCell.x load xDRAM(0::batch_size, maxTime - timeStep - 1, 0::feature_size)
//             //   bwCell.forward()
//             // }
//           }

//           // stage 2: pure LSTM encoding
//           lstmCell.forward()
//           // stage 3: res lstm 0 forwarding
//           resCell0.forward()
//           // stage 4: res lstm 1 forwarding
//           resCell1.forward()
//         }
//       }
//     }

//     // write encoder results
//     writeCSV2D(getMatrix(resCellParam1.hDRAM), "./encoder_h_re.csv", ",", "\n")
//     writeCSV2D(getMatrix(resCellParam1.cDRAM), "./encoder_c_re.csv", ",", "\n")
//   }
// }
