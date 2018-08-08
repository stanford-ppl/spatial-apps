import spatial.dsl._
import virtualized._


object NMTDecoderResUnit extends SpatialApp with NMTDecoderBasePar {
  @virtualize
  def main() {
    type T = FixPt[TRUE, _8, _24]
    //type T = FixPt[TRUE, _8, _8]
    val feature_size = 512
    val hidden_size = 512

    // For cells
    val dm = 16
    val dp = 16
    val forgetBias = 1
    val batch_size = 1
    val maxTime = 4

    // TODO: for now I'm going to disable all the parallelization factors in this scope to pass compilation
    // pars
    val initPar = 1 
    val outerPar = 1
    val midPar = 1
    val innerPar = 1
    val elePar = 1

    val paramPath = "/home/tianzhao/lstm/lstm-apps/models/nmt/small_test_param_weights_512_512/"
    val output_hidden = NMTDecoderResBase[T](paramPath.asInstanceOf[java.lang.String], batch_size, feature_size, hidden_size, maxTime, dm, dp, initPar, outerPar, midPar, innerPar, elePar)
    writeCSV2D(getMatrix(output_hidden), "./decoder_re.csv")
  }
}

object NMTDecoderRes_512_512_8_8_pars_1_1_1_1_m5 extends SpatialApp with NMTDecoderBasePar {
  @virtualize
  def main() {
    type T = FixPt[TRUE, _8, _8]
    val feature_size = 512
    val hidden_size = 512

    // For cells
    val dm = 16
    val dp = 16
    val forgetBias = 1
    val batch_size = 1
    val maxTime = 4

    // TODO: for now I'm going to disable all the parallelization factors in this scope to pass compilation
    // pars
    val initPar = 8
    val outerPar = 1
    val midPar = 1
    val innerPar = 1
    val elePar = 1

    val paramPath = "/home/tianzhao/lstm/lstm-apps/models/nmt/small_test_param_weights_512_512/"
    val output_hidden = NMTDecoderResBase[T](paramPath.asInstanceOf[java.lang.String], batch_size, feature_size, hidden_size, maxTime, dm, dp, initPar, outerPar, midPar, innerPar, elePar)
    writeCSV2D(getMatrix(output_hidden), "./decoder_re.csv")
  }
}


object NMTDecoderRes_128_128_8_8_pars_1_1_1_1_m5 extends SpatialApp with NMTDecoderBasePar {
  @virtualize
  def main() {
    type T = FixPt[TRUE, _8, _8]
    val feature_size = 128
    val hidden_size = 128

    // For cells
    val dm = 16
    val dp = 16
    val forgetBias = 1
    val batch_size = 1
    val maxTime = 4

    // TODO: for now I'm going to disable all the parallelization factors in this scope to pass compilation
    // pars
    val initPar = 8
    val outerPar = 1
    val midPar = 1
    val innerPar = 1
    val elePar = 1

    val paramPath = "/home/tianzhao/lstm/lstm-apps/models/nmt/small_test_param_weights_128_128/"
    val output_hidden = NMTDecoderResBase[T](paramPath.asInstanceOf[java.lang.String], batch_size, feature_size, hidden_size, maxTime, dm, dp, initPar, outerPar, midPar, innerPar, elePar)
    writeCSV2D(getMatrix(output_hidden), "./decoder_re.csv")
  }
}


object NMTDecoderRes_512_512_8_8_pars_1_1_1_1 extends SpatialApp with NMTDecoderBasePar {
  @virtualize
  def main() {
    type T = FixPt[TRUE, _8, _8]
    val feature_size = 512
    val hidden_size = 512

    // For cells
    val dm = 16
    val dp = 16
    val forgetBias = 1
    val batch_size = 1
    val maxTime = 4

    // TODO: for now I'm going to disable all the parallelization factors in this scope to pass compilation
    // pars
    val initPar = 8
    val outerPar = 1
    val midPar = 1
    val innerPar = 1
    val elePar = 1


    val paramPath = "/home/tianzhao/lstm/lstm-apps/models/nmt/small_test_param_weights_512_512"
    val output_hidden = NMTDecoderResBase[T](paramPath.asInstanceOf[java.lang.String], batch_size, feature_size, hidden_size, maxTime, dm, dp, initPar, outerPar, midPar, innerPar, elePar)
    writeCSV2D(getMatrix(output_hidden), "./decoder_re.csv")
  }
}


object NMTDecoder_128_128_8_8_pars_1_1_1_1 extends SpatialApp with NMTDecoderBasePar {
  @virtualize
  def main() {
    type T = FixPt[TRUE, _8, _8]
    val feature_size = 128
    val hidden_size = 128

    // For cells
    val dm = 16
    val dp = 16
    val forgetBias = 1
    val batch_size = 1
    val maxTime = 4

    // TODO: for now I'm going to disable all the parallelization factors in this scope to pass compilation
    // pars
    val initPar = 8
    val outerPar = 1
    val midPar = 1
    val innerPar = 1
    val elePar = 1


    val paramPath = "/home/tianzhao/lstm/lstm-apps/models/nmt/small_test_param_weights_128_128/"
    val output_hidden = NMTDecoderResBase[T](paramPath.asInstanceOf[java.lang.String], batch_size, feature_size, hidden_size, maxTime, dm, dp, initPar, outerPar, midPar, innerPar, elePar)
    writeCSV2D(getMatrix(output_hidden), "./decoder_re.csv")
  }
}

object NMTEncoderUnit extends SpatialApp with NMTEncoderBasePar {
  @virtualize
  def main() {
    type T = FixPt[TRUE, _8, _24]
    val feature_size = 512
    val hidden_size = 512

    val dm = 16
    val dp = 16
    val batch_size = 1
    val maxTime = 4

    // pars
    val initPar = 1
    val outerPar = 1
    val midPar = 1
    val innerPar = 1
    val elePar = 1

    val paramPath = "/home/tianzhao/spatial-lang-LSTM/apps/models/nmt/small_test_param_weights_512_512/"
    val output_hidden = NMTEncoderBase[T](paramPath.asInstanceOf[java.lang.String], batch_size, feature_size, hidden_size, maxTime, dm, dp, initPar, outerPar, midPar, innerPar, elePar)
    writeCSV2D(getMatrix(output_hidden), "./res1_re.csv")
  }
}

// Slightly smaller design incase none fits...
object NMTEncoder_512_512_3_5_16_16_pars_4_1_4_2 extends SpatialApp with NMTEncoderBasePar {
  @virtualize
  def main() {
    type T = FixPt[TRUE, _4, _4]
    val feature_size = 512
    val hidden_size = 512

    val dm = 16
    val dp = 16
    val batch_size = 1
    val maxTime = 4

    // pars
    val initPar = 2
    val outerPar = 4
    val midPar = 1
    val innerPar = 4
    val elePar = 2

    val paramPath = "/home/tianzhao/spatial-lang-LSTM/apps/models/nmt/small_test_param_weights_512_512/"
    val output_hidden = NMTEncoderBase[T](paramPath.asInstanceOf[java.lang.String], batch_size, feature_size, hidden_size, maxTime, dm, dp, initPar, outerPar, midPar, innerPar, elePar)
    writeCSV2D(getMatrix(output_hidden), "./res1_re.csv")
  }
}


object NMTEncoder_1024_1024_3_5_pars_8_1_4_2 extends SpatialApp with NMTEncoderBasePar {
  @virtualize
  def main() {
    type T = FixPt[TRUE, _3, _5]
    val feature_size = 1024
    val hidden_size = 1024

    val dm = 16
    val dp = 16
    val batch_size = 1
    val maxTime = 4

    val initPar = 2
    val outerPar = 8
    val midPar = 1 //batch_size...par at 1
    val innerPar = 4
    val elePar = 2

    val paramPath = "/home/tianzhao/spatial-lang-LSTM/apps/models/nmt/test_param_weights/"

    val output_hidden = NMTEncoderBase[T](paramPath.asInstanceOf[java.lang.String], batch_size, feature_size, hidden_size, maxTime, dm, dp, initPar, outerPar, midPar, innerPar, elePar)
    writeCSV2D(getMatrix(output_hidden), "./res1_re.csv")
  }
}


object NMTEncoder_64_64_3_5_vcs extends SpatialApp with NMTEncoderBasePar {
  @virtualize
  def main() {
    type T = FixPt[TRUE, _3, _5]
    val feature_size = 64
    val hidden_size = 64

    val dm = 16
    val dp = 16
    val batch_size = 1
    val maxTime = 4

    val initPar = 2
    val outerPar = 1
    val midPar = 1
    val innerPar = 1
    val elePar = 1

    val paramPath = "/home/tianzhao/spatial-lang-LSTM/apps/models/nmt/small_test_param_weights_64_64/"

    val output_hidden = NMTEncoderBase[T](paramPath.asInstanceOf[java.lang.String], batch_size, feature_size, hidden_size, maxTime, dm, dp, initPar, outerPar, midPar, innerPar, elePar)
    writeCSV2D(getMatrix(output_hidden), "./res1_re.csv")
  }
}


object NMTEncoder_64_64_3_5_synth extends SpatialApp with NMTEncoderBasePar {
  @virtualize
  def main() {
    type T = FixPt[TRUE, _3, _5]
    val feature_size = 64
    val hidden_size = 64

    val dm = 16
    val dp = 16
    val batch_size = 1
    val maxTime = 4

    val initPar = 2
    val outerPar = 1
    val midPar = 1
    val innerPar = 1
    val elePar = 1

    val paramPath = "/home/tianzhao/spatial-lang-LSTM/apps/models/nmt/small_test_param_weights_64_64/"
    val output_hidden = NMTEncoderBase[T](paramPath.asInstanceOf[java.lang.String], batch_size, feature_size, hidden_size, maxTime, dm, dp, initPar, outerPar, midPar, innerPar, elePar)
    writeCSV2D(getMatrix(output_hidden), "./res1_re.csv")
  }
}


object NMTEncoder_128_128_3_5_synth extends SpatialApp with NMTEncoderBasePar {
  @virtualize
  def main() {
    type T = FixPt[TRUE, _3, _5]
    val feature_size = 128
    val hidden_size = 128

    val dm = 32
    val dp = 16
    val batch_size = 1
    val maxTime = 4

    val initPar = 2
    val outerPar = 1
    val midPar = 1
    val innerPar = 1
    val elePar = 1

    val paramPath = "/home/tianzhao/spatial-lang-LSTM/apps/models/nmt/small_test_param_weights_128_128/"

    val output_hidden = NMTEncoderBase[T](paramPath.asInstanceOf[java.lang.String], batch_size, feature_size, hidden_size, maxTime, dm, dp, initPar, outerPar, midPar, innerPar, elePar)
    writeCSV2D(getMatrix(output_hidden), "./res1_re.csv")
  }
}


object NMTEncoder_256_256_3_5_synth extends SpatialApp with NMTEncoderBasePar {
  @virtualize
  def main() {
    type T = FixPt[TRUE, _3, _5]
    val feature_size = 256
    val hidden_size = 256

    val dm = 32
    val dp = 16
    val batch_size = 1
    val maxTime = 4

    val initPar = 2
    val outerPar = 1
    val midPar = 1
    val innerPar = 1
    val elePar = 1

    val paramPath = "/home/tianzhao/spatial-lang-LSTM/apps/models/nmt/small_test_param_weights_256_256/"

    val output_hidden = NMTEncoderBase[T](paramPath.asInstanceOf[java.lang.String], batch_size, feature_size, hidden_size, maxTime, dm, dp, initPar, outerPar, midPar, innerPar, elePar)
    writeCSV2D(getMatrix(output_hidden), "./res1_re.csv")
  }
}


object NMTEncoder_512_512_3_5_synth extends SpatialApp with NMTEncoderBasePar {
  @virtualize
  def main() {
    type T = FixPt[TRUE, _3, _5]
    val feature_size = 512
    val hidden_size = 512

    val dm = 32
    val dp = 16
    val batch_size = 1
    val maxTime = 4

    val initPar = 2
    val outerPar = 1
    val midPar = 1
    val innerPar = 1
    val elePar = 1

    val paramPath = "/home/tianzhao/spatial-lang-LSTM/apps/models/nmt/small_test_param_weights_512_512/"

    val output_hidden = NMTEncoderBase[T](paramPath.asInstanceOf[java.lang.String], batch_size, feature_size, hidden_size, maxTime, dm, dp, initPar, outerPar, midPar, innerPar, elePar)
    writeCSV2D(getMatrix(output_hidden), "./res1_re.csv")
  }
}


object NMTEncoder_1024_1024_3_5_synth extends SpatialApp with NMTEncoderBasePar {
  @virtualize
  def main() {
    type T = FixPt[TRUE, _3, _5]
    val feature_size = 1024
    val hidden_size = 1024

    val dm = 32
    val dp = 16
    val batch_size = 1
    val maxTime = 4

    val initPar = 2
    val outerPar = 1
    val midPar = 1
    val innerPar = 1
    val elePar = 1

    val paramPath = "/home/tianzhao/spatial-lang-LSTM/apps/models/nmt/small_test_param_weights_1024_1024/"

    val output_hidden = NMTEncoderBase[T](paramPath.asInstanceOf[java.lang.String], batch_size, feature_size, hidden_size, maxTime, dm, dp, initPar, outerPar, midPar, innerPar, elePar)
    writeCSV2D(getMatrix(output_hidden), "./res1_re.csv")
  }
}


object NMTEncoder_64_64_3_5_pars_4_1_4_2 extends SpatialApp with NMTEncoderBasePar {
  @virtualize
  def main() {
    type T = FixPt[TRUE, _3, _5]
    val feature_size = 64
    val hidden_size = 64

    val dm = 16
    val dp = 16
    val batch_size = 1
    val maxTime = 4

    val initPar = 2
    val outerPar = 4
    val midPar = 1
    val innerPar = 4
    val elePar = 2

    val paramPath = "/home/tianzhao/spatial-lang-LSTM/apps/models/nmt/small_test_param_weights_64_64/"

    val output_hidden = NMTEncoderBase[T](paramPath.asInstanceOf[java.lang.String], batch_size, feature_size, hidden_size, maxTime, dm, dp, initPar, outerPar, midPar, innerPar, elePar)
    writeCSV2D(getMatrix(output_hidden), "./res1_re.csv")
  }
}


trait NMTEncoderBasePar extends SpatialApp with CellsPar {
  @virtualize
  def NMTEncoderBase[T:Type:Num](paramPath: java.lang.String, batch_size: Int, feature_size: Int, hidden_size: Int, maxTime: Int, dm: Int, dp: Int, initPar: Int, outerPar: Int, midPar: Int, innerPar: Int, elePar: Int) (implicit convert : Cast[String, T]) = {
    val xDRAM = DRAM[T](batch_size, maxTime, feature_size)
    setMem(xDRAM, loadCSV1D[T](paramPath+"x_3d.csv", ","))

    val hidStateBufFw = DRAM[T](batch_size, maxTime, feature_size)
    val hidStateBufBw = DRAM[T](batch_size, maxTime, feature_size)

    val fwCellParam = CellParam[T](paramPath, "BiLSTMFw", batch_size, feature_size, hidden_size, dp, dm, false, initPar, outerPar, midPar, innerPar, elePar)
    val bwCellParam = CellParam[T](paramPath, "BiLSTMBw", batch_size, feature_size, hidden_size, dp, dm, false, initPar, outerPar, midPar, innerPar, elePar)
    val fusedCellParam = CellParam[T](paramPath, "FusedLSTM", batch_size, feature_size, hidden_size, dp, dm, isFusedInputs=true, initPar, outerPar, midPar, innerPar, elePar)
    val resCellParam0 = CellParam[T](paramPath, "ResCell0", batch_size, feature_size, hidden_size, dp, dm, false, initPar, outerPar, midPar, innerPar, elePar)
    val resCellParam1 = CellParam[T](paramPath, "ResCell1", batch_size, feature_size, hidden_size, dp, dm, false, initPar, outerPar, midPar, innerPar, elePar)

    Accel {
      val fwCell = BasicLSTMCell[T](fwCellParam, SRAM[T](batch_size, hidden_size))
      val bwCell = BasicLSTMCell[T](bwCellParam, SRAM[T](batch_size, hidden_size))
      val lstmCell = FusedReadsBasicLSTMCell[T](fusedCellParam, fwCell.h, bwCell.h)
      val resCell0 = ResidualBasicLSTMCell[T](resCellParam0, lstmCell.h)
      val resCell1 = ResidualBasicLSTMCell[T](resCellParam1, resCell0.h)

      // Only init the SRAMs to 0 for now
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
  }
}


trait NMTDecoderBasePar extends SpatialApp with CellsPar {
  // Decoder with residual connections
  @virtualize
  def NMTDecoderResBase[T:Type:Num](paramPath: java.lang.String, batch_size: Int, feature_size: Int, hidden_size: Int, maxTime: Int, dm: Int, dp: Int, initPar: Int, outerPar: Int, midPar: Int, innerPar: Int, elePar: Int) (implicit convert : Cast[String, T]) = {
    val xDRAM = DRAM[T](batch_size, maxTime, feature_size)
    setMem(xDRAM, loadCSV1D[T](paramPath+"x_3d.csv", ","))

    val deBasicCell0Param = CellParam[T](paramPath, "DeCell0", batch_size, feature_size, hidden_size, dp, dm, true, initPar, outerPar, midPar, innerPar, elePar)
    val deBasicCell1Param = CellParam[T](paramPath, "DeCell1", batch_size, feature_size, hidden_size, dp, dm, true, initPar, outerPar, midPar, innerPar, elePar)
    val deResCell0Param = CellParam[T](paramPath, "ResCell0", batch_size, feature_size, hidden_size, dp, dm, true, initPar, outerPar, midPar, innerPar, elePar)
    val deResCell1Param = CellParam[T](paramPath, "ResCell1", batch_size, feature_size, hidden_size, dp, dm, true, initPar, outerPar, midPar, innerPar, elePar)


    Accel {
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

    deResCell1Param.hDRAM
  }


  @virtualize
  def NMTDecoderBase[T:Type:Num](paramPath: java.lang.String, batch_size: Int, feature_size: Int, hidden_size: Int, maxTime: Int, dm: Int, dp: Int, initPar: Int, outerPar: Int, midPar: Int, innerPar: Int, elePar: Int) (implicit convert : Cast[String, T]) = {
    // Decoder without residual connections
    val xDRAM = DRAM[T](batch_size, maxTime, feature_size)
    setMem(xDRAM, loadCSV1D[T](paramPath+"x_3d.csv", ","))

    val deBasicCell0Param = CellParam[T](paramPath, "DeCell0", batch_size, feature_size * 2, hidden_size, dp, dm, false, initPar, outerPar, midPar, innerPar, elePar)
    val deBasicCell1Param = CellParam[T](paramPath, "DeCell1", batch_size, feature_size * 2, hidden_size, dp, dm, false, initPar, outerPar, midPar, innerPar, elePar)
    val deResCell0Param = CellParam[T](paramPath, "ResCell0", batch_size, feature_size * 2, hidden_size, dp, dm, false, initPar, outerPar, midPar, innerPar, elePar)
    val deResCell1Param = CellParam[T](paramPath, "ResCell1", batch_size, feature_size * 2, hidden_size, dp, dm, false, initPar, outerPar, midPar, innerPar, elePar)


    Accel {
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

    deResCell1Param.hDRAM
  }
}
