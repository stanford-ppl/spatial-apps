import spatial.dsl._
import virtualized._


object Decoder1Cell extends SpatialApp with CellsPar {

  @virtualize
  def NMTDecoderResBase[T:Type:Num](paramPath: java.lang.String, batch_size: Int, feature_size: Int, hidden_size: Int, maxTime: Int, dm: Int, dp: Int, initPar: Int, outerPar: Int, midPar: Int, innerPar: Int, elePar: Int) (implicit convert : Cast[String, T]) = {
    val xDRAM = DRAM[T](batch_size, maxTime, feature_size)
    setMem(xDRAM, loadCSV1D[T](paramPath+"x_3d.csv", ","))

    val deBasicCell0Param = CellParam[T](paramPath, "DeCell0", batch_size, feature_size, hidden_size, dp, dm, true, initPar, outerPar, midPar, innerPar, elePar)
    //val deBasicCell1Param = CellParam[T](paramPath, "DeCell1", batch_size, feature_size, hidden_size, dp, dm, true, initPar, outerPar, midPar, innerPar, elePar)
    //val deResCell0Param = CellParam[T](paramPath, "ResCell0", batch_size, feature_size, hidden_size, dp, dm, true, initPar, outerPar, midPar, innerPar, elePar)
    //val deResCell1Param = CellParam[T](paramPath, "ResCell1", batch_size, feature_size, hidden_size, dp, dm, true, initPar, outerPar, midPar, innerPar, elePar)


    Accel {
      val deBasicCell0 = BasicLSTMCell(deBasicCell0Param, SRAM[T](batch_size, hidden_size))
      //val deBasicCell1 = BasicLSTMCell(deBasicCell1Param, deBasicCell0.h)
      //val deResCell0 = ResidualBasicLSTMCell(deResCell0Param, deBasicCell1.h)
      //val deResCell1 = ResidualBasicLSTMCell(deResCell1Param, deResCell0.h)

      Parallel {
        deBasicCell0.init()
        //deBasicCell1.init()
        //deResCell0.init()
        //deResCell1.init()
      }

      Sequential.Foreach (maxTime by 1) { timeStep =>
        deBasicCell0.x load xDRAM(0::batch_size, timeStep, 0::feature_size)

        deBasicCell0.forward()
        //deBasicCell1.forward()
        //deResCell0.forward()
        //deResCell1.forward()
      }

      deBasicCell0.flush()
      //deResCell1.flush()
    }

    deBasicCell0Param.hDRAM
    //deResCell1Param.hDRAM
  }

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
