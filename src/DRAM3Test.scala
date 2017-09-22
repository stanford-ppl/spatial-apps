import spatial.dsl._
import org.virtualized._


trait Params extends SpatialApp {
  val N = 3
  val JX = 4
  val dco = 2
  val d = 4
  val dn = 1
  val dJX = 2
  val ddco = 2
  val dd = 2
  val simFileDir = "/home/tianzhao/spatial-lang/apps/np-sims/"
  val dataPaths = List(simFileDir + "/a_3_4_2.csv", simFileDir + "/hidden_3_4_4.csv", 
                    simFileDir + "/kernel_6_16.csv", simFileDir + "/bias_16.csv")
}


// This app does np.concatenate([a, hidden], axis=1).dot(kernel) + bias(broadcasted)
object ConcatAffine extends SpatialApp with Params {
  type T = FixPt[TRUE, _16, _16]

  @virtualize
  def main() {
    val (a, hidden, kernel, bias) = (DRAM[T](N, JX, dco), DRAM[T](N, JX, d), 
                                     DRAM[T](dco+d, 4*d), DRAM[T](4*d))
    val drams = List(a, hidden, kernel, bias)
    drams.zipWithIndex.foreach { case(e, idx) =>
      setMem(e, loadCSV1D[T](dataPaths(idx), ","))
    } 

  }
}



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