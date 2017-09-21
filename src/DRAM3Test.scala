import spatial.dsl._
import org.virtualized._

trait Params extends SpatialApp {
  val N = 3
  val JX = 4
  val d = 2
  val dn = 1
  val dJX = 2
  val dd = 2
  val simFileDir = "/home/tianzhao/spatial-lang/apps/np-sims/a_3_4_2.csv"
}

object DRAM3Test extends SpatialApp with Params {
  type T = FixPt[TRUE, _16, _16]
  @virtualize
  def main() {
    val dramVal = loadCSV1D[T](simFileDir, ",")
    val dram3 = DRAM[T](3, 4, 2)
    val resultDram = DRAM[T](3, 4, 2)
    setMem(dram3, dramVal)

    Accel {
      val tileA = SRAM[T](dn, dJX, dd)
      val tileB = SRAM[T](dn, dJX, dd)
      Foreach(N by dn, JX by dJX, d by dd) { (i,j,k) =>
        tileA load dram3(i::i+1, j::j+2, k::k+2)
        Foreach(dn by 1, dJX by 1, dd by 1) { (ii,jj,kk) =>
          tileB(ii,jj,kk) = tileA(ii,jj,kk) * 2
        }

        resultDram(i::i+dn, j::j+dJX, k::k+dd) store tileB
      }
    }

    val result = getMem(resultDram)
    printArray(result, "Result: ")

  }
}


object DRAM3ConcatTest extends SpatialApp with Params {
  type T = FixPt[TRUE, _16, _16] 

  @virtualize
  def main() {


  }
}