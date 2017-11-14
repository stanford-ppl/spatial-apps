import spatial.dsl._
import org.virtualized._

object BankingTests extends SpatialApp {
  val R = 32; val C = 16
  val P = 1;  val Q = 4

  @virtualize
  def main() {
    val dram = DRAM[Int](R,C)

    Accel {
      val x = SRAM[Int](R,C)

      Foreach(0 until R, 0 until C par Q){(i,j) =>
        x(i,j) = i + j
      }
      dram store x
    }

    val data = getMatrix(dram)
    printMatrix(data, "data")
  }
}