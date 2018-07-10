import spatial.dsl._
import spatial.targets._
import virtualized._

object GDA__C_128_R_4096_ts_256_op_4_mp_2 extends SpatialApp { // Regression (Dense) // Args: 64

  type X = Float

val C = 128
val R = 4096

val ts = 256
val op = 4
val mp = 2

  val ip = 16
  val margin = 1

  @virtualize
  def gda[T: Type : Num](xCPU: Array[T], yCPU: Array[Int], mu0CPU: Array[T], mu1CPU: Array[T]) = {

    val R = ArgIn[Int]
    setArg(R, yCPU.length); bound(yCPU.length) = GDA.R

    val x = DRAM[T](R, C)
    val y = DRAM[Int](R)
    val mu0 = DRAM[T](C)
    val mu1 = DRAM[T](C)
    val sigma = DRAM[T](C, C)

    setMem(x, xCPU)
    setMem(y, yCPU)
    setMem(mu0, mu0CPU)
    setMem(mu1, mu1CPU)

    Accel {
      val mu0Tile = SRAM[T](C)
      val mu1Tile = SRAM[T](C)
      Parallel {
        mu0Tile load mu0(0 :: C par 16) // Load mu0
        mu1Tile load mu1(0 :: C par 16) // Load mu1
      }

      val sigmaOut = SRAM[T](C, C)

      MemReduce(sigmaOut)(R by ts par op){ r =>
        val gdaYtile = SRAM[Int](ts)
        val gdaXtile = SRAM[T](ts, C)
        val blk = Reg[Int]
        Parallel {
          gdaYtile load y(r :: r + ts par 16)
          gdaXtile load x(r :: r + ts, 0 :: C par 16) // Load tile of x
        }

        val sigmaBlk = SRAM[T](C, C)

        MemReduce(sigmaBlk)(ts par mp) { rr =>
          val subTile = SRAM[T](C)
          val sigmaTile = SRAM[T](C, C)
          Foreach(C par ip) { cc =>
            subTile(cc) = gdaXtile(rr, cc) - mux(gdaYtile(rr) == 1, mu1Tile(cc), mu0Tile(cc))
          }
          Foreach(C by 1, C par ip) { (ii, jj) =>
            sigmaTile(ii, jj) = subTile(ii) * subTile(jj)
          }
          sigmaTile
        }{_+_}
      }{_+_}

      sigma(0 :: C, 0 :: C par 16) store sigmaOut
    }

    getMem(sigma)
  }


  def printArr(a: Array[Int], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  @virtualize
  def main() {


    val x = Array.tabulate(R) { i => Array.tabulate(C) { j => (i * C + j) % 256 } }
    val ys = Array.tabulate(R) { i => i % 256 }
    val mu0 = Array.tabulate(C) { i => i % 2 }
    val mu1 = Array.tabulate(C) { i => i % 2 }

    val result = gda(x.flatten, ys, mu0, mu1)

    val gold = x.zip(ys) { (row, y) =>
      val sub = if (y == 1) row.zip(mu1){_-_} else row.zip(mu0) {_-_}
      Array.tabulate(C) { i => Array.tabulate(C) { j => sub(i) * sub(j) } }.flatten
    }.reduce { (a, b) => a.zip(b) {_+_} }

    printArr(gold, "gold: ")
    printArr(result, "result: ")

    val cksum = gold.zip(result){ case (a,b) => a < b + margin && a > b - margin }.reduce{_&&_}
    println("PASS: " + cksum  + " (GDA)")



  }

}
