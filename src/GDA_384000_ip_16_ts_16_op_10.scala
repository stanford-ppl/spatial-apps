import org.virtualized._
import spatial._

object GDA_384000_ip_16_ts_16_op_10 extends SpatialApp { // Regression (Dense) // Args: 64
  import IR._

  type X = Int

  val MAXC = 96
  val C = MAXC
  val margin = 1

val ip = 16
val op = 10

val ts = 16

  @virtualize
  def gda[T: Type : Num](xCPU: Array[T], yCPU: Array[Int], mu0CPU: Array[T], mu1CPU: Array[T]) = {
    val rTileSize = ts(96 -> 19200)
val op = 10
val ip = 16
    val subLoopPar = ip(1 -> 16)
    val prodLoopPar = ip(1 -> 96)
    val outerAccumPar = ip(1 -> 1)

    val rows = yCPU.length;
    bound(rows) = 360000
    val cols = mu0CPU.length;
    bound(cols) = MAXC

    val R = ArgIn[Int]
    // val C = ArgIn[SInt]
    // setArg(C, cols)
    setArg(R, rows)

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
      val mu0Tile = SRAM[T](MAXC)
      val mu1Tile = SRAM[T](MAXC)
      Parallel {
        mu0Tile load mu0(0 :: C par 16) // Load mu0
        mu1Tile load mu1(0 :: C par 16) // Load mu1
      }

      val sigmaOut = SRAM[T](MAXC, MAXC)

      MemReduce(sigmaOut)(R by rTileSize par op){ r =>
        val gdaYtile = SRAM[Int](rTileSize)
        val gdaXtile = SRAM[T](rTileSize, MAXC)
        val blk = Reg[Int]
        Parallel {
          gdaYtile load y(r :: r + rTileSize par 16)
          gdaXtile load x(r :: r + rTileSize, 0 :: C par 16) // Load tile of x
          Pipe {
            blk := min(R.value - r, rTileSize)
          }
        }

        val sigmaBlk = SRAM[T](MAXC, MAXC)

        MemReduce(sigmaBlk)(blk par param(1)) { rr =>
          val subTile = SRAM[T](MAXC)
          val sigmaTile = SRAM[T](MAXC, MAXC)
          Foreach(C par subLoopPar) { cc =>
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
    val R = args(0).to[Int]
    // val C = args(0).to[SInt] // TODO: Should be selectable up to maximum

    // val x  = Array.fill(R){ Array.fill(C){ random[X](10) }}
    // val ys = Array.fill(R){ random[Int](1) }
    // val mu0 = Array.fill(C){ random[X](10) }
    // val mu1 = Array.fill(C){ random[X](10) }

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

    // // println("actual: " + gold.mkString(", "))
    // //println("result: " + result.mkString(", "))
    // // println("Sum of differences: " + gold.zip(result){_-_}.reduce{_+_})
    // printArr(gold, "gold: ")
    // printArr(result, "result: ")

    // val cksum = gold.zip(result){ case (a,b) => a < b + margin && a > b - margin }.reduce{_&&_}
    // println("PASS: " + cksum  + " (GDA)")

    // assert( result == gold )
  }

}
