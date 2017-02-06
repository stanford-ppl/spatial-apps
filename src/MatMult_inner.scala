import spatial._
import org.virtualized._

object MatMult_inner extends SpatialApp {
  import IR._

  type X = Int //FixPt[Signed,B16,B16]

  val tileSizeM = 8
  val tileSizeN = 192
  val tileSizeP = 192
  val innerPar = 32
  val midPar = 1
  val outerPar = 10
  val storePar = 1

  @virtualize
  def MatMult_inner[T:Num](A: Array[T], B: Array[T], mm: Int, nn: Int, pp: Int) = {
    val M = ArgIn[Int]
    val N = ArgIn[Int]
    val P = ArgIn[Int]
    setArg(M,mm)
    setArg(N,nn)
    setArg(P,pp)

    val a = DRAM[T](M, P)
    val b = DRAM[T](P, N)
    val c = DRAM[T](M, N)

    val bm = tileSizeM (1 -> 1536)
    val bn = tileSizeN (96 -> 96 -> 1536)
    val bp = tileSizeP (96 -> 96 -> 1536)

    val op = outerPar (1 -> 6)
    val mp = midPar   (1 -> 96)
    val ip = innerPar (1 -> 96)
    val px = 1 (1 -> 1) // Cannot parallelize accum across k blocks
    val stPar = storePar (1 -> 1)

    setMem(a, A)
    setMem(b, B)

    Accel {
      Foreach(M by bm, N by bn par op){(i,j) =>
        val tileC = SRAM[T](bm, bn)

        Foreach(P by bp par px){k =>
          val tileA = SRAM[T](bm, bp)
          val tileB = SRAM[T](bp, bn)
          Parallel {
            tileA load a(i::i+bm, k::k+bp) // Reads M*N*P times
            tileB load b(k::k+bp, j::j+bn)
          }
          Foreach(bm by 1, bn by 1 par mp){ (ii,jj) =>    // MetaPipe?
            val prod = Reduce(Reg[T])(bp by 1 par ip){kk => tileA(ii, kk) * tileB(kk, jj) }{_+_}
            val prev = mux(k == 0, 0.as[T], tileC(ii,jj))
            tileC(ii,jj) = prev + prod.value // Is a unit pipe that should be recognized as accum
          }
        }
        c(i::i+bm, j::j+bn par stPar) store tileC // Writes M*N times
      }
    }
    getMem(c)
  }

  @virtualize
  def main() = {
    val M = args(0).to[Int]
    val N = args(1).to[Int]
    val P = args(2).to[Int]

    val a = Array.fill(M){ Array.fill(P){ 1.as[X] } }
    val b = Array.fill(P){ Array.fill(N){ 1.as[X] } }
    // val a = Array.fill(M){ Array.fill(P){random[T](100)} }
    // val b = Array.fill(P){ Array.fill(N){random[T](100)} }

    val result = MatMult_inner(a.flatten, b.flatten, M, N, P)

    val gold = Array.tabulate(M){i =>
      val aRow = a(i)
      Array.tabulate(N){j =>
        val bCol = b.map{row => row(j)}
        aRow.zip(bCol){_*_}.reduce{_+_}
      }
    }.flatten

    println("expected cksum: " + gold.map(a => a).reduce{_+_})
    println("result cksum: " + result.map(a => a).reduce{_+_})

    val cksum = result.zip(gold){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (MatMult_inner)")

  }


}
