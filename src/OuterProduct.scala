import spatial.dsl._
import virtualized._

object OuterProduct extends SpatialApp { // Regression (Dense) // Args: 640 640

  type X = FixPt[TRUE,_32,_0]

  val M = 1024 // param
  val N = 1024 // param

  val ts1 = 32 // param
  val ts2 = 16 // param
  val op = 1 // param (1, <N> / <ts2>, 4)
  val ip = 16

  @virtualize
  def outerproduct[T:Type:Num](a: Array[T], b: Array[T]) = {

    val sizeA = ArgIn[Int]
    val sizeB = ArgIn[Int]
    setArg(sizeA, a.length); bound(a.length) = M
    setArg(sizeB, b.length); bound(b.length) = N

    val vec1 = DRAM[T](sizeA)
    val vec2 = DRAM[T](sizeB)
    val out = DRAM[T](sizeA, sizeB)

    setMem(vec1, a)
    setMem(vec2, b)

    Accel {
      Foreach(sizeA by ts1, sizeB by ts2 par op){ (i,j) =>
        val b1 = SRAM[T](ts1)
        val b2 = SRAM[T](ts2)
        val outTile = SRAM[T](ts1, ts2)
        //val blkA = Reg[Int]
        //val blkB = Reg[Int]
        Parallel {
          b1 load vec1(i::i+ts1 par ip)
          b2 load vec2(j::j+ts2 par ip)
        }
        Foreach(ts1 by 1, ts2 par ip){ (ii,jj) => outTile(ii, jj) = b1(ii) * b2(jj) } // 2

        out(i::i+ts1, j::j+ts2 par 16) store outTile
      }
    }
    getMem(out)
  }

  @virtualize
  def main() = {
    // val a = Array.fill(M)(random[T](100))
    // val b = Array.fill(N)(random[T](100))
    val a = Array.tabulate(M) { i => (i % 64).to[X] }
    val b = Array.tabulate(N){ i => (i % 64).to[X] }

    val result = outerproduct(a, b)

    val gold = Array.tabulate(M){i => Array.tabulate(N){j => a(i) * b(j) }}.flatten
    val gold_cksum = gold.map(a => a).reduce{_+_}
    val result_cksum = result.map(a => a).reduce{_+_}
    println("expected cksum: " + gold_cksum)
    println("result cksum:   " + result_cksum)
    // (0 until M*N) foreach { i => assert(result(i) == gold(i)) }

    val cksum = result_cksum == gold_cksum
    println("PASS: " + cksum + " (OuterProduct)")


  }
}

