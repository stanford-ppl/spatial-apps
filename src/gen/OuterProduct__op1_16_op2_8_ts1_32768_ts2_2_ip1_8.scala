import spatial.dsl._
import virtualized._

object OuterProduct__op1_16_op2_8_ts1_32768_ts2_2_ip1_8 extends SpatialApp { // Regression (Dense) // Args: 640 640

  type X = FixPt[TRUE,_32,_0]

val op1 = 16
val op2 = 8
val ts1 = 32768
val ts2 = 2
  val M = op1 * ts1 * 4
  val N = op2 * ts2 * 4
  val ip = 16
val ip1 = 8
  val ip2 = ip / ip1

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
      Foreach(sizeA by ts1 par op1){ i =>
        val b1 = SRAM[T](ts1)
        b1 load vec1(i::i+ts1 par ip)
        Foreach(sizeB by ts2 par op2) { j =>
          val b2 = SRAM[T](ts2)
          b2 load vec2(j::j+ts2 par ip)
          val outTile = SRAM[T](ts1, ts2)
          Foreach(ts1 by ip2, ts2 par ip1){ (ii,jj) => outTile(ii, jj) = b1(ii) * b2(jj) } // 2
          out(i::i+ts1, j::j+ts2 par ip) store outTile
        }
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
