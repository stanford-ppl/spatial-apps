import spatial.dsl._
import virtualized._

object DotProduct__N_1048576_ts_65536_op_1 extends SpatialApp { // Regression (Dense) // Args: 640


  type X = FixPt[TRUE,_32,_0]

val N = 1048576
val ts = 65536
val op = 1
  val ip = 16

  @virtualize
  def dotproduct[T:Type:Num](aIn: Array[T], bIn: Array[T]): T = {

    bound(aIn.length) = N

    val size = ArgIn[Int]
    setArg(size, aIn.length)

    val a = DRAM[T](size)
    val b = DRAM[T](size)
    val out = ArgOut[T]
    setMem(a, aIn)
    setMem(b, bIn)

    Accel {
      out := Reduce(Reg[T](0.to[T]))(size by ts par op){i =>
        val aBlk = SRAM[T](ts)
        val bBlk = SRAM[T](ts)
        Parallel {
          aBlk load a(i::i+ts par ip)
          bBlk load b(i::i+ts par ip)
        }
        Reduce(Reg[T](0.to[T]))(ts par ip){ii => aBlk(ii) * bBlk(ii) }{_+_}
      }{_+_}
    }
    getArg(out)
  }

  @virtualize
  def main() {
    val a = Array.fill(N){ random[X](4) }
    val b = Array.fill(N){ random[X](4) }

    val result = dotproduct(a, b)
    val gold = a.zip(b){_*_}.reduce{_+_}

    println("expected: " + gold)
    println("result: " + result)

    val cksum = gold == result
    println("PASS: " + cksum + " (DotProduct)")
  }
}

