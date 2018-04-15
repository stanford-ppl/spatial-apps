import spatial.dsl._
import virtualized._

object DotProduct extends SpatialApp { // Regression (Dense) // Args: 640


  type X = FixPt[TRUE,_32,_0]

  val N = 1024 // param
  val innerPar = 16 // param
  val outerPar = 1 // param
  val tileSize = 32 // param

  @virtualize
  def dotproduct[T:Type:Num](aIn: Array[T], bIn: Array[T]): T = {
    val B  = tileSize (32 -> 64 -> 19200)
    val P1 = outerPar (1 -> 6)
    val P2 = innerPar (1 -> 192)
    val P3 = innerPar (1 -> 192)

    bound(aIn.length) = N

    val size = ArgIn[Int]
    setArg(size, aIn.length)

    val a = DRAM[T](size)
    val b = DRAM[T](size)
    val out = ArgOut[T]
    setMem(a, aIn)
    setMem(b, bIn)

    Accel {
      out := Reduce(Reg[T](0.to[T]))(size by B par P1){i =>
        //val ts = Reg[Int](0)
        //ts := min(B, size-i)
        val aBlk = SRAM[T](B)
        val bBlk = SRAM[T](B)
        Parallel {
          //aBlk load a(i::i+ts.value par P3)
          //bBlk load b(i::i+ts.value par P3)
          aBlk load a(i::i+B par P3)
          bBlk load b(i::i+B par P3)
        }
        Reduce(Reg[T](0.to[T]))(B par P2){ii => aBlk(ii) * bBlk(ii) }{_+_}
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

