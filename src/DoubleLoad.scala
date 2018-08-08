import spatial.dsl._
import virtualized._

object DoubleLoad extends SpatialApp { // Regression (Dense) // Args: 640 640

  type X = FixPt[TRUE,_32,_0]

  val M = 1024 // param pmuSize / 16 * 3
  val ts = 32 // param [pmuSize / 1024] | <N> % p == 0
  val op = 1 // param (2, 16, 2) | <M> / <ts> % p == 0
  val ip = 16

  @virtualize
  def outerproduct[T:Type:Num](a: Array[T]) = {

    val sizeA = ArgIn[Int]
    setArg(sizeA, a.length); bound(a.length) = M

    val vec1 = DRAM[T](sizeA)
    val out = DRAM[T](sizeA)

    setMem(vec1, a)

    Accel {
      Sequential.Foreach(sizeA by ts par op){ i =>
        val b1 = SRAM[T](ts)
        val b2 = SRAM[T](ts)
        b1 load vec1(i::i+ts par ip)
        Sequential.Foreach(3 by 1) { j =>
          Sequential.Foreach(ts par ip){ ii => 
            b1(ii) = 3.to[T]
            val x = b1(ii) + 1.to[T]
            b2(ii) = b1(ii+1) + x
          }
          Foreach(ts par ip){ ii => 
            b2(ii) = b1(ii) + 4.to[T]
          }
        }
        out(i::i+ts par ip) store b2
      }
    }
    getMem(out)
  }

  @virtualize
  def main() = {
    val a = Array.tabulate(M) { i => (i % 64).to[X] }
    outerproduct(a)
  }
}

