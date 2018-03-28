import spatial.dsl._
import org.virtualized._


trait DotProductBaseDesign extends SpatialApp {
  def DPBase[T:Type:Num](size: Int, innerPar: Int, outerPar: Int, tileSize: Int, aIn: Array[T], bIn: Array[T]) = {
    val a = DRAM[T](size)
    val b = DRAM[T](size)
    setMem(a, aIn)
    setMem(b, bIn)
    val result = ArgOut[T]
    Accel {
      val accum = Reg[T](0.to[T])
      Reduce(accum)(size by tileSize par outerPar) { i =>
        val aTile = SRAM[T](tileSize)
        val bTile = SRAM[T](tileSize)

        Parallel {
          aTile load a(i::i+tileSize par innerPar)
          bTile load b(i::i+tileSize par innerPar)
        }

        Foreach (tileSize by 1 par innerPar) { ii =>
          accum := accum.value + aTile(ii) * bTile(ii)
        }
//        Reduce(0)(tileSize by 1 par innerPar){ ii => aTile(ii) * bTile(ii) }{_+_}
      }{_+_}

      result := accum.value
    }

    val accelResult = getArg(result)
    accelResult
  }
}


// Try various design parameters
object DotProduct_1_1_16_Int extends SpatialApp with DotProductBaseDesign {
  type T = Int

  @virtualize
  def main() {
    val size = 32
    val aIn = Array.fill(size){ random[T](size) }
    val bIn = Array.fill(size){ random[T](size) }

    val innerPar = 1
    val outerPar = 1
    val tileSize = 16


    val a = DRAM[T](size)
    val b = DRAM[T](size)
    setMem(a, aIn)
    setMem(b, bIn)
    val result = ArgOut[T]


    val accelResult = DPBase[T](size, innerPar, outerPar, tileSize, aIn, bIn)
    val gold = aIn.zip(bIn){_*_}.reduce{_+_}
    val cksum = gold == accelResult
    println("PASS: " + cksum + "(DotProduct_1_1_16_Int)")
  }
}
