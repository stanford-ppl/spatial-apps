import spatial.dsl._
import org.virtualized._



object DotProduct_1_1_16_Int_Foreach extends SpatialApp {
  type T = Int

  @virtualize
  def main() {
    val size = 256
    // val aIn = Array.fill(size){ random[T](size) }
    // val bIn = Array.fill(size){ random[T](size) }
    val aIn = Array.tabulate(size){ i => (i % 16).to[T] }
    val bIn = Array.tabulate(size){ i => (i % 16).to[T] }

    val innerPar = 1
    val outerPar = 1
    val tileSize = 16


    val a = DRAM[T](size)
    val b = DRAM[T](size)
    setMem(a, aIn)
    setMem(b, bIn)
    val result = ArgOut[T]

    Accel {
      val accum = Reg[T](0.to[T])
      Sequential.Foreach (size by tileSize par outerPar) { i =>
        val aTile = SRAM[T](tileSize)
        val bTile = SRAM[T](tileSize)

        Parallel {
          aTile load a(i::i+tileSize par innerPar)
          bTile load b(i::i+tileSize par innerPar)
        }

        Sequential.Foreach (tileSize by 1 par innerPar) { ii =>
          accum := aTile(ii) * bTile(ii) + accum.value
        }
      }

      result := accum.value
    }

    val accelResult = getArg(result)
    val gold = aIn.zip(bIn){_*_}.reduce{_+_}
    val cksum = gold == accelResult
    println("accelResult = " + accelResult)
    println("gold = " + gold)
    println("PASS: " + cksum + "(DotProduct_1_1_16_Int_Foreach)")
  }
}


object DotProduct_1_1_16_Int extends SpatialApp {
  type T = Int

  @virtualize
  def main() {
    val size = 256
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

    Accel {
      val accum = Reg[T](0.to[T])
      Reduce(accum)(size by tileSize par outerPar) { i =>
        val aTile = SRAM[T](tileSize)
        val bTile = SRAM[T](tileSize)

        Parallel {
          aTile load a(i::i+tileSize par innerPar)
          bTile load b(i::i+tileSize par innerPar)
        }

        Reduce(0)(tileSize by 1 par innerPar){ ii => aTile(ii) * bTile(ii) }{_+_}
      }{_+_}

      result := accum.value
    }

    val accelResult = getArg(result)
    val gold = aIn.zip(bIn){_*_}.reduce{_+_}
    val cksum = gold == accelResult
    println("PASS: " + cksum + "(DotProduct_1_1_16_Int)")
  }
}


object DotProduct_16_16_32_Int extends SpatialApp {
  type T = Int

  @virtualize
  def main() {
    val size = 32
    val aIn = Array.fill(size){ random[T](size) }
    val bIn = Array.fill(size){ random[T](size) }

    val innerPar = 16
    val outerPar = 16
    val tileSize = 32


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

        Reduce(0)(tileSize by 1 par innerPar){ ii => aTile(ii) * bTile(ii) }{_+_}
      }{_+_}

      result := accum.value
    }

    val accelResult = getArg(result)
    val gold = aIn.zip(bIn){_*_}.reduce{_+_}
    val cksum = gold == accelResult
    println("PASS: " + cksum + "(DotProduct_16_16_32_Int)")
  }
}



// TODO: FixPt[FALSE,_12,_0] wasn't working fine. Seems that * is not defined for that number type.
// Why?
object DotProduct_4_4_16_Int extends SpatialApp {
  type T = Int

  @virtualize
  def main() {
    val size = 256
    val aIn = Array.fill(size){ random[T](size) }
    val bIn = Array.fill(size){ random[T](size) }

    val innerPar = 4
    val outerPar = 4
    val tileSize = 16


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

        Reduce(0)(tileSize by 1 par innerPar){ ii => aTile(ii) * bTile(ii) }{_+_}
      }{_+_}

      result := accum.value
    }

    val accelResult = getArg(result)
    val gold = aIn.zip(bIn){_*_}.reduce{_+_}
    val cksum = gold == accelResult
    println("PASS: " + cksum + "(DotProduct_4_4_32_Int)")
  }
}


// VCS tests. Cycles for various apps seems not accurate
object DotProduct_4_4_32_UInt12_more_cyles extends SpatialApp {
  type T = Int

  @virtualize
  def main() {
    val size = 256
    val aIn = Array.fill(size){ random[T](size) }
    val bIn = Array.fill(size){ random[T](size) }

    val innerPar = 4
    val outerPar = 4
    val tileSize = 16


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

        Reduce(0)(tileSize by 1 par innerPar){ ii => aTile(ii) * bTile(ii) }{_+_}
      }{_+_}

      result := accum.value
    }

    val accelResult = getArg(result)
    val gold = aIn.zip(bIn){_*_}.reduce{_+_}
    val cksum = gold == accelResult
    println("PASS: " + cksum + "(DotProduct_4_4_4_UInt12)")
  }
}


object DotProduct_1_1_4_Int_fewer_cycles extends SpatialApp {
  type T = Int

  @virtualize
  def main() {
    val size = 128
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

    Accel {
      val accum = Reg[T](0.to[T])
      Reduce(accum)(size by tileSize par outerPar) { i =>
        val aTile = SRAM[T](tileSize)
        val bTile = SRAM[T](tileSize)

        Parallel {
          aTile load a(i::i+tileSize par innerPar)
          bTile load b(i::i+tileSize par innerPar)
        }

        Reduce(0)(tileSize by 1 par innerPar){ ii => aTile(ii) * bTile(ii) }{_+_}
      }{_+_}

      result := accum.value
    }

    val accelResult = getArg(result)
    val gold = aIn.zip(bIn){_*_}.reduce{_+_}
    val cksum = gold == accelResult
    println("PASS: " + cksum + "(DotProduct_1_1_4_Int_fewer_cycles )")
  }
}
