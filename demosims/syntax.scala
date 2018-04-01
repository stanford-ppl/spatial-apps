type T = Int

val a = DRAM[T](size)
val b = DRAM[T](size)

val aIn = Array.fill(size){ random[Int](size) }
val bIn = Array.fill(size){ random[Int](size) }
