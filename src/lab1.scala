import spatial.dsl._
import org.virtualized._

object Lab1Part1RegExample extends SpatialApp {
  type T = Int

  @virtualize
  def main() {
    val N = args(0).to[T]
    val M = args(1).to[T]
    val argRegIn0 = ArgIn[T]
    val argRegIn1 = ArgIn[T]
    setArg(argRegIn0, N)
    setArg(argRegIn1, M)
    val argRegOut = ArgOut[T]

    Accel {
      val regDefault = Reg[T](2.to[T])
      regDefault := 4.to[T]
      val regDefaultValue = regDefault.value
      val argRegIn0Value = argRegIn0.value
      val argRegIn1Value = argRegIn1.value
      argRegOut := regDefaultValue + argRegIn0Value + argRegIn1Value
    }

    val argRegOutResult = getArg(argRegOut)
    println("Result = " + argRegOutResult)

    val gold = M + N + 4.to[T]
    println("Gold = " + gold)
    val chksum = gold == argRegOutResult
    println("PASS = " + chksum)
  }
}


// object Lab1Part2DramSramExample extends SpatialApp {
//
// }
//
//
// object Lab1Part3RegFileExample extends SpatialApp {
//
// }
//
// object Lab1Part4FIFOExample extends SpatialApp {
//
// }
//
// object Lab1Part5FILOExample extends SpatialApp {
//
// }
//
// object Lab1Part6CounterExample extends SpatialApp {
//
// }
//
// object Lab1Part7CounterChainExample extends SpatialApp {
//
// }
//
// object Lab1Part8LineBufferExample extends SpatialApp {
//
// }
//
// object Lab1Part9LUTExample extends SpatialApp {
//
// }
