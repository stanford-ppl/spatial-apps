import spatial.dsl._
import org.virtualized._
import spatial.SpatialCompiler
import spatial.interpreter.Interpreter

object StreamInOutAdd extends SpatialStream {
  import spatial.targets.DE1

  @virtualize def prog(): Unit = {
    val in  = StreamIn[Int](DE1.GPInput1)
    val out = StreamOut[Int](DE1.GPOutput1)
    Accel(*) {
      out := in + 4
    }
  }
}


object StreamInOutAddInterpreter extends StreamInOutAdd with SpatialStreamInterpreter {

  val outs = List(Out1)

  val inputs = collection.immutable.Map[Bus, List[MetaAny[_]]](
    (In1 -> List[Int](1, 2, 3, 6))
  )

}

object StreamInOutAddCompiler extends StreamInOutAdd with SpatialStreamCompiler
