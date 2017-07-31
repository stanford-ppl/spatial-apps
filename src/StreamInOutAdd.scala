import spatial.dsl._
import org.virtualized._
import spatial.interpreter._

trait StreamInOutAdd extends SpatialStream {

  @virtualize def spatial() = {
    val in  = StreamIn[Int](In1)
    val out = StreamOut[Int](Out1)    
    Accel(*) {
      out := in + 4
      breakpoint
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
