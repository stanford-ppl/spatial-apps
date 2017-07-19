import spatial.dsl._
import org.virtualized._
import spatial.interpreter._

object StreamInOutMultInterpreter extends SpatialStream with SpatialStreamInterpreter {
  
  @virtualize def prog(): spatial.dsl.Unit = {
    val in  = StreamIn[Int](In1)
    val out = StreamOut[Int](Out1)
    Accel(*) {
      out := in + 4
    }
  }

  val outs = List(In1)

  val inputs = Map[Bus, List[MetaAny[_]]](
    (In1 -> List[Int](3, 4, 2, 6))
  )
  

}
