import org.virtualized._
import spatial._

object SwitchHostIO extends SpatialApp {
  import IR._

  override val target = targets.DE1
  type UINT10 = FixPt[FALSE,_10,_0]

  @virtualize 
  def main() { 
    val io1 = HostIO[Int]
    val switch = target.SliderSwitch
    val swInput = StreamIn[Int](switch)
    Accel(*) {
      Pipe { io1 := swInput.value() }
    }

    val r1 = getArg(io1)
    println("received: " + r1)
  }
}
