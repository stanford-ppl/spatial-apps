import org.virtualized._
import spatial._

object SwitchLED extends SpatialApp {
  import IR._

  override val target = targets.DE1
  type UINT10 = FixPt[FALSE,_10,_0]

  @virtualize 
  def main() { 
    val leds = target.LEDR
    val ledOutput = StreamOut[Int](leds)
    val switch = target.SliderSwitch
    val swInput = StreamIn[Int](switch)
    Accel(*) {
      ledOutput := swInput.value()
    }
  }
}
