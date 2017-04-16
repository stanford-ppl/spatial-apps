import spatial._
import org.virtualized._
import forge._

object SwitchStreamingTest extends SpatialApp {
  import IR._

  override val target = targets.DE1

  type UINT2 = FixPt[FALSE,_2,_0]
  type UINT3 = FixPt[FALSE,_3,_0]
  type UINT5 = FixPt[FALSE,_5,_0]
  type UINT6 = FixPt[FALSE,_6,_0]
  type UINT9 = FixPt[FALSE,_9,_0]
  type UINT7 = FixPt[FALSE,_7,_0]
  type UINT32 = FixPt[FALSE,_32,_0]

  @virtualize
  def main() {
    // LEDR example
    val inputSwitch = target.SliderSwitch
    val input = StreamIn[UINT32](inputSwitch)
    val outputLEDR: Bus = target.LEDR
    val output = StreamOut[UINT32](outputLEDR)
    Accel(*) {
      val switchVal = input.value()
      // Assign to a value of switch
      output := switchVal
      // Assign to a constant
    }
  }
}
