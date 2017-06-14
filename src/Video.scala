import org.virtualized._
import spatial.dsl._

object Video extends SpatialApp {


  override val target = targets.DE1
  type UINT10 = FixPt[FALSE,_10,_0]
  type UINT2 = FixPt[FALSE,_2,_0]
  type UINT3 = FixPt[FALSE,_3,_0]
  type UINT5 = FixPt[FALSE,_5,_0]
  type UINT6 = FixPt[FALSE,_6,_0]
  type UINT8 = FixPt[FALSE,_8,_0]
  type UINT7 = FixPt[FALSE,_7,_0]
  type UINT32 = FixPt[FALSE,_32,_0]

  @struct case class BGR(b: UINT5, g: UINT6, r: UINT5)

  @virtualize 
  def main() { 
//    val switch = target.SliderSwitch
//    val swInput = StreamIn[Int](switch)
    val onboardVideo = target.VideoCamera
    val outputVideo: Bus = target.VGA
    val input  = StreamIn[BGR](onboardVideo)
    val output = StreamOut[BGR](outputVideo)

    Accel(*) {
//      io1 := swInput.value()
//      val pixel = input.value()
//      output := BGR(pixel.b, pixel.g, pixel.r)
      output := input.value()
    }

//    val r1 = getArg(io1)
//    println("received: " + r1)
  }
}
