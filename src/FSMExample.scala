import spatial._
import org.virtualized._

object FSMExample extends SpatialApp {
  import IR._

  override val target = targets.DE1

  type UINT2 = FixPt[FALSE,_2,_0]
  type UINT3 = FixPt[FALSE,_3,_0]
  type UINT5 = FixPt[FALSE,_5,_0]
  type UINT6 = FixPt[FALSE,_6,_0]
  type UINT8 = FixPt[FALSE,_8,_0]
  type UINT7 = FixPt[FALSE,_7,_0]
  type UINT32 = FixPt[FALSE,_32,_0]

  @struct case class bBgGrR(tlll: UINT3, b: UINT5, tll: UINT2, g: UINT6, tl: UINT3, r: UINT5)
  @struct case class BGR(b: UINT5, g: UINT6, r: UINT5)

  @virtualize
  def main() {
    val switch = target.SliderSwitch
    val onboardVideo = target.VideoCamera
    val outputVideo: Bus = target.VGA
    val swInput = StreamIn[UINT32](switch)
    val input  = StreamIn[bBgGrR](onboardVideo)
    val output = StreamOut[BGR](outputVideo)

    Accel(*) {
      // Version 1
      FSM[UINT32]{ state => true }{state =>
        if (state > 0) {
          val pixel = input.value()
          output := BGR(pixel.b, pixel.g, pixel.r)
        } else {
          val pixel = input.value()
          val gray = (pixel.r.to[UINT8] + pixel.g.to[UINT8] + pixel.b.to[UINT8]) / 3
          output := BGR(gray.to[UINT5], gray.to[UINT6], gray.to[UINT5])
        }
      }{ state =>
        swInput.value()
      }
    }
  }
}

