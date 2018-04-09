import spatial.dsl._
import org.virtualized._
import spatial.targets.DE1
import spatial.targets.Arria10


object Arria10MovingBox extends SpatialApp {
  override val target = Arria10

  val Cmax = 1280
  val Rmax = 720
  // 720 / 4 = 180, set at 0, 180, 360, 540
  @struct case class Pixel32(b: UInt8, g: UInt8, r: UInt8, a: UInt8)

  @virtualize
  def convolveVideoStream(): Unit = {
    val videoCamDRAM = DRAM[Pixel32](Rmax, Cmax)
    val dpDRAM = DRAM[Pixel32](Rmax, Cmax)

    val dwell = ArgIn[Int]
    val d = args(0).to[Int]
    setArg(dwell, d)

    // Arria10 uses argb format
    Accel (*) {
      Foreach(0 until 4) { i =>
        Foreach(0 until dwell) { _ =>
          Foreach(0 until Rmax) { r =>
            val frameST = SRAM[Pixel32](1, Cmax)
            Foreach(0 until Cmax) { c =>
              val pixel1 = mux(r > 180 && r < 360 && c > 180 && c < 360, Pixel32(255,0,127,0), Pixel32(255,0,0,0))
              val pixel2 = mux(r > 360 && r < 540 && c > 180 && c < 360, Pixel32(255,127,0,0), Pixel32(255,0,0,0))
              val pixel3 = mux(r > 360 && r < 540 && c > 360 && c < 540, Pixel32(255,0,0,127), Pixel32(255,0,0,0))
              val pixel4 = mux(r > 180 && r < 360 && c > 360 && c < 540, Pixel32(255,127,0,0), Pixel32(255,0,0,0))
              frameST(0, c) = mux(i == 0, pixel1, mux(i == 1, pixel2, mux( i == 2, pixel3, mux(i == 3, pixel4, Pixel32(255,0,0,0)))))
            }

            dpDRAM(i::i+1, 0::Cmax) store frameST
          }
        }
      }
    }
  }

  @virtualize
  def main() {
    val R = Rmax
    val C = Cmax
    convolveVideoStream()
  }

}


// This version is used for verifying if I can use bursts to replace streams
object RGBArria10Example extends SpatialApp {
  override val target = Arria10

  val Cmax = 1280
  val Rmax = 720

  @struct case class Pixel32(b: UInt8, g: UInt8, r: UInt8, a: UInt8)
  // type Pixel32 = Int

  @virtualize
  def videoStream(): Unit = {
    // val imgOut = BufferedOut[Pixel32](Arria10.DP)
    // val imgIn = BufferedIn[Pixel32](Arria10.VideoCamera)

    // val aIn = (0::Rmax, 0::Cmax){(i, j) => random[Pixel32](3)}

    val videoCamDRAM = DRAM[Pixel32](Rmax, Cmax)
    val dpDRAM = DRAM[Pixel32](Rmax, Cmax)

    // setMem(videoCamDRAM, aIn)

    Accel (*) {
      Foreach(0 until Rmax) { i =>
        val frameLD = SRAM[Pixel32](1, Cmax)
        val frameST = SRAM[Pixel32](1, Cmax)

        frameLD load videoCamDRAM(i::i+1, 0::Cmax)
        Foreach (0 until Cmax) { j =>
          val rgbPixel = frameLD(0,j)
          frameST(0,j) = rgbPixel
        }

        dpDRAM(i::i+1, 0::Cmax) store frameST
      }
    }

    val resultDP = getMatrix(dpDRAM)
    printMatrix(resultDP, "DP value")

  }

  @virtualize
  def main() {
    val R = Rmax
    val C = Cmax
    videoStream()
  }
}



// This version assumes that the pixels are read in one-by-one
// object RGBArria10 extends SpatialApp {
//   override val target = Arria10
//
//   val Cmax = 1280
//   val Rmax = 720
//
//   @struct case class Pixel32(b: UInt8, g: UInt8, r: UInt8, a: UInt8)
//
//   @virtualize
//   def videoStream(): Unit = {
//     val imgOut = BufferedOut[Pixel32](Arria10.DP)
//     val imgIn = BufferedIn[Pixel32](Arria10.VideoCamera)
//
//     Accel (*) {
//       Foreach(0 until Rmax, 0 until Cmax){ (r, c) =>
//         val pixel = imgIn.value(r, c)
//         imgOut(r, c) = pixel
//       }
//     }
//   }
//
//   @virtualize
//   def main() {
//     val R = Rmax
//     val C = Cmax
//     videoStream()
//   }
// }



object MovingBoxArria10 extends SpatialApp {
  override val target = Arria10

  val Cmax = 1280
  val Rmax = 720

  @struct case class Pixel32(b: UInt8, g: UInt8, r: UInt8, a: UInt8)

  @virtualize
  def convolveVideoStream(): Unit = {
    val imgOut = BufferedOut[Pixel32](Arria10.DP)
    val imgIn = BufferedIn[Pixel32](Arria10.VideoCamera)
    val dwell = ArgIn[Int]
    val d = args(0).to[Int]
    setArg(dwell, d)

    Accel (*) {
      Foreach(0 until 4) { i =>
        Foreach(0 until dwell) { _ =>
          Foreach(0 until Rmax, 0 until Cmax){ (r, c) =>
            val pixel = imgIn.value(r, c)
            imgOut(r, c) = pixel
          }
        }
      }
    }
  }

  @virtualize
  def main() {
    val R = Rmax
    val C = Cmax
    convolveVideoStream()
  }
}

object MovingBox extends SpatialApp {
  override val target = DE1

  val Cmax = 320
  val Rmax = 240

  @struct case class Pixel16(b: UInt5, g: UInt6, r: UInt5)

  @virtualize
  def convolveVideoStream(): Unit = {
    val imgOut = BufferedOut[Pixel16](DE1.VGA)
    val dwell = ArgIn[Int]
    val d = args(0).to[Int]
    setArg(dwell, d)

    Accel (*) {
      Foreach(0 until 4) { i =>
        Foreach(0 until dwell) { _ =>
          Foreach(0 until Rmax, 0 until Cmax){ (r, c) =>
            val pixel1 = mux(r > 60 && r < 120 && c > 60 && c < 120, Pixel16(0,63,0), Pixel16(0,0,0))
            val pixel2 = mux(r > 120 && r < 180 && c > 60 && c < 120, Pixel16(31,0,0), Pixel16(0,0,0))
            val pixel3 = mux(r > 120 && r < 180 && c > 120 && c < 180, Pixel16(0,0,31), Pixel16(0,0,0))
            val pixel4 = mux(r > 60 && r < 120 && c > 120 && c < 180, Pixel16(31,0,0), Pixel16(0,0,0))
            val pixel = mux(i == 0, pixel1, mux( i == 1, pixel2, mux( i == 2, pixel3, mux(i == 3, pixel4, Pixel16(0,0,0)))))
            imgOut(r, c) = pixel
          }
        }
      }
    }
  }

  @virtualize
  def main() {
    val R = Rmax
    val C = Cmax
    convolveVideoStream()
  }
}

object ColoredLines extends SpatialApp { // try arg = 100
  override val target = DE1
  val Cmax = 320
  val Rmax = 240



  @struct case class Pixel16(b: UInt5, g: UInt6, r: UInt5)

  @virtualize
  def convolveVideoStream(): Unit = {
    val dwell = ArgIn[Int]
    val d = args(0).to[Int]
    setArg(dwell, d)
    val imgOut = BufferedOut[Pixel16](target.VGA)

    Accel (*) {
      Foreach(0 until Rmax) { i =>
        Foreach(0 until dwell) { _ =>
          Foreach(0 until Rmax, 0 until Cmax){ (r, c) =>
            val bluepixel = mux(r == i, Pixel16((r%32).as[UInt5],0,0), Pixel16(0,0,0))
            val greenpixel = mux(r == i, Pixel16(0,(r%32).as[UInt6],0), Pixel16(0,0,0))
            val redpixel = mux(r == i, Pixel16(0,0,(r%32).as[UInt5]), Pixel16(0,0,0))
            val pixel = mux(r < (Rmax/3), bluepixel, mux(r < (2*Rmax/3), greenpixel, redpixel))
            imgOut(r, c) = pixel
          }
        }
      }
    }
  }

  @virtualize
  def main() {
    val R = Rmax
    val C = Cmax
    convolveVideoStream()
  }
}

object LinebufRaster extends SpatialApp { // try arg = 100


  override val target = DE1
  val Cmax = 320
  val Rmax = 240



  @struct case class Pixel16(b: UInt5, g: UInt6, r: UInt5)

  @virtualize
  def convolveVideoStream(): Unit = {
    val dwell = ArgIn[Int]
    val d = args(0).to[Int]
    setArg(dwell, d)
    val imgOut = BufferedOut[Pixel16](target.VGA)
    val dumbdelay = 20 // extra delay so fill and drain take different number of cycles

    Accel (*) {
      val lb = LineBuffer[Pixel16](1,Cmax)
      Foreach(0 until Rmax) { i =>
        Foreach(0 until dwell) { _ =>
          Foreach(0 until Rmax){ r =>
            Foreach(0 until Cmax) { c =>
                val bluepixel = mux(abs(r - i) < 4, Pixel16(31.to[UInt5],0,0), Pixel16(0,0,0))
                val greenpixel = mux(abs(r - i) < 4, Pixel16(0,31.to[UInt6],0), Pixel16(0,0,0))
                val redpixel = mux(abs(r - i) < 4, Pixel16(0,0,31.to[UInt5]), Pixel16(0,0,0))
                val pixel = mux(c < Cmax/3, bluepixel, mux(c < Cmax*2/3, greenpixel, redpixel))
                lb.enq(pixel)
            }
            Foreach(0 until Cmax) { c =>
              imgOut(r, c) = lb(0, c)
            }

          }
        }
      }
    }
  }

  @virtualize
  def main() {
    val R = Rmax
    val C = Cmax
    convolveVideoStream()
  }
}


object ColorSelect extends SpatialApp { // try arg = 100
  override val target = DE1
  val Cmax = 320
  val Rmax = 240

  type UINT3 = FixPt[FALSE,_3,_0]
  type UINT4 = FixPt[FALSE,_4,_0]

  @struct class sw3(b1: UINT3, b2: UINT4, b3: UINT3)
  @struct case class Pixel16(b: UInt5, g: UInt6, r: UInt5)

  @virtualize
  def convolveVideoStream(): Unit = {
    val dwell = ArgIn[Int]
    val d = args(0).to[Int]
    setArg(dwell, d)
    val switch = target.SliderSwitch
    val swInput = StreamIn[sw3](switch)

    val imgOut = BufferedOut[Pixel16](target.VGA)

    Accel (*) {
      Foreach(0 until Rmax) { i =>
        Foreach(0 until dwell) { _ =>
          Foreach(0 until Rmax, 0 until Cmax){ (r, c) =>
            val swBits = swInput.value()
            val bluepixel = mux(r == i, Pixel16(31.to[UInt5],0,0), Pixel16(0,0,0))
            val greenpixel = mux(r == i, Pixel16(0,63.to[UInt6],0), Pixel16(0,0,0))
            val redpixel = mux(r == i, Pixel16(0,0,31.to[UInt5]), Pixel16(0,0,0))
            val pixel = mux(swBits.b1 == 0.to[UINT3], bluepixel, mux(swBits.b1 == 1.to[UINT3], greenpixel, redpixel))
            imgOut(r, c) = pixel
          }
        }
      }
    }
  }

  @virtualize
  def main() {
    val R = Rmax
    val C = Cmax
    convolveVideoStream()
  }
}
