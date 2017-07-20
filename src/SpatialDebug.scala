import org.virtualized._
import spatial.dsl._

object SpatialDebug extends SpatialApp {

  val N: scala.Int = 10

  @virtualize def main() {

    @struct case class S(x: Double)

    Accel {

      val sram = SRAM[S](N)

      Foreach(N by 1){i =>
        sram(i) = S(i.to[Double])
      }

      val x = Reg[Double](sram(0).x)

      Reduce(x)(N by 1)(i => sram(i).x)(_+_)

      println(x)

    }
  }


}

