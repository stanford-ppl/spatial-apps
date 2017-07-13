import spatial.dsl._
import org.virtualized._
import spatial.SpatialCompiler
import spatial.interpreter.Interpreter
import spatial.interpreter.Streams

trait ParticleFilter extends SpatialStream {

  type SReal = FltPt[_32,_0]

  @struct case class SQuaternion(r: SReal, i: SReal, j: SReal, k: SReal)
  @struct case class SVec3(x: SReal, y: SReal, z: SReal)
  @struct case class SState(v: SVec3, p: SVec3)
  @struct case class SParticle(w: SReal, q: SQuaternion, st: SState)  


  val N: scala.Int = 10

  @virtualize def prog() = {

    val inIMU  = StreamIn[SReal](In2)        
    val inV  = StreamIn[SQuaternion](In1)
    val out = StreamOut[SQuaternion](Out1)
    val p = 100 (1 -> 100)

    Accel {
      val state = SRAM[SQuaternion](N)
      val fifoV = FIFO[SQuaternion](100)
      val fifoIMU = FIFO[SReal](100)

      val lastV = Reg[SQuaternion]
      val lastIMU = Reg[SReal]


      Foreach(N by 1)(x => {
        val g1 = gaussian(0.0, 1.0)
        val g2 = gaussian(0.0, 1.0)        
        state(x) = SQuaternion(g1._1, g1._2, g2._1, g2._2)
      })
      
      Stream(*)(x => {
        fifoV.enq(inV)
      })

      Stream(*)(x => {
        fifoIMU.enq(inIMU)
      })
      
      FSM[Int]{x => x < 10}{x => {
        out := state(x)
        if (fifoV.empty && fifoIMU.empty)
          ()
        else if (fifoV.empty) {
          println(fifoIMU.deq())
        } else {
          println(fifoV.deq())
          ()
        }
      }}{x => x + 1}

    }
  }

  //Box-Muller
  //http://www.design.caltech.edu/erik/Misc/Gaussian.html
  @virtualize def gaussian(mean: SReal, variance: SReal) = {

    val x1 = Reg[SReal]
    val x2 = Reg[SReal]
    val w = Reg[SReal]
    do {
      x1 := 2.0 * random[SReal](1.0) - 1.0
      x2 := 2.0 * random[SReal](1.0) - 1.0
      w := (x1 * x1 + x2 * x2)
      false
    } while (w.value >= 1.0)

    w := sqrt( (-2.0 * log(w.value) ) / w )
    val y1 = x1 * w
    val y2 = x2 * w

    (y1, y2)
    
  }

  def quatMult(q1: SQuaternion, q2: SQuaternion) = {
    SQuaternion(
      q1.r*q2.r - q1.i*q2.i - q1.j*q2.j - q1.k*q2.k,
      q1.r*q2.i + q1.i*q2.r + q1.j*q2.k - q1.k*q2.j,
      q1.r*q2.j - q1.i*q2.k + q1.j*q2.r + q1.k*q2.i,
      q1.r*q2.k + q1.i*q2.j - q1.j*q2.i + q1.k*q2.r
    )
  }

  def quatInverse(q: SQuaternion) = {
    val n = q.r*q.r + q.i*q.i + q.j*q.j + q.k*q.k
    SQuaternion(q.r/n, -q.i/n, -q.j/n, q.j/n)
  }

  def localAngleToQuat(v: SVec3): SQuaternion =
    ???

  val outs = List(Out1)


  val inputs = Map[Bus, List[MetaAny[_]]](
    (In1 -> List[SReal](3f, 4f, 2f, 6f).map(x => SQuaternion(x, x, x, x))),
    (In2 -> List[SReal](3f, 4f, 2f, 6f))
  )
  
  

}

object ParticleFilterInterpreter extends ParticleFilter with SpatialStreamInterpreter 

object ParticleFilterCompiler extends ParticleFilter with SpatialApp {
  def main() =
    prog()
}



