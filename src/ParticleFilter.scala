import spatial.dsl._
import org.virtualized._
import spatial.SpatialCompiler
import spatial.interpreter.Interpreter
import spatial.interpreter.Streams
import spatial.metadata._

trait ParticleFilter extends SpatialStream {

  val N: scala.Int                                                    = 10
  val initV: (scala.Double, scala.Double, scala.Double)               = (0.0, 0.0, 0.0)
  val initP: (scala.Double, scala.Double, scala.Double)               = (0.0, 0.0, 0.0)
  val initQ: (scala.Double, scala.Double, scala.Double, scala.Double) = (1.0, 0.0, 0.0, 0.0)
  val initTime: scala.Double                                          = 0.0
  val covGyro: scala.Double = 0.01

  type Real         = FltPt[_32, _0]
  type Time         = Real
  type Position     = Vec3
  type Velocity     = Vec3
  type Acceleration = Vec3
  type Omega        = Vec3
  type Attitude     = Quat


  @struct case class Quat(r: Real, i: Real, j: Real, k: Real)
  @struct case class Vec3(x: Real, y: Real, z: Real)
  @struct case class IMU(t: Time, a: Acceleration, g: Omega)
  @struct case class Vicon(t: Time, p: Position, q: Attitude)
  @struct case class State(v: Velocity, p: Position)
  @struct case class Particle(w: Real, q: Quat, st: State, lastA: Acceleration)
  @struct case class POSE(p: Position, q: Quat)

  case class Mat(h: scala.Int, w: scala.Int, x: Array[Real]) { self =>
    def *(y: Mat) = MatMult(self, y)
    def +(y: Mat) = MatAdd(self, y)
    def -(y: Mat) = MatSub(self, y)
    def t         = transpose(self)
  }

//  implicit class FIFOpeek[T](x: FIFO[T]) {
//    def peek: T = ???
//  }

  implicit class Vec3Ops(x: Vec3) {
    def *(y: Real) = Vec3(x.x*y, x.y*y, x.z*y)
    def +(y: Real) = Vec3(x.x+y, x.y+y, x.z+y)
    def +(y: Vec3) = Vec3(x.x+y.x, x.y+y.y, x.z+y.z)        
  }

  implicit class QuatOps(x: Quat) {
    def *(y: Real) = Quat(x.r*y, x.i*y, x.j*y, x.k*y)
    def *(y: Quat) = QuatMult(x, y)
    def rotateBy(q: Quat) = q*x
    def rotate(v: Vec3): Vec3 = {
      val inv = x.inverse
      val nq = (x*Quat(0.0, v.x, v.y, v.z))*inv
      Vec3(nq.i, nq.j, nq.k)
    }
    def inverse = QuatInverse(x)
  }


  @virtualize def prog() = {

    val inIMU = StreamIn[IMU](In1)
    val inV   = StreamIn[Vicon](In2)
    val out   = StreamOut[POSE](Out1)
    val parFactor = N (1 -> N)

    Accel {

      val particles = SRAM[Particle](N)
      val fifoIMU   = FIFO[IMU](100)
      val fifoV     = FIFO[Vicon](100)

      val lastTime = Reg[Time](initTime)
      val lastO    = Reg[Omega](Vec3(0.0, 0.0, 0.0))

      Sequential {

        Foreach(N by 1 par parFactor)(x => {
          particles(x) = Particle(1 / N,
                                  Quat(initQ._1, initQ._2, initQ._3, initQ._4),
                                  State(Vec3(initP._1, initP._2, initP._3), Vec3(initV._1, initV._2, initV._3)),
                                  Vec3(0.0, 0.0, 0.0))
        })

        Parallel {
          Stream(*)(x => {
            fifoV.enq(inV)
          })

          Stream(*)(x => {
            fifoIMU.enq(inIMU)
          })

          FSM.fsm[Boolean](true, (x => x), (x => {
            if ((fifoV.empty && !fifoIMU.empty) || (!fifoIMU.empty && !fifoV.empty && fifoIMU.peek.t < fifoV.peek.t))
              updateFromIMU(fifoIMU.deq(), lastTime, lastO, particles, parFactor)
            else if (!fifoV.empty)
              updateFromV(fifoV.deq(), lastTime, lastO, particles, parFactor)

            out := averagePOSE(particles)
          }) , (x => (!fifoV.empty || !fifoIMU.empty)), style=SeqPipe)
          
        }
      }
    }
  }

  @virtualize def updateAttitudeAndAcc(x: Acceleration, dt: Time, lastO: Reg[Omega], particles: SRAM1[Particle], parFactor: Int) = {
    Foreach(N by 1 par parFactor)(i => {
      val pp = particles(i)
      val nq = sampleAtt(pp.q, lastO, dt)
      particles(i) = Particle(pp.w, nq, pp.st, nq.rotate(x))
    })
  }


  def updateFromIMU(x: IMU, lastTime: Reg[Time], lastO: Reg[Omega], particles: SRAM1[Particle], parFactor: Int) = {
    val dt = x.t - lastTime
    lastTime := x.t
    lastO := x.g
    updateAttitudeAndAcc(x.a, dt, lastO, particles, parFactor)
  }

  def sampleAtt(q: Quat, om: Omega, dt: Time): Quat = {
    val withNoise  = gaussianVec(om, covGyro)
    val integrated = withNoise * dt
    val lq         = localAngleToQuat(integrated)
    lq.rotateBy(q)
  }
  

  def updateFromV(x: Vicon, lastTime: Reg[Time], lastO: Reg[Omega], particles: SRAM1[Particle], parFactor: Int) = {
    val dt = x.t - lastTime
    lastTime := x.t

  }

  def gaussianVec(mean: Vec3, variance: Real) = {
    val g1 = gaussian()
    val g2 = gaussian()
    (Vec3(g1._1, g1._2, g2._1)*sqrt(variance)) + mean
  }

  //Box-Muller
  //http://www.design.caltech.edu/erik/Misc/Gaussian.html
  @virtualize def gaussian() = {

    val x1 = Reg[Real]
    val x2 = Reg[Real]
    val w  = Reg[Real]
    val w2 = Reg[Real]

    FSM.fsm[Boolean]((true), (x => x), (x => {
      x1 := 2.0 * random[Real](1.0) - 1.0
      x2 := 2.0 * random[Real](1.0) - 1.0
      w := (x1 * x1 + x2 * x2)
    }), (x => w.value >= 1.0), style=SeqPipe)

    w2 := sqrt((-2.0 * log(w.value)) / w)

    val y1 = x1 * w2;
    val y2 = x2 * w2;

    (y1, y2)
  }

  def QuatMult(q1: Quat, q2: Quat) = {
    Quat(
      q1.r * q2.r - q1.i * q2.i - q1.j * q2.j - q1.k * q2.k,
      q1.r * q2.i + q1.i * q2.r + q1.j * q2.k - q1.k * q2.j,
      q1.r * q2.j - q1.i * q2.k + q1.j * q2.r + q1.k * q2.i,
      q1.r * q2.k + q1.i * q2.j - q1.j * q2.i + q1.k * q2.r
    )
  }

  def QuatInverse(q: Quat) = {
    val n = q.r * q.r + q.i * q.i + q.j * q.j + q.k * q.k
    Quat(q.r, -q.i, -q.j, q.j)*(1/n)
  }

  def norm(v: Vec3) =
    v.x * v.x + v.y * v.y + v.z * v.z

  def localAngleToQuat(v: Vec3): Quat = {
    val n    = norm(v)
    val l    = n / 2
    val sl   = sin(l)
    val nrot = v*(l/n)
    Quat(cos(l), nrot.x, nrot.y, nrot.z)
  }

  def MatMult(a: Mat, b: Mat): Mat =
    ???

  def MatAdd(a: Mat, b: Mat): Mat =
    ???

  def MatSub(a: Mat, b: Mat): Mat =
    ???

  def kalmanPredict(xp: Mat, sp: Mat, f: Mat, u: Mat, q: Mat) = {
    val xm   = f * xp + u
    val sigm = f * sp * f.t + q
    (xm, sigm)
  }

  def inv(x: Mat): Mat =
    ???

  def transpose(x: Mat): Mat =
    ???

  def kalmanUpdate(xm: Mat, sigm: Mat, z: Mat, h: Mat, r: Mat) = {
    val s   = h * sigm * h.t + r
    val za  = h * xm
    val k   = sigm * h.t * inv(s)
    val sig = sigm - k * s * k.t
    val x   = xm + k * (z - za)
    (x, sig, (za, s))
  }

  def averagePOSE(x: SRAM1[Particle]): POSE = {
    //TODO FIX THAT, VERY INCORRECT
    val p = x(0)
    POSE(p.st.p, p.q)
  }

  val outs = List(Out1)

  val inputs = Map[Bus, List[MetaAny[_]]](
    (In1 -> List[Real](3f, 4f, 2f, 6f).map(x => IMU(x/10, Vec3(x, x, x), Vec3(x/100, x/100, x/100)))),
    (In2 -> List[Real]().map(x => Quat(x, x, x, x)))  
  )

}

object ParticleFilterInterpreter extends ParticleFilter with SpatialStreamInterpreter

object ParticleFilterCompiler extends ParticleFilter with SpatialApp {
  def main() =
    prog()
}
