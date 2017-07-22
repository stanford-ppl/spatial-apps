
import spatial.dsl.{Matrix => _, _ => _}
import org.virtualized._
import spatial.SpatialCompiler
import spatial.interpreter._
import spatial.metadata._
import collection.mutable.Map
import spatial.stdlib._

trait RaoBlackParticleFilter extends SpatialStream {

  val N: scala.Int                                                    = 1
  val initV: (scala.Double, scala.Double, scala.Double)               = (0.0, 0.0, 0.0)
  val initP: (scala.Double, scala.Double, scala.Double)               = (0.0, 0.0, 0.0)
  val initQ: (scala.Double, scala.Double, scala.Double, scala.Double) = (1.0, 0.0, 0.0, 0.0)
  val initCov = 0.00001

  val initTime: scala.Double  = 0.0
  val covGyro: scala.Double   = 0.01
  val covAcc: scala.Double    = 0.01
  val covViconP: scala.Double = 0.01
  val covViconQ: scala.Double = 0.01

  type SReal         = Double


  @struct case class SVec3(x: SReal, y: SReal, z: SReal)


  type STime         = SReal
  type SPosition     = SVec3
  type SVelocity     = SVec3
  type SAcceleration = SVec3
  type SOmega        = SVec3
  type SAttitude     = SQuat


  @struct case class SQuat(r: SReal, i: SReal, j: SReal, k: SReal)
  @struct case class SIMU(t: STime, a: SAcceleration, g: SOmega)
  @struct case class SPOSE(p: SVec3, q: SAttitude)
  @struct case class SVicon(t: STime, pose: SPOSE)
  //x(v1, v2, v3, p1, p2, p3)
  @struct case class Particle(w: SReal, q: SQuat, lastA: SAcceleration, lastQ: SQuat)


  val matrix = new Matrix[SReal] {
    def sqrtT(x: Double) = sqrt(x)
    val zero = 0.to[SReal]
    val one = 1.to[SReal]
  }
  import matrix._

  def toMatrix(v: SVec3): Matrix = {
    Matrix(3, 1, List(v.x, v.y, v.z))
  }

  def toVec(v: SVec3): Vec= {
    Vec(v.x, v.y, v.z)
  }

  implicit class SQuatOps(x: SQuat) {
    def *(y: SReal)        = SQuat(x.r * y, x.i * y, x.j * y, x.k * y)
    def *(y: SQuat)        = SQuatMult(x, y)
    def dot(y: SQuat)      = x.r * y.r + x.i * y.i + x.j * y.j + x.k * y.k
    def rotateBy(q: SQuat) = q * x
    def rotate(v: SVec3): SVec3 = {
      val inv = x.inverse
      val nq  = (x * SQuat(0.0, v.x, v.y, v.z)) * inv
      SVec3(nq.i, nq.j, nq.k)
    }
    def inverse = SQuatInverse(x)
  }

  def SQuatMult(q1: SQuat, q2: SQuat) = {
    SQuat(
      q1.r * q2.r - q1.i * q2.i - q1.j * q2.j - q1.k * q2.k,
      q1.r * q2.i + q1.i * q2.r + q1.j * q2.k - q1.k * q2.j,
      q1.r * q2.j - q1.i * q2.k + q1.j * q2.r + q1.k * q2.i,
      q1.r * q2.k + q1.i * q2.j - q1.j * q2.i + q1.k * q2.r
    )
  }

  def SQuatInverse(q: SQuat) = {
    val n = q.r * q.r + q.i * q.i + q.j * q.j + q.k * q.k
    SQuat(q.r, -q.i, -q.j, q.j) * (1 / n)
  }

  

  @virtualize def prog() = {

    val inSIMU     = StreamIn[SIMU](In1)
    val inV       = StreamIn[SVicon](In2)
    val out       = StreamOut[SVicon](Out1)
    val parFactor = N (1 -> N)

    Accel {

      val particles = SRAM[Particle](N)
      val states = SRAM[SReal](N, 6)
      val covs = SRAM[SReal](N, 6, 6)
      val fifoSIMU   = FIFO[SIMU](100)
      val fifoV     = FIFO[SVicon](100)

      val lastSTime = Reg[STime](initTime)
      val lastO    = Reg[SOmega](SVec3(0.0, 0.0, 0.0))

      Sequential {

        Foreach(0::N par parFactor)(x => {

          Pipe {
            Pipe { states(x, 0) = initV._1 }
            Pipe { states(x, 1) = initV._2 }
            Pipe { states(x, 2) = initV._3 }
            Pipe { states(x, 3) = initP._1 }
            Pipe { states(x, 4) = initP._2 }
            Pipe { states(x, 5) = initP._3 }
          }

          val initSQuat = SQuat(initQ._1, initQ._2, initQ._3, initQ._4)          
          
          Parallel {
            particles(x) = Particle(
              math.log(1.0 / N),
              initSQuat,
              SVec3(0.0, 0.0, 0.0),
              initSQuat
            )
            Foreach(0::6)(i =>
              covs(x, i, i) = initCov
            )
          }
        })

        Parallel {
          Stream(*)(x => {
            fifoV.enq(inV)
          })

          Stream(*)(x => {
            fifoSIMU.enq(inSIMU)
          })

          FSM[Boolean, Boolean](true)(x => x)(x => {
            if ((fifoV.empty && !fifoSIMU.empty) || (!fifoSIMU.empty && !fifoV.empty && fifoSIMU.peek.t < fifoV.peek.t)) {
              val imu = fifoSIMU.deq()
              updateFromSIMU(imu, lastSTime, lastO, particles, states, covs, parFactor)
            }
            else if (!fifoV.empty) {
              val v = fifoV.deq()
              updateFromV(v, lastSTime, lastO, particles, states, covs, parFactor)
            }

            out := SVicon(lastSTime, averageSPOSE(particles, states, parFactor))
            normWeights(particles, parFactor)
            resample(particles, parFactor)
          })(x => (!fifoV.empty || !fifoSIMU.empty))

        }
      }
    }
  }

  def rotationMatrix(q: SQuat) =
    Matrix(3, 3, List(
      1.0 - 2.0 * (q.j ** 2 + q.k ** 2),
      2.0 * (q.i * q.j - q.k * q.r),
      2.0 * (q.i * q.k + q.j * q.r),
      2.0 * (q.i * q.j + q.k * q.r),
      1.0 - 2.0 * (q.i ** 2 + q.k ** 2),
      2.0 * (q.j * q.k - q.i * q.r),
      2.0 * (q.i * q.k - q.j * q.r),
      2.0 * (q.j * q.k + q.i * q.r),
      1.0 - 2.0 * (q.i ** 2 + q.j ** 2)
    ))

  @virtualize
  def update(accO: Option[SAcceleration],
    vicon: Option[SPOSE],
    dt: STime,
    lastO: Reg[SOmega],
    particles: SRAM1[Particle],
    states: SRAM2[SReal],
    covs: SRAM3[SReal],
    parFactor: Int) = {

    Foreach(0::N par parFactor)(i => {
      val pp = particles(i)
      val nq =
        if (dt > 0.00001)
          sampleAtt(pp.q, lastO, dt)
        else
          pp.q
      val X: Option[SReal] = None
      val Sdt: Option[SReal] = Some(dt)
      val S1: Option[SReal] = Some(1)
      val F =
        Matrix.sparse(6, 6, IndexedSeq[Option[SReal]](
          S1, X, X, X, X, X,
          X, S1, X, X, X, X,
          X, X, S1, X, X, X,
          Sdt, X, X, S1, X, X,
          X, Sdt, X, X, S1, X,
          X, X, Sdt, X, X, S1
        ))

      val U = Matrix.sparse(6, 1, IndexedSeq[Option[SReal]](
        Some(pp.lastA.x * dt),
        Some(pp.lastA.y * dt),
        Some(pp.lastA.z * dt),
        X,
        X,
        X
      ))
      val rotMatrix = rotationMatrix(pp.lastQ)
      val covFixAcc = (rotMatrix * rotMatrix.t) * (covAcc * dt * dt)
      val Q = Matrix.sparse(6, 6, IndexedSeq[Option[SReal]](
        Some(covFixAcc(0, 0)), Some(covFixAcc(0, 1)), Some(covFixAcc(0, 2)), X, X, X,
        Some(covFixAcc(1, 0)), Some(covFixAcc(1, 1)), Some(covFixAcc(1, 2)), X, X, X,
        Some(covFixAcc(2, 0)), Some(covFixAcc(2, 1)), Some(covFixAcc(2, 2)), X, X, X,
        X, X, X, X, X, X,
        X, X, X, X, X, X,
        X, X, X, X, X, X
      ))

      val state = Matrix.fromSRAM1(6, states, i)
      val cov = Matrix.fromSRAM2(6, 6, covs, i)
      

      val nla        = accO.map(x => nq.rotate(x)).getOrElse(pp.lastA)
      val nlq        = accO.map(x => nq).getOrElse(pp.lastQ)

      val (nx, nsig) = kalmanPredict(state, cov, F, U, Q)

      if (vicon.isDefined) {

        val x = vicon.get

        val h = Matrix.sparse(3, 6,
          IndexedSeq[Option[SReal]](
            X, X, X, S1, X, X,
            X, X, X, X, S1, X,
            X, X, X, X, X, S1
          ))
        val r = Matrix.eye(3, covViconP)

        val (nx2, nsig2, lik) = kalmanUpdate(nx, nsig, toMatrix(x.p), h, r)
        val nw                = pp.w + likelihoodSPOSE(x, lik._1, nq, lik._2)

        nx2.loadTo(states, i)
        nsig2.loadTo(covs, i)        
        particles(i) = Particle(nw, nq, nla, nlq)

      }
      else {
        nx.loadTo(states, i)
        nsig.loadTo(covs, i)                        
        particles(i) = Particle(pp.w, nq, nla, nlq)
      }
    })
  }

  def updateFromSIMU(x: SIMU, lastSTime: Reg[STime], lastO: Reg[SOmega], particles: SRAM1[Particle], states: SRAM2[SReal], covs: SRAM3[SReal], parFactor: Int) = {
    val dt = x.t - lastSTime
    lastSTime := x.t
    lastO := x.g
    update(Some(x.a), None, dt, lastO, particles, states, covs, parFactor)
  }

  def updateFromV(x: SVicon, lastSTime: Reg[STime], lastO: Reg[SOmega], particles: SRAM1[Particle], states: SRAM2[SReal], covs: SRAM3[SReal],  parFactor: Int) = {
    val dt = x.t - lastSTime
    lastSTime := x.t
    update(None, Some(x.pose), dt, lastO, particles, states, covs, parFactor)
  }

  @virtualize def likelihoodSPOSE(measurement: SPOSE, expectedPosMeasure: Matrix, quatState: SQuat, covPos: Matrix) = {
    val wPos                = unnormalizedGaussianLogPdf(toMatrix(measurement.p), expectedPosMeasure, covPos)
    val covViconQMat = Matrix.eye(3, covViconQ)
    val error               = quatToLocalAngle(measurement.q.rotateBy(quatState.inverse))
    val wSQuat               = unnormalizedGaussianLogPdf(error, Matrix(3, 1, List[SReal](0, 0, 0)), covViconQMat)
    //    println("wPos: " + wPos)
    //    println("wSQuat: " + wSQuat)
    wPos + wSQuat
  }

  def sampleAtt(q: SQuat, om: SOmega, dt: STime): SQuat = {
    val withNoise  = gaussianVec(toVec(om), covGyro)
    val integrated = withNoise * dt
    val lq         = localAngleToQuat(integrated)
    lq.rotateBy(q)
  }

  def gaussianVec(mean: Vec, variance: SReal) = {
    val reg = RegFile[SReal](3)
    Foreach(0::2)(i => {
      val g1 = gaussian()
      Pipe {
        Pipe { reg(i*2) = g1._1 }
        Pipe { reg((i*2+1)%3) = g1._2 }
      }
    })
    Vec(3, RegId1(reg)) * sqrt(variance) + mean
  }

  //Box-Muller
  //http://www.design.caltech.edu/erik/Misc/Gaussian.html
  @virtualize def gaussian() = {

    val x1 = Reg[SReal]
    val x2 = Reg[SReal]
    val w  = Reg[SReal]
    val w2 = Reg[SReal]

    FSM[Boolean, Boolean](true)(x => x)(x => {
      x1 := 2.0 * random[SReal](1.0) - 1.0
      x2 := 2.0 * random[SReal](1.0) - 1.0
      w := (x1 * x1 + x2 * x2)
    })(x => w.value >= 1.0)

    w2 := sqrt((-2.0 * log(w.value)) / w)

    val y1 = x1 * w2;
    val y2 = x2 * w2;

    (y1, y2)
  }


  @virtualize def normWeights(particles: SRAM1[Particle], parFactor: Int) = {
    val totalWeight = Reg[SReal](0)
    val maxR = Reg[SReal]

    maxR := particles(0).w

    Reduce(maxR)(0::N)(i => particles(i).w)(max(_,_))
    Reduce(totalWeight)(0::N)(i => exp(particles(i).w - maxR))(_+_)
    totalWeight := maxR + log(totalWeight)
    Foreach(0::N par parFactor)(i => {
      val p = particles(i)
      particles(i) = Particle(p.w - totalWeight, p.q, p.lastA, p.lastQ)
    })
  }

  @virtualize def resample(particles: SRAM1[Particle], parFactor: Int) = {

    val weights = SRAM[SReal](N)
    val out = SRAM[Particle](N)

    val u = random[SReal](1.0)

    Foreach(0::N)(i => {
      if (i == 0)
        weights(i) = exp(particles(i).w)
      else
        weights(i) = weights(i-1) + exp(particles(i).w)
    })

    val k = Reg[Int](0)
    Foreach(0::N)(i => {
      val b = weights(k)*N < i.to[SReal] + u
      FSM[Boolean, Boolean](b)(x => x)(x => k := k + 1)(x => weights(k)*N < i.to[SReal]+u)

      out(i) = particles(k)
    })

    Foreach(0::N par parFactor)(i => {
      val p = out(i)
      particles(i) = Particle(log(1.0/N), p.q, p.lastA, p.lastQ)
    })

  }

  def unnormalizedGaussianLogPdf(measurement: Matrix, state: Matrix, cov: Matrix): SReal = {
    val e = (measurement-state)
    -1/2.0*((e.t*(cov.inv)*e)(0, 0))
  }

  def localAngleToQuat(v: Vec): SQuat = {
    val n    = v.norm
    val l    = n / 2
    val sl   = sin(l)
    val nrot = v * (sl / n)
    SQuat(cos(l), nrot(0), nrot(1), nrot(2))
  }

  def quatToLocalAngle(q: SQuat): Matrix = {
    val r: SReal = min(q.r, 1.0)
    val n = acos(r) * 2
    val s = n / sin(n / 2)
    Matrix(3, 1, List(q.i, q.j, q.k)) * s
  }

  def kalmanPredict(xp: Matrix, sigp: Matrix, f: Matrix, u: Matrix, q: Matrix) = {
    val xm   = f * xp + u
    val sigm = (f * sigp * f.t) + q
    (xm, sigm)
  }


  def kalmanUpdate(xm: Matrix, sigm: Matrix, z: Matrix, h: Matrix, r: Matrix) = {
    val s   = (h * sigm * h.t) + r
    val za = h * xm
    val k = sigm * h.t * s.inv
    val sig = sigm - (k * s * k.t)
    val x = xm + (k * (z - za))
    (x, sig, (za, s))
  }

  @virtualize def averageSPOSE(particles: SRAM1[Particle], states: SRAM2[SReal], parFactor: Int): SPOSE = {
    val firstQ = particles(0).q
    val accumP = RegFile[SReal](3, List[SReal](0, 0, 0))//(SVec3(0, 0, 0))
    val accumQ = Reg[SQuat](SQuat(0, 0, 0, 0))
    Parallel {
      Foreach(0::N par parFactor, 0::3)((i,j) => {
        accumP(j) = accumP(j) + states(i, j+3)
      })
      
      Reduce(accumQ)(0::N par parFactor)(i => {
        val p = particles(i)
        if (firstQ.dot(p.q) > 0.0)
          p.q * exp(p.w)
        else
          p.q * -(exp(p.w))
      })(_ + _)
    }
    SPOSE(SVec3(accumP(0), accumP(1), accumP(2)), accumQ)
  }

}

object RaoBlackParticleFilterInterpreter extends RaoBlackParticleFilter with SpatialStreamInterpreter {

  val outs = List(Out1)

  val inputs = collection.immutable.Map[Bus, List[MetaAny[_]]](
    (In1 -> List[SReal](3f, 4f, 2f, 6f).map(x => SIMU(x / 10, SVec3(x, x, x), SVec3(x / 100, x / 100, x / 100)))),
    (In2 -> List[SReal](3.5f, 5f).map(x => SVicon(x / 10, SPOSE(SVec3(x, x, x), SQuat(1, 0, 0, 0)))))
  )

}

object RaoBlackParticleFilterCompiler extends RaoBlackParticleFilter with SpatialStreamCompiler

