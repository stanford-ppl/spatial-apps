
import spatial.dsl.{Matrix => _, _ => _}
import org.virtualized._
import spatial.SpatialCompiler
import spatial.interpreter._
import spatial.metadata._
import collection.mutable.Map
import spatial.stdlib._

trait RaoBlackParticleFilter extends SpatialStream {

  type SCReal = scala.Double
  type SReal  = FixPt[TRUE, _16, _16]

  implicit def toReal(x: SCReal) = x.to[SReal]

  def sqrt(x: SReal) = sqrt_approx(x)
  def sin(x: SReal) = sin_taylor(x)
  def cos(x: SReal) = cos_taylor(x)  

  val lutP: scala.Int = 10000
  val lutAcos: scala.Int = 1000 
  lazy val logLUT = 
    LUT[SReal](lutP)(((-9:SReal)::List.tabulate[SReal](lutP-1)(i => math.log(((i+1)/lutP.toDouble)*20))):_*)

  lazy val expLUT = 
    LUT[SReal](lutP)(List.tabulate[SReal](lutP)(i => math.exp(i/lutP.toDouble*10-10)):_*)

  lazy val acosLUT = 
    LUT[SReal](lutAcos)(List.tabulate[SReal](lutAcos)(i => math.acos(i/lutAcos.toDouble)):_*)
  
  @virtualize
  def acos(x: SReal) = {
    val r = acosLUT((x*(lutP.toDouble)).to[Index])
    if (x >= 0)
      r
    else
      PI - r
  }

  @virtualize
  def log(x: SReal) = 
    logLUT(((x/20.0)*(lutP.toDouble)).to[Index])
  
  @virtualize
  def exp(x: SReal): SReal = {
    if (x >= 0.0)
      1
    else
      expLUT((((x+10)/10.0)*(lutP.toDouble)).to[Index])
  }
  
  val N: scala.Int                                                    = 1
  val initV: (SCReal, SCReal, SCReal)               = (0.0, 0.0, 0.0)
  val initP: (SCReal, SCReal, SCReal)               = (0.0, 0.0, 0.0)
  val initQ: (SCReal, SCReal, SCReal, SCReal) = (1.0, 0.0, 0.0, 0.0)
  val initCov = 0.00001


  val initTime: SCReal  = 0.0
  val covGyro: SCReal   = 0.01
  val covAcc: SCReal    = 0.01
  val covViconP: SCReal = 0.01
  val covViconQ: SCReal = 0.01

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
    def sqrtT(x: SReal) = sqrt(x)
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

  @virtualize def initParticles(particles: SRAM1[Particle], states: SRAM2[SReal], covs: SRAM3[SReal], parFactor: Int) = {

    acosLUT
    logLUT
    expLUT

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
      
      Sequential {
        particles(x) = Particle(
          math.log(1.0 / N),
          initSQuat,
          SVec3(0.0, 0.0, 0.0),
          initSQuat
        )
        Foreach(0::6, 0::6)((i,j) =>
          if (i == j)
            covs(x, i, i) = initCov
          else 
            covs(x, i, i) = 0
        )
      }
    })
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

        initParticles(particles, states, covs, parFactor)
        Parallel {

          Stream(*)(x => {
            fifoV.enq(inV)
          })

          Stream(*)(x => {
            fifoSIMU.enq(inSIMU)
          })

          val choice = Reg[Int]
          val dt = Reg[SReal]          
          FSM[Boolean, Boolean](true)(x => x)(x => {
            
            if ((fifoV.empty && !fifoSIMU.empty) || (!fifoSIMU.empty && !fifoV.empty && fifoSIMU.peek.t < fifoV.peek.t)) {
              choice := 0
              val imu = fifoSIMU.peek
              val t = imu.t
              lastO := imu.g
              dt := t - lastSTime
              lastSTime := t              
            }
            else if (!fifoV.empty) {
              choice := 1
              val t = fifoV.peek.t
              dt := t - lastSTime
              lastSTime := t
            }
            else
                choice := -1

            if (choice.value != -1) {
              updateAtt(dt, lastO, particles, parFactor)
            }
            if (choice.value == 0) {
              val imu = fifoSIMU.deq()
              imuUpdate(imu.a, particles, parFactor)
            }
            if (choice.value != -1) {
              kalmanPredictParticle(dt, particles, states, covs, parFactor)
            }
            if (choice.value == 1) {
              val v = fifoV.deq()
              viconUpdate(v.pose, dt, particles, states, covs, parFactor)
            }
            if (choice.value != -1) {
              normWeights(particles, parFactor)
              out := SVicon(lastSTime, averageSPOSE(particles, states, parFactor))
              resample(particles, states, covs, parFactor)
            }
          })(x => (!fifoV.empty || !fifoSIMU.empty))

        }
      }
    }
  }

  def rotationMatrix(q: SQuat) =
    Matrix(3, 3, List(
      1.0 - 2.0 * (q.j ** 2 + q.k ** 2), 2.0 * (q.i * q.j - q.k * q.r), 2.0 * (q.i * q.k + q.j * q.r),
      2.0 * (q.i * q.j + q.k * q.r), 1.0 - 2.0 * (q.i ** 2 + q.k ** 2), 2.0 * (q.j * q.k - q.i * q.r),
      2.0 * (q.i * q.k - q.j * q.r), 2.0 * (q.j * q.k + q.i * q.r), 1.0 - 2.0 * (q.i ** 2 + q.j ** 2)
    ))

  @virtualize
  def updateAtt(
    dt: SReal,
    lastO: SOmega,
    particles: SRAM1[Particle],
    parFactor: Int
  ) = {
    Sequential.Foreach(0::N par parFactor)(i => {
      val pp = particles(i)
      val nq = 
        if (dt > 0.00001)
          sampleAtt(pp.q, lastO, dt)
        else
          pp.q
      particles(i) = Particle(pp.w, nq, pp.lastA, pp.lastQ)
    })
  }

  @virtualize
  def kalmanPredictParticle(
    dt: STime,
    particles: SRAM1[Particle],
    states: SRAM2[SReal],
    covs: SRAM3[SReal],
    parFactor: Int
  ) = {
    Sequential.Foreach(0::N par parFactor)(i => {

      val pp = particles(i)

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

      val (nx, nsig) = kalmanPredict(state, cov, F, U, Q)
      nx.loadTo(states, i)
      nsig.loadTo(covs, i)      


    })
  }

  @virtualize
  def imuUpdate(acc: SAcceleration, particles: SRAM1[Particle], parFactor: Int) = {
    Sequential.Foreach(0::N par parFactor)(i => {
      val pp = particles(i)
      val na = pp.q.rotate(acc)
      particles(i) = Particle(pp.w, pp.q, na, pp.q)
    })
  }

  @virtualize
  def viconUpdate(
    vicon: SPOSE,
    dt: STime,
    particles: SRAM1[Particle],
    states: SRAM2[SReal],
    covs: SRAM3[SReal],
    parFactor: Int) = {

    Sequential.Foreach(0::N par parFactor)(i => {
      
      val pp = particles(i)

      val X: Option[SReal] = None
      val S1: Option[SReal] = Some(1)

      val h = Matrix.sparse(3, 6,
        IndexedSeq[Option[SReal]](
          X, X, X, S1, X, X,
          X, X, X, X, S1, X,
          X, X, X, X, X, S1
        ))

      val r = Matrix.eye(3, covViconP)

      val state = Matrix.fromSRAM1(6, states, i)
      val cov = Matrix.fromSRAM2(6, 6, covs, i)    
      
      val (nx2, nsig2, lik) = kalmanUpdate(state, cov, toMatrix(vicon.p), h, r)
      val nw                = pp.w + likelihoodSPOSE(vicon, lik._1, pp.q, lik._2)

      nx2.loadTo(states, i)
      nsig2.loadTo(covs, i)
      particles(i) = Particle(nw, pp.q, pp.lastA, pp.lastQ)
    })
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

  @virtualize def gaussianVec(mean: Vec, variance: SReal) = {
    val reg = RegFile[SReal](3)
    Sequential.Foreach(0::2)(i => {
      val g1 = gaussian()
        reg(i*2) = g1._1 
        if (i != 1)
          reg((i*2+1)) = g1._2
    })
    (Vec(3, RegId1(reg)) :* sqrt(variance)) :+ mean
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

  @virtualize def resample(particles: SRAM1[Particle], states: SRAM2[SReal], covs: SRAM3[SReal], parFactor: Int) = {

    val weights = SRAM[SReal](N)
    val out = SRAM[Particle](N)
    val outStates = SRAM[SReal](N, 6)
    val outCovs = SRAM[SReal](N, 6, 6)    

    val u = random[SReal](1.0)

    Foreach(0::N)(i => {
      if (i == 0)
        weights(i) = exp(particles(i).w)
      else
        weights(i) = weights(i-1) + exp(particles(i).w)
    })

    val k = Reg[Int](0)
    Sequential.Foreach(0::N)(i => {
      val b = weights(k)*N < i.to[SReal] + u

      FSM[Boolean, Boolean](b)(x => x)(x => k := k + 1)(x => weights(k)*N < i.to[SReal]+u)

      Foreach(0::6)(x => {
        outStates(i, x) = states(k, x)
      })
      Foreach(0::6, 0::6)( (y, x) => {
        outCovs(i, y, x) = covs(k, y, x)
      })
      
      out(i) = particles(k)
    })


    Sequential.Foreach(0::N)(i => {
      val p = out(i)
      Foreach(0::6)(x => {
        states(i, x) = outStates(i, x)
      })
      Foreach(0::6, 0::6)( (y, x) => {
        covs(i, y, x) = outCovs(i, y, x)
      })
      particles(i) = Particle(log(1.0/N), p.q, p.lastA, p.lastQ)
    })

  }

  def unnormalizedGaussianLogPdf(measurement: Matrix, state: Matrix, cov: Matrix): SReal = {
    val e = (measurement :- state)
    -1/2.0*((e.t*(cov.inv)*e).apply(0, 0))
  }

  def localAngleToQuat(v: Vec): SQuat = {
    val n    = v.norm
    val l    = n / 2
    val sl   = sin(l)
    val nrot = v :* (sl / n)
    SQuat(cos(l), nrot(0), nrot(1), nrot(2))
  }

  def quatToLocalAngle(q: SQuat): Matrix = {
    val r: SReal = min(q.r, 1.0)
    val n = acos(r) * 2
    val s = n / sin(n / 2)
    Matrix(3, 1, List(q.i, q.j, q.k)) :* s
  }

  def kalmanPredict(xp: Matrix, sigp: Matrix, f: Matrix, u: Matrix, q: Matrix) = {
    val xm   = f * xp :+ u
    val sigm = (f * sigp * f.t) :+ q
    (xm, sigm)
  }


  def kalmanUpdate(xm: Matrix, sigm: Matrix, z: Matrix, h: Matrix, r: Matrix) = {
    val s   = (h * sigm * h.t) :+ r
    val za = h * xm
    val k = sigm * h.t * s.inv
    val sig = sigm :- (k * s * k.t)
    val x = xm :+ (k * (z :- za))
    (x, sig, (za, s))
  }

  @virtualize def averageSPOSE(particles: SRAM1[Particle], states: SRAM2[SReal], parFactor: Int): SPOSE = {
    val firstQ = particles(0).q
    val accumP = RegFile[SReal](3, List[SReal](0, 0, 0))
    val accumQ = Reg[SQuat](SQuat(0, 0, 0, 0))
    Parallel {
      Foreach(0::N par parFactor, 0::3)((i,j) => {
        accumP(j) = accumP(j) + exp(particles(i).w) * states(i, j+3)
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

