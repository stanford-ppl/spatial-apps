
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

  val N: scala.Int                                  = 10
  val initV: (SCReal, SCReal, SCReal)               = (0.0, 0.0, 0.0)
  val initP: (SCReal, SCReal, SCReal)               = (0.0, 0.0, 0.0)
  val initQ: (SCReal, SCReal, SCReal, SCReal) = (1.0, 0.0, 0.0, 0.0)
  val initCov = 0.00001

  val initTime: SCReal  = 0.0
  val covGyro: SCReal   = 1.0
  val covAcc: SCReal    = 0.1
  val covViconP: SCReal = 0.01
  val covViconQ: SCReal = 0.01

  @struct case class SVec3(x: SReal, y: SReal, z: SReal)


  type STime         = SReal//Double
  type SPosition     = SVec3
  type SVelocity     = SVec3
  type SAcceleration = SVec3
  type SOmega        = SVec3
  type SAttitude     = SQuat


  @struct case class SQuat(r: SReal, i: SReal, j: SReal, k: SReal)
  @struct case class SIMU(a: SAcceleration, g: SOmega)  
  @struct case class TSA(t: STime, v: SIMU)
  @struct case class SPOSE(p: SVec3, q: SAttitude)
  @struct case class TSB(t: STime, v: SPOSE)
  @struct case class TSR(t: STime, pose: SPOSE)  
  @struct case class Particle(w: SReal, q: SQuat, lastA: SAcceleration, lastQ: SQuat)
  

  val lutP: scala.Int = 10000
  val lutAcos: scala.Int = 1000

  lazy val acosLUT = 
    LUT[SReal](lutAcos)(List.tabulate[SReal](lutAcos)(i => math.acos(i/lutAcos.toDouble)):_*)

  lazy val sqrtLUT = 
    LUT[SReal](lutP)(List.tabulate[SReal](lutP)(i => math.sqrt(((i/lutP.toDouble)*5))):_*)
    
  lazy val logLUTSmall = 
    LUT[SReal](lutP)(((-9:SReal)::List.tabulate[SReal](lutP-1)(i => math.log(((i+1)/lutP.toDouble)*1))):_*)

  lazy val logLUTBig = 
    LUT[SReal](lutP)(((-9:SReal)::List.tabulate[SReal](lutP-1)(i => math.log(((i+1)/lutP.toDouble)*200))):_*)
  
  lazy val expLUT = 
    LUT[SReal](lutP)(List.tabulate[SReal](lutP)(i => math.exp(i/lutP.toDouble*20-10)):_*)

  
  def sin(x: SReal) = sin_taylor(x)
  def cos(x: SReal) = cos_taylor(x)  

  @virtualize
  def log(x: SReal) = {
    if (x < 1.0)
      logLUTSmall(((x/1.0)*(lutP.toDouble)).to[Index])
    else
      logLUTBig(((x/200.0)*(lutP.toDouble)).to[Index])      
  }

  @virtualize
  def sqrt(x: SReal) = {
    if (x < 5)
      sqrtLUT(((x/5.0)*(lutP.toDouble)).to[Index])
    else
      sqrt_approx(x)
  }

  @virtualize
  def exp(x: SReal): SReal =  {
    if (x <= -10)
      4.5399929762484854E-5
    else
      expLUT((((x+10)/20.0)*(lutP.toDouble)).to[Index])      

  }
  
  
  @virtualize
  def acos(x: SReal) = {
    val ind = (x*(lutP.toDouble)).to[Index]
    if (ind <= 0)
      0
    else if (ind >= lutAcos)
      PI
    else {
      val r = acosLUT(ind)
      if (x >= 0)
        r
      else
        PI - r
    }
  }

  val matrix = new Matrix[SReal] {
    val IN_PLACE = false
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
    logLUTSmall
    logLUTBig
    sqrtLUT
    expLUT

    Sequential.Foreach(0::N par parFactor)(x => {
      
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
            covs(x, i, j) = 0
        )
      }
    })
  }
  

  @virtualize def spatial() = {

//    val inSIMU     = StreamIn[TSA](In1)
//    val inV       = StreamIn[TSB](In2)
//    val out       = StreamOut[TSR](Out1)
    val out        = DRAM[TSR](11)
    val outI  = Reg[Index](0)

    
    val parFactor = 1 (1 -> N)

    Accel {

      val sramBUFFER = SRAM[TSR](10)

      val particles = SRAM[Particle](N)
      val states = SRAM[SReal](N, 6)
      val covs = SRAM[SReal](N, 6, 6)
      val fifoSIMU   = FIFO[TSA](100)
      val fifoV     = FIFO[TSB](100)

      val lastSTime = Reg[STime](initTime)
      val lastO    = Reg[SOmega](SVec3(0.0, 0.0, 0.0))

      Sequential {

        initParticles(particles, states, covs, parFactor)
        val tsas = List(
          TSA(0.0, SIMU(SVec3(0.25592961314469886, 0.07098284446822825, 0.21456271801876464), SVec3(-1.5064973528139753, -0.616587623133762, -1.2282747703297874))),
          TSA(0.005, SIMU(SVec3(0.25592961314469886, 0.07098284446822825, 0.21456271801876464), SVec3(-1.5064973528139753, -0.616587623133762, -1.2282747703297874))),
          TSA(0.01, SIMU(SVec3(-0.028161957507305768, 0.5264671724413507, -0.08577696872942979), SVec3(-8.614952996266531, -3.699394514006443, -1.2972430998591449))),
          TSA(0.015, SIMU(SVec3(0.5872729169473, 1.3887486685548156, 0.05238849400043921),SVec3(-7.587044288799566, -4.571689569385891, 1.3110956660901323))),
          TSA(0.02, SIMU(SVec3(-1.2583237251856354, 1.4047261203145096, 1.066904859524985),SVec3(-6.119550697150657, -1.7511569969392748, -0.7289420270309247)))
        )
        val tsbs = List(
          TSB(0.025, SPOSE(SVec3(0.0747780945262634, 0.10808064129411422, -0.10529076833659502), SQuat(0.9962501245604087, -0.04308471266603672, -0.01947353436952111, -0.07245811415579231)))
        )


        tsas.foreach(x => Pipe {fifoSIMU.enq(x) } )
        tsbs.foreach(x => Pipe {fifoV.enq(x) } )         
        
        Parallel {

          /*
          Stream(*)(x => {
            fifoV.enq(inV)
          })

          Stream(*)(x => {
            fifoSIMU.enq(inSIMU)
          })
           */


          val choice = Reg[Int]
          val dt = Reg[SReal]          
          FSM[Boolean, Boolean](true)(x => x)(x => {
            Sequential {
              if ((fifoV.empty && !fifoSIMU.empty) || (!fifoSIMU.empty && !fifoV.empty && fifoSIMU.peek.t < fifoV.peek.t)) {
                choice := 0
                val imu = fifoSIMU.peek
                val t = imu.t
                lastO := imu.v.g
                dt := (t - lastSTime).to[SReal]
                lastSTime := t
              }
              else if (!fifoV.empty) {
                choice := 1
                val t = fifoV.peek.t
                dt := (t - lastSTime).to[SReal]                
                lastSTime := t
              }
              else
                choice := -1

              if (choice.value != -1) {
                updateAtt(dt, lastO, particles, parFactor)
              }
              if (choice.value == 0) {
                val imu = fifoSIMU.deq()
                imuUpdate(imu.v.a, particles, parFactor)
              }
              if (choice.value != -1) {
                kalmanPredictParticle(dt, particles, states, covs, parFactor)
              }
              if (choice.value == 1) {
                val v = fifoV.deq()
                viconUpdate(v.v, dt, particles, states, covs, parFactor)
              }
              if (choice.value != -1) {
                normWeights(particles, parFactor)

                sramBUFFER(outI) = TSR(lastSTime, averageSPOSE(particles, states, parFactor))

                outI := outI + 1

              //  out := TSR(lastSTime, averageSPOSE(particles, states, parFactor))
                resample(particles, states, covs, parFactor)
              }
            }
          })(x => (!fifoV.empty || !fifoSIMU.empty))            
//          })(x => true)

        }

        out(0::10) store sramBUFFER
      }
    }

    getMem(out).foreach(x => println(x))
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
    Foreach(0::N par parFactor)(i => {
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
    dt: SReal,
    particles: SRAM1[Particle],
    states: SRAM2[SReal],
    covs: SRAM3[SReal],
    parFactor: Int
  ) = {
    Foreach(0::N par parFactor)(i => {


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

      val pp = particles(i)

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
    Foreach(0::N par parFactor)(i => {
      val pp = particles(i)
      val na = pp.q.rotate(acc)
      particles(i) = Particle(pp.w, pp.q, na, pp.q)
    })
  }

  @virtualize
  def viconUpdate(
    vicon: SPOSE,
    dt: SReal,
    particles: SRAM1[Particle],
    states: SRAM2[SReal],
    covs: SRAM3[SReal],
    parFactor: Int) = {

    val X: Option[SReal] = None
    val S1: Option[SReal] = Some(1)
   
    val h = Matrix.sparse(3, 6,
      IndexedSeq[Option[SReal]](
        X, X, X, S1, X, X,
        X, X, X, X, S1, X,
        X, X, X, X, X, S1
      ))
    
    val r = Matrix.eye(3, covViconP)

    val viconP = toMatrix(vicon.p)

    covViconQMat
    zeroVec

    Foreach(0::N par parFactor)(i => {
     

      val state = Matrix.fromSRAM1(6, states, i, true)
      val cov = Matrix.fromSRAM2(6, 6, covs, i, true)    
      
      val (nx2, nsig2, lik) = kalmanUpdate(state, cov, viconP, h, r)
      nx2.loadTo(states, i)
      nsig2.loadTo(covs, i)

      val pp = particles(i)
      val nw                = likelihoodSPOSE(vicon, lik._1, pp.q, lik._2)
      particles(i) = Particle(pp.w + nw, pp.q, pp.lastA, pp.lastQ)
    })
  }


  lazy val covViconQMat = Matrix.eye(3, covViconQ)
  lazy val zeroVec = Matrix(3, 1, List[SReal](0, 0, 0))
  @virtualize def likelihoodSPOSE(measurement: SPOSE, expectedPosMeasure: Matrix, quatState: SQuat, covPos: Matrix) = {
    val wPos                = unnormalizedGaussianLogPdf(toMatrix(measurement.p), expectedPosMeasure, covPos)
    val error               = quatToLocalAngle(measurement.q.rotateBy(quatState.inverse))
    val wSQuat               = unnormalizedGaussianLogPdf(error, zeroVec, covViconQMat)
    //    println("wPos: " + wPos)
    //    println("wSQuat: " + wSQuat)
    wPos + wSQuat
  }


  def sampleAtt(q: SQuat, om: SOmega, dt: SReal): SQuat = {
    val withNoise  = gaussianVec(toVec(om), covGyro)
    val integrated = withNoise * dt
    val lq         = localAngleToQuat(integrated)
    lq.rotateBy(q)
  }

  @virtualize def gaussianVec(mean: Vec, variance: SReal) = {
    val reg = RegFile[SReal](3)
    //Real sequential
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
    val maxR = Reduce(Reg[SReal])(0::N)(i => particles(i).w)(max(_,_))
    val totalWeight = Reduce(Reg[SReal])(0::N)(i => exp(particles(i).w - maxR))(_+_)
    val norm = maxR + log(totalWeight)
    Foreach(0::N par parFactor)(i => {
      val p = particles(i)
      particles(i) = Particle(p.w - norm, p.q, p.lastA, p.lastQ)
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
      def notDone = (weights(k) * N < i.to[SReal] + u) && k < N
      FSM[Boolean, Boolean](notDone)(x => x)(x => k := k + 1)(x => notDone)

      Foreach(0::6)(x => {
        outStates(i, x) = states(k, x)
      })
      Foreach(0::6, 0::6)( (y, x) => {
        outCovs(i, y, x) = covs(k, y, x)
      })
      
      out(i) = particles(k)
    })


    Foreach(0::N)(i => {
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
    val n    = (v*256).norm/256
    val l    = n / 2.0
    val sl   = sin(l)
    println(v(0) + " " + v(1) + " " + v(2) + "" + n + " " + sl)
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
    val k = sigm * h.t * s.inv
    val sig = sigm :- (k * s * k.t)
    val za = h * xm    
    val x = xm :+ (k * (z :- za))
    (x, sig, (za, s))
  }

  @virtualize def averageSPOSE(particles: SRAM1[Particle], states: SRAM2[SReal], parFactor: Int): SPOSE = {
    val firstQ = particles(0).q
    val accumP = RegFile[SReal](3, List[SReal](0, 0, 0))
    val accumQ = Reg[SQuat](SQuat(1, 0, 0, 0))
    accumP.reset
    accumQ.reset
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
    (In1 -> List[TSA](
      TSA(0.0, SIMU(SVec3(0.25592961314469886, 0.07098284446822825, 0.21456271801876464), SVec3(-1.5064973528139753, -0.616587623133762, -1.2282747703297874))),
      TSA(0.005, SIMU(SVec3(0.25592961314469886, 0.07098284446822825, 0.21456271801876464), SVec3(-1.5064973528139753, -0.616587623133762, -1.2282747703297874))),
      TSA(0.01, SIMU(SVec3(-0.028161957507305768, 0.5264671724413507, -0.08577696872942979), SVec3(-8.614952996266531, -3.699394514006443, -1.2972430998591449))),
      TSA(0.015, SIMU(SVec3(0.5872729169473, 1.3887486685548156, 0.05238849400043921),SVec3(-7.587044288799566, -4.571689569385891, 1.3110956660901323))),
      TSA(0.02, SIMU(SVec3(-1.2583237251856354, 1.4047261203145096, 1.066904859524985),SVec3(-6.119550697150657, -1.7511569969392748, -0.7289420270309247)))
    )),
    (In2 -> List[TSB](TSB(0.025, SPOSE(SVec3(0.0747780945262634, 0.10808064129411422, -0.10529076833659502), SQuat(0.9962501245604087, -0.04308471266603672, -0.01947353436952111, -0.07245811415579231))))
  ))

}

object RaoBlackParticleFilterCompiler extends RaoBlackParticleFilter with SpatialStreamCompiler

object RaoBlackParticleFilterDRAM extends RaoBlackParticleFilter with SpatialStreamCompiler

