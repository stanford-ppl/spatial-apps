import spatial.dsl.{Matrix => _, _ => _}
import org.virtualized._
import spatial.SpatialCompiler
import spatial.interpreter._
import spatial.metadata._
import collection.mutable.Map
import spatial.stdlib._

trait MiniParticleFilter extends SpatialStream {

  type CReal = scala.Double
  type SReal  = FixPt[TRUE, _16, _16]

  implicit def toSReal(x: CReal) = x.to[SReal]

  type STime         = SReal
  type SPosition2D     = SVec2
  type SAcceleration2D = SVec2
  type SWeight       = SReal

  @struct case class SVec2(x: SReal, y: SReal)
  @struct case class TSA(t: Double, v: SAcceleration2D)
  @struct case class TSB(t: Double, v: SPosition2D)
  @struct case class TSR(t: Double, v: SPosition2D)    

  val N: scala.Int = 10
  val covAcc: scala.Double = 0.01
  val covGPS: scala.Double = 0.01
  val dt: scala.Double = 0.1

  val lutP: scala.Int = 10000

  lazy val sqrtLUT = 
    LUT[SReal](lutP)(List.tabulate[SReal](lutP)(i => math.sqrt(((i/lutP.toDouble)*5))):_*)
    
  lazy val logLUTSmall = 
    LUT[SReal](lutP)(((-9:SReal)::List.tabulate[SReal](lutP-1)(i => math.log(((i+1)/lutP.toDouble)*1))):_*)

  lazy val logLUTBig = 
    LUT[SReal](lutP)(((-9:SReal)::List.tabulate[SReal](lutP-1)(i => math.log(((i+1)/lutP.toDouble)*200))):_*)
  
  lazy val expLUT = 
    LUT[SReal](lutP)(List.tabulate[SReal](lutP)(i => math.exp(i/lutP.toDouble*20-10)):_*)

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


  val initV: (CReal, CReal) = (0.0, 0.0)
  val initP: (CReal, CReal) = (0.0, 0.0)

  val matrix = new Matrix[SReal] {
    val IN_PLACE = false    
    def sqrtT(x: SReal) = sqrt(x)
    val zero           = 0
    val one            = 1
  }

  import matrix._

  def toMatrix(v: SVec2): Matrix = {
    Matrix(2, 1, List(v.x, v.y))
  }

  def toVec(v: SVec2): Vec = {
    Vec(v.x, v.y)
  }

  def initParticles(weights: SRAM1[SWeight], states: SRAM2[SReal], parFactor: Int) = {

    sqrtLUT
    expLUT
    logLUTSmall
    logLUTBig    
    Foreach(0 :: N par parFactor)(x => {

      Pipe {
        Pipe { states(x, 0) = initV._1 }
        Pipe { states(x, 1) = initV._2 }
        Pipe { states(x, 2) = initP._1 }
        Pipe { states(x, 3) = initP._2 }
      }

      weights(x) = math.log(1.0 / N)

    })
  }

  @virtualize def spatial() = {

    val inAcc     = StreamIn[TSA](In1)
    val inGPS     = StreamIn[TSB](In2)
    val out       = StreamOut[TSR](Out1)
    val parFactor = 1 (1 -> N)

    Accel {

      val weights = SRAM[SWeight](N)
      val states  = SRAM[SReal](N, 4)

      Sequential {

        initParticles(weights, states, parFactor)
        
        Sequential.Foreach(1 :: 101)(i => {

          updateFromAcc(inAcc.v, dt, states, parFactor)

          if (i%5 == 0) {
            updateFromGPS(inGPS.v, weights, states, parFactor)
            normSWeights(weights, parFactor)
          }

          out := TSR(i.to[Double]*dt.to[Double], averagePos(weights, states, parFactor))

          if (i%5 == 0 && tooLowEffective(weights))
            resample(weights, states, parFactor)

        })
      }
    }
  }

  @virtualize def updateFromAcc(acc: SAcceleration2D, dt: STime, states: SRAM2[SReal], parFactor: Int) = {

    Foreach(0 :: N par parFactor)(i => {
      val dv = sampleVel(acc, dt, covAcc)
      val s  = Matrix.fromSRAM1(4, states, i)
      val ds = Matrix(4, 1, List(dv(0), dv(1), s(0, 0)*dt, s(1, 0)*dt))      
      val ns = s + ds
      ns.loadTo(states, i)
    })
  }

  @virtualize def tooLowEffective(weights: SRAM1[SReal]): Boolean = {
    val thresh = log(1.0/N)
    val c = Reduce(0)(0::N)(i => if (weights(i) > thresh) 1 else 0)(_+_)
    c < N/10
  }

  def updateFromGPS(pos: SPosition2D, weights: SRAM1[SReal], states: SRAM2[SReal], parFactor: Int) = {
    val covPos = Matrix.eye(2, covGPS)
    Foreach(0 :: N par parFactor)(i => {
      val state = Matrix.fromSRAM1(2, states, i, ofs=2)
      val lik   = unnormalizedGaussianLogPdf(toMatrix(pos), state, covPos)
      weights(i) = weights(i) + lik
    })
  }

  @virtualize def sampleVel(a: SAcceleration2D, dt: STime, covAcc: SReal): Vec = {
    val withNoise  = gaussianVec(toVec(a), covAcc)
    val integrated = withNoise * dt
    integrated
  }

  def gaussianVec(mean: Vec, variance: SReal) = {
    val g1 = gaussian()
    (Vec(g1._1, g1._2) * sqrt(variance)) + mean
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

  @virtualize def normSWeights(weights: SRAM1[SWeight], parFactor: Int) = {
    val totalSWeight = Reg[SReal](0)
    val maxR        = Reg[SReal](-100)
    maxR.reset
    totalSWeight.reset
    Reduce(maxR)(0 :: N)(i => weights(i))(max(_, _))
    Reduce(totalSWeight)(0 :: N)(i => exp(weights(i) - maxR))(_ + _)
    totalSWeight := maxR + log(totalSWeight)
    Foreach(0 :: N par parFactor)(i => {
      weights(i) = weights(i) - totalSWeight
    })

  }

  @virtualize def resample(weights: SRAM1[SWeight], states: SRAM2[SReal], parFactor: Int) = {

    val cweights  = SRAM[SReal](N)
    val outStates = SRAM[SReal](N, 4)

    val u = random[SReal](1.0)

    Foreach(0 :: N)(i => {
      if (i == 0)
        cweights(i) = exp(weights(i))
      else
        cweights(i) = cweights(i - 1) + exp(weights(i))
    })

    val k = Reg[Int](0)
    Foreach(0 :: N)(i => {
      def notDone = (cweights(k) * N < i.to[SReal] + u) && k < N
      FSM[Boolean, Boolean](notDone)(x => x)(x => k := k + 1)(x => notDone)

      Foreach(0 :: 4)(x => {
        outStates(i, x) = states(k, x)
      })

    })

    Foreach(0 :: N par parFactor)(i => {

      Foreach(0 :: 4)(x => {
        states(i, x) = outStates(i, x)
      })
      
      weights(i) = log(1.0 / N)
    })

  }

  def unnormalizedGaussianLogPdf(measurement: Matrix, state: Matrix, cov: Matrix): SReal = {
    val e = (measurement - state)
    -1 / 2.0 * ((e.t * (cov.inv) * e).apply(0, 0))
  }

  @virtualize def averagePos(weights: SRAM1[SReal], states: SRAM2[SReal], parFactor: Int): SVec2 = {
    val accumP = RegFile[SReal](2, List[SReal](0, 0))
    accumP.reset
    Foreach(0 :: N par parFactor, 0 :: 2)((i, j) => {
      accumP(j) = accumP(j) + exp(weights(i)) * states(i, j + 2)
    })

    SVec2(accumP(0), accumP(1))
  }
  

}

object MiniParticleFilterInterpreter extends MiniParticleFilter with SpatialStreamInterpreter {

  val outs = List(Out1)

  val inputs = collection.immutable.Map[Bus, List[MetaAny[_]]](
    (In1 -> List[CReal](0f, 1f, 2f, 3f, 2f).map(x => TSA(x, SVec2(x, x)))),
    (In2 -> List[CReal](1f).map(x => TSB(x, SVec2(x, x))))
  )

}

object MiniParticleFilterCompiler extends MiniParticleFilter with SpatialStreamCompiler
