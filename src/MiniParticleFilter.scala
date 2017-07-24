import spatial.dsl.{Matrix => _, _ => _}
import org.virtualized._
import spatial.SpatialCompiler
import spatial.interpreter._
import spatial.metadata._
import collection.mutable.Map
import spatial.stdlib._

trait MiniParticleFilter extends SpatialStream {

  type CReal = scala.Double
//  type Real  = Double

  type Real  = FixPt[TRUE, _16, _16]

  implicit def toReal(x: CReal) = x.to[Real]
  def sqrt(x: Real) = sqrt_approx(x)

  val lutP: scala.Int = 10000
  lazy val logLUT = 
    LUT[Real](lutP)(((-9:Real)::List.tabulate[Real](lutP-1)(i => math.log(((i+1)/lutP.toDouble)*20))):_*)

  lazy val expLUT = 
    LUT[Real](lutP)(List.tabulate[Real](lutP)(i => math.exp(i/lutP.toDouble*10-10)):_*)
  
  @virtualize
  def log(x: Real) = 
    logLUT(((x/20.0)*(lutP.toDouble)).to[Index])

  @virtualize
  def exp(x: Real): Real = {
    if (x >= 0.0)
      1
    else
      expLUT((((x+10)/10.0)*(lutP.toDouble)).to[Index])
  }
  val N: scala.Int          = 10
  val initV: (CReal, CReal) = (0.0, 0.0)
  val initP: (CReal, CReal) = (0.0, 0.0)

  val initTime: CReal = 0.0
  val covAcc: CReal   = 0.01
  val covGPS: CReal   = 0.01

  @struct case class Vec2(x: Real, y: Real)

  type Time         = Real
  type Position     = Vec2
  type Acceleration = Vec2
  type Weight       = Real

  val matrix = new Matrix[Real] {
    def sqrtT(x: Real) = sqrt(x)
    val zero           = 0
    val one            = 1
  }

  import matrix._

  def toMatrix(v: Vec2): Matrix = {
    Matrix(2, 1, List(v.x, v.y))
  }

  def toVec(v: Vec2): Vec = {
    Vec(v.x, v.y)
  }

  def initParticles(weights: SRAM1[Weight], states: SRAM2[Real], parFactor: Int) = {

    expLUT
    logLUT
    Foreach(0 :: N par parFactor)(x => {

      Pipe {
        Pipe { states(x, 0) = initV._1 }
        Pipe { states(x, 1) = initV._2 }
        Pipe { states(x, 2) = initP._1 }
        Pipe { states(x, 3) = initP._2 }
      }

      Parallel {
        weights(x) = math.log(1.0 / N)
      }

    })
  }

  @virtualize def prog() = {

    val inAcc     = StreamIn[Acceleration](In1)
    val inGPS     = StreamIn[Position](In2)
    val out       = StreamOut[Position](Out1)
    val parFactor = 1 (1 -> N)

    Accel {

      val weights = SRAM[Weight](N)
      val states  = SRAM[Real](N, 4)
      val dt      = 0.1

      Sequential {

        initParticles(weights, states, parFactor)

        Sequential.Foreach(0 :: 6)(i => {

          updateFromAcc(inAcc, dt, states, parFactor)

          if (i == 5)
            updateFromGPS(inGPS, weights, states, parFactor)

          normWeights(weights, parFactor)
          out := averagePos(weights, states, parFactor)          
          resample(weights, states, parFactor)          

        })
      }
    }
  }

  def updateFromAcc(acc: Acceleration, dt: Time, states: SRAM2[Real], parFactor: Int) = {

    Foreach(0 :: N par parFactor)(i => {
      val nq = sampleVel(acc, dt, covAcc)
      val v  = Matrix.fromSRAM1(4, states, i)
      val dv = Matrix(4, 1, List(nq(0), nq(1), v(0, 0) * dt, v(1, 0) * dt))
      val nv = v + dv
//      println(v(0,0))
      nv.loadTo(states, i)
    })
  }

  def updateFromGPS(pos: Position, weights: SRAM1[Real], states: SRAM2[Real], parFactor: Int) = {
    val covPos = Matrix.eye(2, covGPS)
    Foreach(0 :: N par parFactor)(i => {
      val state = Matrix.fromSRAM1(2, states, i, 2)
      val lik   = unnormalizedGaussianLogPdf(toMatrix(pos), state, covPos)
      weights(i) = weights(i) + lik
    })
  }

  def sampleVel(a: Acceleration, dt: Time, covAcc: Real): Vec = {
    val withNoise  = gaussianVec(toVec(a), covAcc)
    val integrated = withNoise * dt
    integrated
  }

  def gaussianVec(mean: Vec, variance: Real) = {
    val g1 = gaussian()
    Vec(g1._1, g1._2) * sqrt(variance) + mean
  }

  //Box-Muller
  //http://www.design.caltech.edu/erik/Misc/Gaussian.html
  @virtualize def gaussian() = {

    val x1 = Reg[Real]
    val x2 = Reg[Real]
    val w  = Reg[Real]
    val w2 = Reg[Real]

    FSM[Boolean, Boolean](true)(x => x)(x => {
      x1 := 2.0 * random[Real](1.0) - 1.0
      x2 := 2.0 * random[Real](1.0) - 1.0
      w := (x1 * x1 + x2 * x2)
    })(x => w.value >= 1.0)

    w2 := sqrt((-2.0 * log(w.value)) / w)

    val y1 = x1 * w2;
    val y2 = x2 * w2;

    (y1, y2)
  }

  @virtualize def normWeights(weights: SRAM1[Weight], parFactor: Int) = {
    val totalWeight = Reg[Real](0)
    val maxR        = Reg[Real]

    maxR := weights(0)

    Reduce(maxR)(0 :: N)(i => weights(i))(max(_, _))
    Reduce(totalWeight)(0 :: N)(i => exp(weights(i) - maxR))(_ + _)
    totalWeight := maxR + log(totalWeight)
    Foreach(0 :: N par parFactor)(i => {
      weights(i) = weights(i) - totalWeight
    })

  }

  @virtualize def resample(weights: SRAM1[Weight], states: SRAM2[Real], parFactor: Int) = {

    val cweights  = SRAM[Real](N)
    val outStates = SRAM[Real](N, 4)

    val u = random[Real](1.0)

    Foreach(0 :: N)(i => {
      if (i == 0)
        cweights(i) = exp(weights(i))
      else
        cweights(i) = cweights(i - 1) + exp(weights(i))
    })

    val k = Reg[Int](0)
    Foreach(0 :: N)(i => {
      val b = cweights(k) * N < i.to[Real] + u
      FSM[Boolean, Boolean](b)(x => x)(x => k := k + 1)(x => cweights(k) * N < i.to[Real] + u)

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

  def unnormalizedGaussianLogPdf(measurement: Matrix, state: Matrix, cov: Matrix): Real = {
    val e = (measurement - state)
    -1 / 2.0 * ((e.t * (cov.inv) * e).apply(0, 0))
  }

  @virtualize def averagePos(weights: SRAM1[Real], states: SRAM2[Real], parFactor: Int): Vec2 = {
    val accumP = RegFile[Real](2, List[Real](0, 0))
    Parallel {
      Foreach(0 :: N par parFactor, 0 :: 2)((i, j) => {
        accumP(j) = accumP(j) + exp(weights(i)) * states(i, j + 2)
      })
    }
    Vec2(accumP(0), accumP(1))
  }

}

object MiniParticleFilterInterpreter extends MiniParticleFilter with SpatialStreamInterpreter {

  val outs = List(Out1)

  val inputs = collection.immutable.Map[Bus, List[MetaAny[_]]](
    (In1 -> List[CReal](0f, 1f, 2f, 3f, 2f).map(x => Vec2(x, x))),
    (In2 -> List[CReal](1f).map(x => Vec2(x, x)))
  )

}

object MiniParticleFilterCompiler extends MiniParticleFilter with SpatialStreamCompiler
