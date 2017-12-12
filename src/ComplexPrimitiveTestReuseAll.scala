import spatial.dsl._
import org.virtualized._
import spatial.stdlib._
import spatial.targets._


object ComplexPrimitiveTestReuseAll extends SpatialApp {
  
  final val inv_sqrt_2xPI = 0.39894228040143270286f

  @module
  def CNDF(x: Float) = {
    val ax = abs(x)

    val xNPrimeofX = exp_taylor((ax ** 2) * -0.05f) * inv_sqrt_2xPI
    val xK2 = 1.to[Float] / ((ax * 0.2316419f) + 1.0f)

    val xK2_2 = xK2 ** 2
    val xK2_3 = xK2_2 * xK2
    val xK2_4 = xK2_3 * xK2
    val xK2_5 = xK2_4 * xK2

    val xLocal_10 = xK2 * 0.319381530f
    val xLocal_20 = xK2_2 * -0.356563782f
    val xLocal_30 = xK2_3 * 1.781477937f
    val xLocal_31 = xK2_4 * -1.821255978f
    val xLocal_32 = xK2_5 * 1.330274429f

    val xLocal_21 = xLocal_20 + xLocal_30
    val xLocal_22 = xLocal_21 + xLocal_31
    val xLocal_23 = xLocal_22 + xLocal_32
    val xLocal_1 = xLocal_23 + xLocal_10

    val xLocal0 = xLocal_1 * xNPrimeofX
    val xLocal  = -xLocal0 + 1.0f

    mux(x < 0.0f, xLocal0, xLocal)
  }


  @module
  def BlkSchlsEqEuroNoDiv(sptprice: Float, strike: Float, rate: Float,
    volatility: Float, time: Float, otype: Int) = {

      val xLogTerm = log_taylor( sptprice / strike )
      val xPowerTerm = (volatility ** 2) * 0.5f
      val xNum = (rate + xPowerTerm) * time + xLogTerm
      val xDen = volatility * sqrt(time)

      val xDiv = xNum / (xDen ** 2)
      val nofXd1 = CNDF(xDiv)
      val nofXd2 = CNDF(xDiv - xDen)

      val futureValueX = strike * exp_taylor(-rate * time)

      val negNofXd1 = -nofXd1 + 1.0f
      val negNofXd2 = -nofXd2 + 1.0f

      val optionPrice1 = (sptprice * nofXd1) - (futureValueX * nofXd2)
      val optionPrice2 = (futureValueX * negNofXd2) - (sptprice * negNofXd1)
      mux(otype == 0, optionPrice2, optionPrice1)
  }
  
  
  @virtualize
  def main() {
    // Declare SW-HW interface vals
    val mx = ArgIn[Int]
    val x0 = ArgIn[Float]
    val x1 = ArgIn[Float]
    val x2 = ArgIn[Float]
    val x3 = ArgIn[Float]
    val x4 = ArgIn[Float]
    val x5 = ArgIn[Int]

    val y0 = ArgOut[Float]
    val y1 = ArgOut[Float]

    val MAX = args(0).to[Int]
    val N0 = args(1).to[Float]
    val N1 = args(2).to[Float]
    val N2 = args(3).to[Float]
    val N3 = args(4).to[Float]
    val N4 = args(5).to[Float]
    val N5 = args(6).to[Int]

    // Connect SW vals to HW vals
    setArg(x0, N0)
    setArg(x1, N1)
    setArg(x2, N2)
    setArg(x3, N3)
    setArg(x4, N4)
    setArg(x5, N5)
    setArg(mx, MAX)

    // Create HW accelerator
    Accel {
      val max = mx.value
      val out1 = Reg[Float](0)
      val out2 = Reg[Float](0)

      Sequential.Foreach(0 until max){ i=>
        Pipe { out1 := BlkSchlsEqEuroNoDiv(x0, x1, x2, x3, x4, x5) }
        Pipe { out2 := BlkSchlsEqEuroNoDiv(x1, x3, x2, x4, x0, x5) }
      }

      y0 := out1
      y1 := out2

    }


    // Extract results from accelerator
    val result1 = getArg(y0)
    val result2 = getArg(y1)

    // Create validation checks and debug code
    val gold = BlkSchlsEqEuroNoDiv(N0, N1, N2, N3, N4, N5)
    println("expected: " + gold)
    println("result1: " + result1)
    println("result2: " + result2)

    val cksum1 = gold == result1
    val cksum2 = result1 == result2
    val cksum = cksum1 && cksum2


    println("PASS: " + cksum + " (ComplexPrimitiveTestReuseAll)")
  }
}

