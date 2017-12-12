import spatial.dsl._
import org.virtualized._

trait ComplexPrimitiveTest extends SpatialApp {
  final val inv_sqrt_2xPI = 23

  type T = Int

  @module
  def CNDF(x: Int) = {
    val ax = abs(x)

    val xNPrimeofX = exp_taylor((ax ** 2) * -3) * inv_sqrt_2xPI
    val xK2 = 1.to[Int] / ((ax * 3) + 1)

    val xK2_2 = xK2 ** 2
    val xK2_3 = xK2_2 * xK2
    val xK2_4 = xK2_3 * xK2
    val xK2_5 = xK2_4 * xK2

    val xLocal_10 = xK2 * 3
    val xLocal_20 = xK2_2 * -5
    val xLocal_30 = xK2_3 * 7
    val xLocal_31 = xK2_4 * -8
    val xLocal_32 = xK2_5 * 5

    val xLocal_21 = xLocal_20 + xLocal_30
    val xLocal_22 = xLocal_21 + xLocal_31
    val xLocal_23 = xLocal_22 + xLocal_32
    val xLocal_1 = xLocal_23 + xLocal_10

    val xLocal0 = xLocal_1 * xNPrimeofX
    val xLocal  = -xLocal0 + 1

    mux(x < 0, xLocal0, xLocal)
  }


  @module
  def BlkSchlsEqEuroNoDiv(sptprice: Int, strike: Int, rate: Int, volatility: Int, time: Int, otype: Int) = {
    val xLogTerm = log_taylor( sptprice / strike )
    val xPowerTerm = (volatility ** 2) / 2
    val xNum = (rate + xPowerTerm) * time + xLogTerm
    val xDen = volatility * sqrt_approx(time)

    val xDiv = xNum / (xDen ** 2)
    val nofXd1 = CNDF(xDiv)
    val nofXd2 = CNDF(xDiv - xDen)

    val futureValueX = strike * exp_taylor(-rate * time)

    val negNofXd1 = -nofXd1 + 1
    val negNofXd2 = -nofXd2 + 1

    val optionPrice1 = (sptprice * nofXd1) - (futureValueX * nofXd2)
    val optionPrice2 = (futureValueX * negNofXd2) - (sptprice * negNofXd1)
    mux(otype == 0, optionPrice2, optionPrice1)
  }

  @virtualize
  def main(): Unit = {
    val mx = ArgIn[Int]
    val x0 = ArgIn[Int]
    val x1 = ArgIn[Int]
    val x2 = ArgIn[Int]
    val x3 = ArgIn[Int]
    val x4 = ArgIn[Int]
    val x5 = ArgIn[Int]

    val y0 = ArgOut[Int]
    val y1 = ArgOut[Int]

    val MAX = args(0).to[Int]
    val N0 = args(1).to[Int]
    val N1 = args(2).to[Int]
    val N2 = args(3).to[Int]
    val N3 = args(4).to[Int]
    val N4 = args(5).to[Int]
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
      val out1 = Reg[Int](0)
      val out2 = Reg[Int](0)

      Foreach(0 until max){ i => body(i, out1, out2, x0, x1, x2, x3, x4, x5) }
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


    println("PASS: " + cksum + " (ComplexPrimitiveTestReuseNone)")
  }

  def body(i: Int, out1: Reg[Int], out2: Reg[Int], x0: Int, x1: Int, x2: Int, x3: Int, x4: Int, x5: Int): Unit

}


object ComplexPrimitiveTestReuseNone extends ComplexPrimitiveTest {
  def body(i: Int, out1: Reg[Int], out2: Reg[Int], x0: Int, x1: Int, x2: Int, x3: Int, x4: Int, x5: Int): Unit = {
    Parallel {
      Pipe { out1 := BlkSchlsEqEuroNoDiv(x0, x1, x2, x3, x4, x5) }
      Pipe { out2 := BlkSchlsEqEuroNoDiv(x0, x1, x2, x3, x4, x5) }
    }
  }
}

object ComplexPrimitiveTestReuseSome extends ComplexPrimitiveTest {
  def body(i: Int, out1: Reg[Int], out2: Reg[Int], x0: Int, x1: Int, x2: Int, x3: Int, x4: Int, x5: Int): Unit = {
    Sequential {
      Pipe { out1 := BlkSchlsEqEuroNoDiv(x0, x1, x2, x3, x4, x5) }

      Parallel {
        Pipe { out1 := BlkSchlsEqEuroNoDiv(x0, x1, x2, x3, x4, x5) }
        Pipe { out2 := BlkSchlsEqEuroNoDiv(x0, x1, x2, x3, x4, x5) }
      }

      Pipe { out2 := BlkSchlsEqEuroNoDiv(x0, x1, x2, x3, x4, x5) }
    }
  }
}

object ComplexPrimitiveTestReuseAll extends ComplexPrimitiveTest {
  def body(i: Int, out1: Reg[Int], out2: Reg[Int], x0: Int, x1: Int, x2: Int, x3: Int, x4: Int, x5: Int): Unit = {
    Sequential {
      Pipe { out1 := BlkSchlsEqEuroNoDiv(x0, x1, x2, x3, x4, x5) }
      Pipe { out2 := BlkSchlsEqEuroNoDiv(x1, x3, x2, x4, x0, x5) }
    }
  }
}



