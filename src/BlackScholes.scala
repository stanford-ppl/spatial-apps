import spatial.dsl._
import org.virtualized._
import spatial.targets._

object BlackScholes extends SpatialApp {

  type T = Float//FixPt[TRUE,_32,_32]
  val margin = 0.2f // Validates true if within +/- margin

  final val inv_sqrt_2xPI = 0.39894228040143270286f.to[T]

  @virtualize
  def CNDF(x: T): T = {
    val ax = abs(x)

    val xNPrimeofX = exp_taylor((ax ** 2) * -0.05f.to[T]) * inv_sqrt_2xPI
    val xK2 = 1.to[T] / ((ax * 0.2316419f.to[T]) + 1.0f.to[T])

    val xK2_2 = xK2 ** 2
    val xK2_3 = xK2_2 * xK2
    val xK2_4 = xK2_3 * xK2
    val xK2_5 = xK2_4 * xK2

    val xLocal_10 = xK2 * 0.319381530f.to[T]
    val xLocal_20 = xK2_2 * -0.356563782f.to[T]
    val xLocal_30 = xK2_3 * 1.781477937f.to[T]
    val xLocal_31 = xK2_4 * -1.821255978f.to[T]
    val xLocal_32 = xK2_5 * 1.330274429f.to[T]

    val xLocal_21 = xLocal_20 + xLocal_30
    val xLocal_22 = xLocal_21 + xLocal_31
    val xLocal_23 = xLocal_22 + xLocal_32
    val xLocal_1 = xLocal_23 + xLocal_10

    val xLocal0 = xLocal_1 * xNPrimeofX
    val xLocal  = -xLocal0 + 1.0f.to[T]

    mux(x < 0.0f.to[T], xLocal0, xLocal)
  }

  @virtualize
  def BlkSchlsEqEuroNoDiv(sptprice: T, strike: T, rate: T,
    volatility: T, time: T, otype: Int): T = {

    val xLogTerm = log_taylor( sptprice / strike )
    val xPowerTerm = (volatility ** 2) * 0.5f.to[T]
    val xNum = (rate + xPowerTerm) * time + xLogTerm
    val xDen = volatility * sqrt_approx(time)

    val xDiv = xNum / (xDen ** 2)
    val nofXd1 = CNDF(xDiv)
    val nofXd2 = CNDF(xDiv - xDen)

    val futureValueX = strike * exp_taylor(-rate * time)

    val negNofXd1 = -nofXd1 + 1.0f.to[T]
    val negNofXd2 = -nofXd2 + 1.0f.to[T]

    val optionPrice1 = (sptprice * nofXd1) - (futureValueX * nofXd2)
    val optionPrice2 = (futureValueX * negNofXd2) - (sptprice * negNofXd1)
    mux(otype == 0, optionPrice2, optionPrice1)
  }

  @virtualize
  def blackscholes(
    stypes:      Array[Int],
    sprices:     Array[T],
    sstrike:     Array[T],
    srate:       Array[T],
    svolatility: Array[T],
    stimes:      Array[T]
  ): Array[T] = {
    val innerPar = 16
    val outerPar = 1
    val tileSize = 16
    val B  = tileSize (32 -> 96 -> 19200)
    val OP = outerPar (1 -> 2)
    val IP = innerPar (1 -> tileSize)
    val par_load = tileSize
    val par_store = tileSize

    val size = stypes.length; bound(size) = 9995328

    val N = ArgIn[Int]
    setArg(N, size)

    val types    = DRAM[Int](N)
    val prices   = DRAM[T](N)
    val strike   = DRAM[T](N)
    val rate     = DRAM[T](N)
    val vol      = DRAM[T](N)
    val times    = DRAM[T](N)
    val optprice = DRAM[T](N)
    setMem(types, stypes)
    setMem(prices, sprices)
    setMem(strike, sstrike)
    setMem(rate, srate)
    setMem(vol, svolatility)
    setMem(times, stimes)

    Accel {
      Foreach(N by B par OP) { i =>
        val typeBlk   = SRAM[Int](B)
        val priceBlk  = SRAM[T](B)
        val strikeBlk = SRAM[T](B)
        val rateBlk   = SRAM[T](B)
        val volBlk    = SRAM[T](B)
        val timeBlk   = SRAM[T](B)
        val optpriceBlk = SRAM[T](B)

        Parallel {
          typeBlk   load types(i::i+B par par_load)
          priceBlk  load prices(i::i+B par par_load)
          strikeBlk load strike(i::i+B par par_load)
          rateBlk   load rate(i::i+B par par_load)
          volBlk    load vol(i::i+B par par_load)
          timeBlk   load times(i::i+B par par_load)
        }

        Foreach(B par IP){ j =>
          val price = BlkSchlsEqEuroNoDiv(priceBlk(j), strikeBlk(j), rateBlk(j), volBlk(j), timeBlk(j), typeBlk(j))
          optpriceBlk(j) = price
        }
        optprice(i::i+B par par_store) store optpriceBlk
      }
    }
    getMem(optprice)
  }

  @virtualize
  def main(): Unit = {
    val N = args(0).to[Int]

    val types  = Array.fill(N)(1 + random[Int](2))
    val prices = Array.fill(N)(1 + random[T])
    val strike = Array.fill(N)(1 + random[T])
    val rate   = Array.fill(N)(1 + random[T])
    val vol    = Array.fill(N)(1 + random[T])
    val time   = Array.fill(N)(1 + random[T])

    val out = blackscholes(types, prices, strike, rate, vol, time)

    val gold = Array.tabulate(N){i => 
      BlkSchlsEqEuroNoDiv(prices(i), strike(i), rate(i), vol(i), time(i), types(i))
    }
    printArray(out, "result: ")
    printArray(gold, "gold: ")

    val cksum = out.zip(gold){ case (o, g) => (g < (o + margin.to[T])) && g > (o - margin.to[T])}.reduce{_&&_}
    println("PASS: " + cksum + " (BlackSholes)")


  }
}
