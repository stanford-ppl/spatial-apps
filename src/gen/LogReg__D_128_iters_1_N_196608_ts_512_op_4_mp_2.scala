import spatial.dsl._
import spatial.targets._
import virtualized._

object LogReg__D_128_iters_1_N_196608_ts_512_op_4_mp_2 extends SpatialApp {

val D = 128
val iters = 1
val N = 196608
val ts = 512
val op = 4
val mp = 2

  type X = Float //FixPt[TRUE,_16,_16]

  val margin = 5
  val A = 0.0000001f

  val ip = 16

  def sigmoid[T:Type:Num](t:T) = 1.to[T]/(exp(-t) + 1.to[T])

  @virtualize
  def logreg[T:Type:Num](xIn: Array[T], yIn: Array[T], tt: Array[T], n: Int, it: Int) = {
    val iters = ArgIn[Int]
    val N     = ArgIn[Int]
    setArg(iters, it)
    setArg(N, n)

    val x = DRAM[T](N, D)
    val y = DRAM[T](N)
    val theta = DRAM[T](D)

    setMem(x, xIn)
    setMem(y, yIn)
    setMem(theta, tt)






    Accel {
      val btheta = SRAM[T](D)

      btheta load theta(0::D par ip)

      Sequential.Foreach(iters by 1) { epoch =>

        val grad = MemReduce(SRAM[T](D) par ip)(N by ts par op){ i =>
          val xTile = SRAM[T](ts, D)
          val yTile = SRAM[T](ts)
          Parallel {
            xTile load x(i::i+ts, 0::D par ip)
            yTile load y(i::i+ts par ip)
          }
          MemReduce(SRAM[T](D) par ip)(ts by 1 par mp) { ii =>
            val dot = Reduce(Reg[T])(D by 1) { d =>
              xTile(ii,d) * btheta(d)
            } { _ + _ }
            val sub = Reg[T]
            Pipe { 
              sub := yTile(ii) - sigmoid[T](dot)
            }
            val gradRow = SRAM[T](D)
            Foreach(D by 1 par ip) { d => gradRow(d) = xTile(ii, d) * sub.value }
            gradRow
          } { _ + _ }
        } { _ + _ }

        Foreach(D by 1) { d => btheta(d) = btheta(d) + grad(d) * A.to[T] }

      }

      theta(0::D par ip) store btheta // read
    }

    getMem(theta)
  }

  @virtualize
  def main() {

    val sX = Array.fill(N){ Array.fill(D){ random[X](10.to[X])} }
    val sY = Array.tabulate(N){ i => i.to[X]} //fill(N)( random[T](10.0) )
    val theta = Array.fill(D) {random[X](1.to[X]) }

    val result = logreg(sX.flatten,sY, theta, N, iters)

    printArray(theta, "theta: ")
    val gold = Array.empty[X](D)
    for (i <- 0 until D) {
      gold(i) = theta(i)
    }
    for (i <- 0 until iters) {
      val gradient = sX.zip(sY) {case (row, y) =>
        val sub = y - sigmoid(row.zip(gold){(a,b) => a*b}.reduce{_+_})
        row.map{r => r * sub}
      }.reduce{(a,b) => a.zip(b){_+_}}
      for (i <- 0 until D) {
        gold(i) = gold(i) + A*gradient(i)
      }
    }


    printArray(gold, "gold: ")
    printArray(result, "result: ")

    val cksum = result.zip(gold){ (a,b) => a > b-margin && a < b+margin}.reduce{_&&_}
    println("PASS: " + cksum  + " (LogReg)")


    /* OptiMl version
    val w = untilconverged(theta, maxIter = 30) { (cur, iter) =>
      val gradient = ((0::x.numRows) { i =>
        x(i)*(y(i) - sigmoid(cur *:* x(i)))
      }).sum
      val z = cur + gradient*alpha
      z
    }
    */

  }

}

