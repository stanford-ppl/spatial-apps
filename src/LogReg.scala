import spatial.dsl._
import spatial.targets._
import virtualized._

object LogReg extends SpatialApp {

  val iters = 1 // param
  val N = 1024 // param pmuSize * 10
  val ts = 64 // param pmuSize
  val op = 1 // param (1, <N> / <ts>, 2)
  val mp = 1 // param (1, min(<ts>, 5), 2)

  type X = Float //FixPt[TRUE,_16,_16]

  val margin = 5
  val D = 192
  val A = 0.0000001f

  val ip = 16

  def sigmoid[T:Type:Num](t:T) = 1.to[T]/(exp(-t) + 1.to[T])

  @virtualize
  def logreg[T:Type:Num](xIn: Array[T], yIn: Array[T], tt: Array[T], n: Int, it: Int) = {
    val iters = ArgIn[Int]
    val N     = ArgIn[Int]
    setArg(iters, it)
    setArg(N, n)

    val BN = ts (96 -> 96 -> 9600)
    val PX = 1 (1 -> 1)
    val P3 = op (1 -> 96)

    val x = DRAM[T](N, D)
    val y = DRAM[T](N)
    val theta = DRAM[T](D)

    setMem(x, xIn)
    setMem(y, yIn)
    setMem(theta, tt)

    //Accel {
      //val btheta = SRAM[T](D)

      //Sequential.Foreach(iters by 1) { epoch =>

        //Sequential.MemReduce(btheta par ip)(1 by 1){ xx =>
          //val gradAcc = SRAM[T](D)
          //Foreach(N by BN){ i =>
            //val logregX = SRAM[T](BN, D)
            //val logregY = SRAM[T](BN)
            //Parallel {
              //logregX load x(i::i+BN, 0::D par ip)
              //logregY load y(i::i+BN par ip)
            //}
            //MemReduce(gradAcc par ip)(BN par P3){ ii =>
              //val pipe2Res = Reg[T]
              //val subRam   = SRAM[T](D)

              //val dotAccum = Reduce(Reg[T])(D par ip){j => logregX(ii,j) * btheta(j) }{_+_}  // read
              //Pipe { pipe2Res := (logregY(ii) - sigmoid(dotAccum.value)) }
              //Foreach(D par ip) {j => subRam(j) = logregX(ii,j) * pipe2Res.value }
              //subRam
            //}{_+_}
          //}
          //gradAcc
        //}{(b,g) => b+g*A.to[T]}

        //// Flush gradAcc
        ////Pipe(D by 1 par ip) { i => gradAcc(i) = 0.to[T]}
      //}
      //theta(0::D par ip) store btheta // read
    //}

    Accel {
      val btheta = SRAM[T](D)

      btheta load theta(0::D par ip)

      Sequential.Foreach(iters by 1) { epoch =>

        val grad = MemReduce(SRAM[T](D) par ip)(N by BN par op){ i =>
          val xTile = SRAM[T](BN, D)
          val yTile = SRAM[T](BN)
          Parallel {
            xTile load x(i::i+BN, 0::D par ip)
            yTile load y(i::i+BN par ip)
          }
          MemReduce(SRAM[T](D) par ip)(BN by 1 par mp) { ii =>
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
      // printArr(gold, "gold now")
    }


    printArray(gold, "gold: ")
    printArray(result, "result: ")

    val cksum = result.zip(gold){ (a,b) => a > b-margin && a < b+margin}.reduce{_&&_}
    // println("max err: " + result.zip(gold){(a,b) => (a-b)*(a-b)}.reduce{Math.max(_,_)})
    // println("mean err: " + result.zip(gold){(a,b) => (a-b)*(a-b)}.reduce{_+_} / D)
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

