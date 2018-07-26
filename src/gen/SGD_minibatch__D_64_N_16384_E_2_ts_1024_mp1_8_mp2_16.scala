import spatial.dsl._
import spatial.targets._
import virtualized._

object SGD_minibatch__D_64_N_16384_E_2_ts_1024_mp1_8_mp2_16 extends SpatialApp { self => // Regression (Dense) // Args: 40 64 0.0001

val D = 64
val N = 16384
val E = 2
val ts = 1024
val mp1 = 8
val mp2 = 16

  val A = 0.0001f
  val ip = 16

  type TM = FixPt[TRUE,_16,_16]
  type TX = FixPt[TRUE,_16,_16]
  val margin = 1

  @virtualize
  def sgdminibatch(x_in: Array[TX], y_in: Array[TX], alpha: TM, epochs: Int, nn: Int) = {
    val E = ArgIn[Int]
    val N = ArgIn[Int]
    val A = ArgIn[TM]

    setArg(E, epochs); bound(E) = self.E
    setArg(N, nn); bound(N) = self.N
    setArg(A, alpha)

    val x = DRAM[TX](N,D)
    val y = DRAM[TX](N)
    val result = DRAM[TM](D)

    setMem(x, x_in)
    setMem(y, y_in)

    Accel {
      val y_tile = SRAM[TX](ts)
      val sgdmodel = SRAM[TM](D)
      val x_tile = SRAM[TX](ts,D)
      Pipe(D by 1) { i => sgdmodel(i) = 0.to[TM]}
      Sequential.Foreach(E by 1, N by ts) { (e,b) =>
        y_tile load y(b::b+ts par ip)
        x_tile load x(b::b+ts, 0::D par ip)
        val y_err = SRAM[TX](ts)
        Foreach(ts by 1 par mp1) { i => 
          val y_hat = Reduce(Reg[TX])(D by 1 par ip){ j => x_tile(i,j) * sgdmodel(j).to[TX] }{_+_}
          y_err(i) = y_tile(i) - y_hat.value
        }
        MemFold(sgdmodel)(ts by 1 par mp2) { i =>
          val row = SRAM[TX](D)
          Foreach(D by 1 par ip) { j => row(j) = x_tile(i,j) * y_err(i) * A }
          row
        } { _ + _ }
      }
      result(0::D par ip) store sgdmodel
    }

    getMem(result)

  }

  def printArr(a: Array[TM], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  @virtualize
  def main() {
    val sX = Array.fill(N){ Array.fill(D){ random[TX](3.to[TX]) + 1.to[TX]} }
    val ideal_model = Array.tabulate(D){ i => 2.to[TM] }
    val sY = Array.tabulate(N){i => ideal_model.zip(sX.apply(i)){case (a,b) => a.to[TX]*b}.reduce{_+_}}
    val id = Array.tabulate(D){ i => i }
    val ep = Array.tabulate(E){ i => i }

    val result = sgdminibatch(sX.flatten, sY, A.to[TM], E, N)



    val cksum = ideal_model.zip(result){ case (a,b) => abs(a - b) < margin }.reduce{_&&_}
    printArr(result, "result: ")
    printArr(ideal_model, "gold: ")
    println("PASS: " + cksum  + " (SGD_minibatch)")
  }
}
