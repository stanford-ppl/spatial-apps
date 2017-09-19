import spatial.dsl._
import org.virtualized._
import sys.process._
import scala.math.{log,exp}


// TODO: seems that activation is not creating the right value
// Need better quantization?
trait Activations extends SpatialApp {
  val projectDir = "/home/tianzhao/spatial-lang/"
  val lo = 32
  val revPrec = 2
  val totalSize = 128


  // TODO: seems that the aggregated output is quite off. need to fix it...
  // 128 elements, inputs: -32 to 32, outputs: 0 to 1
  def sigmoid[T:Type:Num](p: T) = {
    val sigmoidDir = projectDir + "apps/src/__128_sigmoidLUT.csv"
    val sigmoidLUT = LUT.fromFile[T](totalSize)(sigmoidDir)
    val lowerMux = mux(p < -lo.to[T], 0.to[T], sigmoidLUT(((p + lo.to[T]) * revPrec.to[T]).to[Int]))
    val upperMux = mux(p > lo.to[T], 1.to[T], lowerMux)
    upperMux
  }


  // 128 elements, inputs: -32 to 32, outputs: -1 to 1
  def tanh[T:Type:Num](p: T) = {
    val tanhDir = projectDir + "apps/src/__128_tanhLUT.csv"
    val tanhLUT = LUT.fromFile[T](totalSize) (tanhDir)
    val lowerMux = mux(p < -lo.to[T], -1.to[T], tanhLUT(((p + lo.to[T]) * revPrec.to[T]).to[Int]))
    val upperMux = mux(p > lo.to[T], 1.to[T], lowerMux)
    upperMux
  }
}


trait LUTBase extends SpatialApp with Activations {
  def Forward[T:Type:Num](N: T) = {
    val x = ArgIn[T]
    val y = ArgOut[T]

    setArg(x, N)

    Accel {
      y := x.value + sigmoid[T](4.to[T])
    }

    val result = getArg(y)
    result
  }
}


trait LSTMParams_dcell4 extends SpatialApp {
  val kernelSize = (1500, 100)
  val d = 100
  val N = 60
  val M = 1
  val JX = 161
  val idco = 1400
  val odco = 200
  val internalPath = "/home/tianzhao/spatial-lang/apps/LSTM-internals/"
  val inputPath = internalPath + "IOs/input-0.csv"
  val outputPath = internalPath + "IOs/output-0.csv"
  val kernelPath = internalPath + "weights/kernel-d_cell4_fw-1500-400.csv"
  val biasPath = internalPath + "weights/bias-d_cell4_fw-1500-400.csv"
}


trait Prepro extends SpatialApp with LSTMParams_dcell4 { 
  def prepro[T:Type:Num]()(implicit cast: Cast[MString, T]) = {
    val k = loadCSV2D[T](kernelPath, ",", "\n")
    val b = loadCSV2D[T](biasPath, ",", "\n")
    val inHidden = loadCSV2D[T](inputPath, ",", "\n")
    (k, b, inHidden)
  }
}


// This traits contain different flavors of matmult
trait MatMult extends SpatialApp with LSTMParams_dcell4 {
  // MatMult parallelization factors
  // val innerPar = 16
  // val midPar = 2
  // val outerPar = 2
  // val px = 1 (1 -> 1) 
  val op = 2
  val mp = 2 
  val ip = 16
  val px = 1
  val MM = N*M*JX
  val NN = 4*d
  // TODO: figure out what the max size of BRAM is allowed here
  val bm = 10
  val bp = 10
  val bn = 10

  def MatMult[T:Type:Num](a: DRAM2[T], b: DRAM2[T], c: DRAM2[T]) {
    Foreach(MM by bm, NN by bn par op){(i,j) =>
      val tileC = SRAM[T](bm, bn)

      Foreach(P by bp par px){k =>
        val tileA = SRAM[T](bm, bp)
        val tileB = SRAM[T](bp, bn)
        Parallel {
          tileA load a(i::i+bm, k::k+bp par 1) // Reads M*N*P times
          tileB load b(k::k+bp, j::j+bn par 1)
        }
        
        Foreach(bm by 1, bn by 1 par mp){ (ii,jj) =>    // MetaPipe?
          val prod = Reduce(Reg[T])(bp by 1 par ip){kk => tileA(ii, kk) * tileB(kk, jj) }{_+_}
          val prev = mux(k == 0, 0.to[T], tileC(ii,jj))
          tileC(ii,jj) = prev + prod.value // Is a unit pipe that should be recognized as accum
        }
      }

      c(i::i+bm, j::j+bn) store tileC // Writes M*N times
    }
  }
}


// Cell implementation
trait BasicLSTMCell_16_16 extends SpatialApp with Prepro
                                             with Activations
                                             with MatMult {
  def CellForward[T:Type:Num](dramTuple: (DRAM2[T], DRAM[T], DRAM2[T]))(implicit cast: Cast[MString, T]) = {
    val (kdram, bdram, inhiddram) = dramTuple
     
  }
}


// RNN Sequential Logistics
// This is the part where you can swap in various definitions of BasicLSTMCell
trait RNN extends SpatialApp with BasicLSTMCell_16_16 
                              with Prepro {
  def RNNForward[T:Type:Num]()(implicit cast: Cast[MString, T]) {
    val (k, b, inHidden) = prepro[T]()
    val kdram = DRAM[T](idco+d, d*4)
    val bdram = DRAM[T](d*4)
    val inhiddram = DRAM[T](N*M*JX, (idco+d))
    setMem(kdram, k)
    setMem(bdram, b)
    setMem(inhiddram, inHidden)
    val dramTuple = (kdram, bdram, inhiddram)
    // TODO: how to compress input x? It needs to be of a dynamic shape
    // TODO: for now let's assume that it's of fixed shape 161 as JX 
    Accel {
      // TODO: Pipelining across different stages?
      CellForward[T](dramTuple)
    } 

    // TODO: fill rnnResult
    // rnnResult
  }
}


object LSTM_16_16 extends SpatialApp with RNN {
  type X = FixPt[TRUE, _16, _16]

  @virtualize
  def main() {
    val rnnResult = RNNForward[X]()
  }
}


object LUTTest extends SpatialApp with LUTBase {
  type X = FixPt[TRUE, _16, _16]

  @virtualize
  def main() {
    val N = args(0).to[X]
    val gold = args(0).to[Float] + 1.0 / (1.0 + exp(-4.0))
    println("expected: " + gold)

    val result = Forward[X](N)
    println("result: " + result)
  }
}
