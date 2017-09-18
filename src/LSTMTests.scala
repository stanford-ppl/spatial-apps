import spatial.dsl._
import org.virtualized._
import sys.process._
import scala.math.{log,exp}


trait Activations extends SpatialApp {
  val projectDir = "/home/tianzhao/spatial-lang/"
  val lo = 32
  val revPrec = 2
  val totalSize = 128

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
  val hiddenSize = 100
  val N = 60
  val M = 1
  val JX = 161
  val idco = 1400
  val odco = 200
  val internalPath = "/home/tianzhao/spatial-lang/apps/LSTM-internals/"
  val inputPath = internalPath + "IOs/input-0.csv"
  val outputPath = internalPath + "IOs/output-0.csv"
  val kernelPath_i = internalPath + "weights/i-kernel-d_cell4_fw-1500-100.csv" 
  val kernelPath_j = internalPath + "weights/j-kernel-d_cell4_fw-1500-100.csv" 
  val kernelPath_f = internalPath + "weights/f-kernel-d_cell4_fw-1500-100.csv" 
  val kernelPath_o = internalPath + "weights/o-kernel-d_cell4_fw-1500-100.csv" 
  val biasPath_i = internalPath + "weights/i-bias-d_cell4_fw-1500-100.csv"
  val biasPath_j = internalPath + "weights/j-bias-d_cell4_fw-1500-100.csv"
  val biasPath_f = internalPath + "weights/f-bias-d_cell4_fw-1500-100.csv"
  val biasPath_o = internalPath + "weights/o-bias-d_cell4_fw-1500-100.csv"

  // Paths for the full kernel 
  val kernelPath = internalPath + "weights/kernel-d_cell4_fw-1500-400.csv"
  val biasPath = internalPath + "weights/bias-d_cell4_fw-400.csv"
}


trait Prepro extends SpatialApp with LSTMParams_dcell4 { 
  def prepro[T:Type:Num]()(implicit cast: Cast[MString, T]) = {
    val ik = loadCSV2D[T](kernelPath_i, ",", "\n")
    val jk = loadCSV2D[T](kernelPath_j, ",", "\n")
    val fk = loadCSV2D[T](kernelPath_f, ",", "\n")
    val ok = loadCSV2D[T](kernelPath_o, ",", "\n")
    val ib = loadCSV2D[T](biasPath_i, ",", "\n")
    val jb = loadCSV2D[T](biasPath_j, ",", "\n")
    val fb = loadCSV2D[T](biasPath_f, ",", "\n")
    val ob = loadCSV2D[T](biasPath_o, ",", "\n")
    // val k = loadCSV2D[T](kernelPath, ",", "\n")
    // val b = loadCSV2D[T](biasPath_i, ",", "\n")
    val inHidden = loadCSV2D[T](inputPath, ",", "\n")
    // (ik, jk, fk, ok, ib, jb, fb, ob, inHidden)
    
  }
}


// Cell implementation
trait BasicLSTMCell_16_16 extends SpatialApp with Prepro
                                             with Activations {
  def CellForward[T:Type:Num](dramTuple: (DRAM2[T], DRAM2[T], DRAM2[T], 
                                            DRAM2[T], DRAM2[T], DRAM2[T], 
                                            DRAM2[T], DRAM2[T], DRAM2[T]))(implicit cast: Cast[MString, T]) = {
    val (ikdram, jkdram, fkdram, okdram, ibdram, jbdram, fbdram, obdram, inhdram) = dramTuple

  }
}


// RNN Sequential Logistics
// This is the part where you can swap in various definitions of BasicLSTMCell
trait RNN extends SpatialApp with BasicLSTMCell_16_16 
                              with Prepro {
  def RNNForward[T:Type:Num]()(implicit cast: Cast[MString, T]) {
    val (ik, jk, fk, ok, ib, jb, fb, ob, inHidden) = prepro[T]()
    val ikdram = DRAM[T](idco, hiddenSize)
    val jkdram = DRAM[T](idco, hiddenSize)
    val fkdram = DRAM[T](idco, hiddenSize) 
    val okdram = DRAM[T](idco, hiddenSize)
    val ibdram = DRAM[T](idco, hiddenSize)
    val jbdram = DRAM[T](idco, hiddenSize)
    val fbdram = DRAM[T](idco, hiddenSize)
    val obdram = DRAM[T](idco, hiddenSize)
    val inhdram = DRAM[T](N*M*JX, (idco+hiddenSize))
    setMem(ikdram, ik)
    setMem(jkdram, jk)
    setMem(fkdram, fk)
    setMem(okdram, ok)
    setMem(ibdram, ib)
    setMem(jbdram, jb)
    setMem(fbdram, fb)
    setMem(obdram, ob)
    setMem(inhdram, inHidden)
    val dramTuple = (ikdram, jkdram, fkdram, okdram, ibdram, jbdram, fbdram, obdram, inhdram)
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
