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

trait LSTMParams extends SpatialApp {
  val kernelSize = (1500, 100)
  val hiddenSize = 100
  val N = 60
  val M = 1
  val JX = 161
  val idco = 1400
  val odco = 200
  val internalPath = "/home/tianzhao/spatial-lang/apps/LSTM-internals"
  val inputPath = ""
  val outputPath = ""
  val kernelPath = "" 
  val biasPath = ""
}


trait prepro extends SpatialApp { 
  def prepro[T:Type:Num]() {

  }
}


trait BasicLSTMCell_16_16 extends SpatialApp with LSTMParams
                                             with Activations {
  def Forward[T:Type:Num]() {

  }
}


trait RNN extends SpatialApp {
  def RNNForward[T:Type:Num]() {

  }
}


object LSTM_16_16 extends SpatialApp with BasicLSTMCell_16_16
                                     with RNN {
  type X = FixPt[TRUE, _16, _16]

  @virtualize
  def main() {
    val result = Forward[X]()
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
