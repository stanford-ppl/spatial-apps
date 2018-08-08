import spatial.dsl._
import virtualized._



trait CharRNNTestParams extends SpatialApp {
  val N = 50
  val JX = 2
  val dco = 100
  val d = 100
  val dn = 10
  val ddco = 100
  val dd = 10
  val simFileDir = "/home/tianzhao/spatial-lang/apps/np-sims/"
  val dataPaths = List(simFileDir + "/a.csv", simFileDir + "/hidden.csv",
                       simFileDir + "/memory.csv", simFileDir + "/kernel.csv",
                       simFileDir + "/bias.csv")
}

trait Activations extends SpatialApp {
  type targetT = FixPt[TRUE, _16, _16]
  type LUTInT = FixPt[TRUE, _16, _16]
  val projectDir = "/home/tianzhao/spatial-lang/apps/src/activation-luts/"
  val loSig = 16
  val loTanh = 4
  val spacingShiftBitsSig = 5 // shift down for sig
  val spacingShiftBitsTanh = 7 // shift down for tanh
  val lutNSig = 512
  val lutNTanh = 512
  val sigF = projectDir + "sigmoid_512_16_-5.0.csv"
  val tanhF = projectDir + "tanh_512_4_-7.0.csv"

  // This design implements tanh and sigmoid using taylor expansion
  def tanh_pw[T:Type:Num](t: T): T = {
    val cube_t = (t * t) * t / 3.to[T] // accurate till the cube term
    t - cube_t
  }

  // tanh(x) = 2sigmoid(2x) - 1
  // sigmoid(2x) = (tanh(x) + 1) / 2
  // sigmoid(y) = (tanh(y/2) + 1) / 2
  def sig_pw[T:Type:Num](t: T): T = {
    (tanh_pw(t / 2.to[T]) + 1.to[T]) / 2.to[T]
  }


  // This design implements the paper: http://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=6409494
  // where a combinational circuit with 0 SRAM is used for getting tanh
  // @virtualize
  // def tanh

  // These 2 are only for scala simulations
  def sigmoidSim[T:Type:Num](t: T) = 1.to[T]/(exp(-t) + 1.to[T])


  def tanhSim[T:Type:Num](t: T) = (exp(t) - exp(-t)) / (exp(t) + exp(-t))


  @virtualize
  def sigmoid_(p: LUTInT): targetT = {
    p
  }


  @virtualize
  def tanh_(p: LUTInT): targetT = {
    p
  }

  // targetT defines the precison we want for this LUT
  @virtualize
  def sigmoid_LUT(p: LUTInT): targetT = {
    p
    // val zero = 0.to[targetT]
    // val one = 1.to[targetT]
    // val loSigT = loSig.to[targetT]
    // val halfSigLUT = LUT.fromFile[targetT](lutNSig)(sigF)
    // val index = (abs(p).to[targetT] << spacingShiftBitsSig).to[Index] + 1.to[Index]
    // val valueMux = mux(p < zero, one - halfSigLUT(index), halfSigLUT(index))
    // val lowerMux = mux(p <= -loSigT, zero, valueMux)
    // val upperMux = mux(p >= loSigT, one, lowerMux)
    // upperMux
  }


  @virtualize
  def tanh_LUT(p: LUTInT): targetT = {
    p
    // val zero = 0.to[targetT]
    // val one = 1.to[targetT]
    // val loTanhT = loTanh.to[targetT]
    // val halfTanhLUT = LUT.fromFile[targetT](lutNTanh)(tanhF)
    // val index = (abs(p).to[targetT] << spacingShiftBitsTanh).to[Int] // + 1.to[Index]
    // val valueMux = mux(p < zero, zero - halfTanhLUT(index), halfTanhLUT(index))
    // val lowerMux = mux(p <= -loTanhT, -one, valueMux)
    // val upperMux = mux(p >= loTanhT, one, lowerMux)
    // upperMux
  }

  // Implement a tanh using: tanh(x) = 2 * sigmoid(2*x) - 1
  // def tanh_approx(p: aT) = {
  //   (sigmoid_(p << 1) << 1) - 1
  // }
}
