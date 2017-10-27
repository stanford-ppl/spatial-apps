import spatial.dsl._
import org.virtualized._


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


trait Activations extends SpatialApp with CharRNNParams 
  val projectDir = "/home/tianzhao/spatial-lang/apps/src/activation-luts/"
  val loSig = 16
  val loTanh = 4
  val spacingShiftBitsSig = 5 // shift down for sig
  val spacingShiftBitsTanh = 7 // shift down for tanh
  val lutNSig = 512
  val lutNTanh = 512
  val sigF = projectDir + "sigmoid_512_16_-5.0.csv"
  val tanhF = projectDir + "tanh_512_4_-7.0.csv"


  def sigmoid_(p: aT) = {
    val halfSigLUT = LUT.fromFile[aT](lutNSig)(sigF)
    val index = (abs(p).to[iT] << spacingShiftBitsSig).to[Index] + 1.to[Index]
    val valueMux = mux(p < 0.to[aT], 1.to[aT] - halfSigLUT(index), halfSigLUT(index))
    val lowerMux = mux(p <= -loSig.to[aT], 0.to[aT], valueMux)
    val upperMux = mux(p >= loSig.to[aT], 1.to[aT], lowerMux)
    upperMux
  }


  def sigmoid_old(p: aT) = {
    val halfSigLUT = LUT.fromFile[aT](lutNSig)(sigF)
    val index = (abs(p).to[iT] << spacingShiftBitsSig).to[Index]
    val valueMux = mux(p < 0.to[aT], 1.to[aT] - halfSigLUT(index), halfSigLUT(index))
    val lowerMux = mux(p <= -loSig.to[aT], 0.to[aT], valueMux)
    val upperMux = mux(p >= loSig.to[aT], 1.to[aT], lowerMux)
    upperMux
  }


  def tanh_(p: aT) = {
    val halfTanhLUT = LUT.fromFile[aT](lutNTanh)(tanhF)
    val index = (abs(p).to[iT] << spacingShiftBitsTanh).to[Index] // + 1.to[Index]
    val valueMux = mux(p < 0.to[aT], 0.to[aT] - halfTanhLUT(index), halfTanhLUT(index))
    val lowerMux = mux(p <= -loTanh.to[aT], -1.to[aT], valueMux)
    val upperMux = mux(p >= loTanh.to[aT], 1.to[aT], lowerMux)
    upperMux
  }


  // A few ideas about implementing tanh:
  // range doesn't need to go that much. -8 to 8 would be fine.
  // However more precisions need to be provided within 0 to 8.

  // Implement a tanh using: tanh(x) = 2 * sigmoid(2*x) - 1
  def tanh_approx(p: aT) = {
    (sigmoid_(p << 1) << 1) - 1
  }
}
