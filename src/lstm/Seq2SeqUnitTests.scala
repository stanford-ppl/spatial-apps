import spatial.dsl._
import virtualized._

// This file contains tests needed for buildling the GNMT. 
// T0: BiLSTM
// T1: BankedLSTMEncoder (Single Unit, Banked Weights)
// T2: AttentionWrapper (Single Unit, Banked Weights, Bahdanau)
// T3: BankedLSTMDecoder (Single Unit, Banked Weights)
// T4: Fused BeamSearch

/**
* Should support the following APIs:
* testActivation
* activation
* testSigmoid
* sigmoid
* bankedLinear
* testBankedLinear
*/

object Seq2SeqUnitTests extends SpatialApp {
  type F = Float
  //type T = FixPt[TRUE, _6, _10]
  //type shiftSigType = FixPt[TRUE, _6, _0]
  //type shiftTanhType = FixPt[TRUE, _6, _0]

  type T = Int
  type shiftSigType = Int
  type shiftTanhType = Int
  // TODO: ideally want to use type T = FixPt[TRUE, _5, _11], shiftSigType = FixPt[TRUE, _5, _0]
  val paramPath = 
    "/home/tianzhao/data/rnn/seq2seq/gnmt_decoder/test/".asInstanceOf[java.lang.String]
  val featureSize = 64
  val hiddenSize = 64
  val nGates = 4
  val nDecoderLayers = 3
  val xahSize = featureSize * 3
  val gatesSize = hiddenSize * nGates
  val batchSize = 1
  val maxTime = 4
  val forgetBiasVal = 1
  val featureTile = featureSize
  val gateTile = 1
  val hiddenTile = nGates * gateTile


  // TODO: add targets
  @virtualize
  def main() {
    val forgetBias = forgetBiasVal.to[T]
     
    def csvfy(name: java.lang.String): java.lang.String = 
      paramPath + name + ".csv" 


    def saveDRAM2CSV(src: DRAM2[T], fname: java.lang.String): Unit = 
      writeCSV2D(getMatrix(src), csvfy(fname))

    def loadCSVMat(name: java.lang.String, dim0: Int, dim1: Int): DRAM2[T] = {
      // TODO: this way works. however if you do: 
      // loadCSVMat[T:Type:Num](arg0, arg1...)
      // and then call loadCSVMat[T](......)
      // Error occurs such as
      // Cannot find way to cast type spatial.dsl.MString to type T.
      val k = DRAM[T](dim0, dim1)
      val data = loadCSV2D[T](csvfy(name), ",", "\n")
      setMem(k, data)
      k
    }

    def loadXA(name: java.lang.String, 
      dim0: Int,
      dim1: Int,
      dim2: Int): DRAM3[T] = {
      val xa = DRAM[T](dim0, dim1, dim2)
      val data = loadCSV2D[T](paramPath+name+".csv", ",", "\n")
      setMem(xa, data)
      xa
    }

    def activation(in: T): T = {
      /** This function implements a tanh
      * Comment: the maximal error in this one is 18%. Check the simulation script 
      * for more info.
      */
      // type shiftTanhType = FixPt[TRUE, _5, _0]
      val up = (2.5).to[T]
      val mi = (0.5).to[T]
      val bi = (0.375).to[T]
      val zero = 0.to[T]
      val sftOffset = 2.to[shiftTanhType]
      val absin = abs(in)
      val div4 = absin >> sftOffset
      val li = div4 + bi
      val absout = if (absin > up) (1.to[T])
        else if (mi < absin && absin < up) (li)
        else (absin)
      val negout = zero - absout 

      // print("   tanh: in = " + in)
      // print(", absin = " + absin)
      // print(", div4 = " + div4)
      // print(", li = " + li)
      // print(", absout = " + absout)
      // print(", negout = " + negout)

      val re = mux(in < zero, negout, absout)
      // println(", re = " + re)
      re
    }

    def sigmoid(in: T): T = {
      /**
      * Frame sigmoid as: 
      * sig(a) = (tanh(a/2) + 1) / 2
      * Comment: error of sigmoid implemented using tanh is less than 8%.
      */
       
      // print("   sigmoid: in = " + in)
      // type shiftSigType = FixPt[TRUE, _5, _0]
      val sft1 = 1.to[shiftSigType]
      val bias = 1.to[T]
      val shiftedIn = in >> sft1
      // print(", shiftedIn = " + shiftedIn)
      val sftin = (activation(shiftedIn) + bias) >> sft1
      // println(", sftin = " + sftin)
      sftin
    }

    class DoubleBufferedIOMem(dim0: Int = batchSize, dim1: Int = featureSize) {
      val nWay: Int = 2 
      val fBuf: SRAM2[T] = SRAM[T](dim0, dim1)
      val bBuf: SRAM2[T] = SRAM[T](dim0, dim1)
      val isBBufSrc = Reg[Boolean](true)

      def init(src: DRAM2[T]) { bBuf load src(0::dim0, 0::dim1) }

      def write(idx0: Index, idx1: Index, src: T) {
        if (isBBufSrc.value) {
          fBuf(idx0, idx1) = src
        } else {
          bBuf(idx0, idx1) = src
        }
      }

      def read(idx0: Index, idx1: Index): T =
        if (isBBufSrc.value) (bBuf(idx0, idx1)) else (fBuf(idx0, idx1))

      def flip() { isBBufSrc := mux(isBBufSrc.value, false, true) }

      def flush(dst: DRAM2[T]) { 
        if (isBBufSrc.value) {
          dst(0::dim0, 0::dim1) store bBuf
        } else {
          dst(0::dim0, 0::dim1) store fBuf
        }
      }
    }

    def stepStagedBankedLSTMXAH(xInit: DRAM2[T], aInit: DRAM2[T], hInit: DRAM2[T], cInit: DRAM2[T],
      w: DRAM2[T], b: DRAM2[T], xout: DRAM2[T], cout: DRAM2[T], hout: DRAM2[T]) {
      // This function advances one step in a 3-layer LSTM. XAH are all stored together.
      // Since latency matters and we don't have a lot of load / store streams, 
      // We must reuse these functional elements.  
      // w is banked

      val xx = new DoubleBufferedIOMem(batchSize, featureSize) // 2 * 1, 1 stream
      val hh = new DoubleBufferedIOMem(batchSize * nDecoderLayers, hiddenSize) // 3 * 2 * 1, 2 stream
      val cc = new DoubleBufferedIOMem(batchSize * nDecoderLayers, hiddenSize) // 3 * 2 * 1, 1 stream
      val xwOffset = 0
      val awOffset = featureSize
      val hwOffset = featureSize * 2
      // TODO: need to rewrite init as a read / write.
      xx.init(xInit)
      hh.init(hInit)
      cc.init(cInit)
      val a = SRAM[T](batchSize, hiddenSize) // 1
      a load aInit(0::batchSize, 0::hiddenSize)

      Sequential.Foreach (nDecoderLayers by 1) { iDL => // TODO: Can this one be a normal Foreach ?
                                              // but I do want to overlap some compute with writes...
        Foreach (hiddenSize by hiddenTile) { iTile =>
          val bw = SRAM[T](xahSize, hiddenTile) // 4 * 3
          val bb = SRAM[T](batchSize, hiddenTile) // 4 * 1
          bw load w(iDL * featureSize::(iDL+1)*featureSize, iTile::iTile+hiddenTile) // 1 stream
          bb load b(iDL * batchSize::(iDL+1)*batchSize, iTile::iTile+hiddenTile)  // 1 stream

          def reductionUnitXAH(idx: Index): T = {
            /** Reduce on x, a, h separately. 
            * It is not a good idea to fuse x, a, h into one memory,
            * since these three memories are used for different purposes
            * x is reused between layeres, 
            * h is reused between timesteps,
            * a is used only once by the first layer.
            * TODO: the two reduction methods can be fused
            */
            def reductionUnit(src: SRAM2[T], bwOffset: Index): T = {
              Reduce(Reg[T](0))(featureSize by 1 par 16) { iFeature =>
                val ele = src(0, iFeature) 
                val eleW = bw(idx, bwOffset+iFeature)
                ele * eleW
              }{_+_}
            }

            //old = 
              //Reduce (0 until 3){ j  => // x a h
                //Reduce(Reg[T](0))(featureSize by 1 par 16) { k => // inner pipe
                  //SRAM(i, j, k)
                //}
              //} { _ + _ }

            def reductionUnitDB(src: DoubleBufferedIOMem, bwOffset: Index): T = {
              Reduce(Reg[T](0))(featureSize by 1 par 16) { iFeature => // inner pipe
                val ele = src.read(0, iFeature)
                val eleW = bw(idx, bwOffset+iFeature)  
                ele * eleW
              }{_+_}
            }

            val xReg = Reg[T](0)
            val aReg = Reg[T](0)
            val hReg = Reg[T](0)
            Parallel {
              xReg := reductionUnitDB(xx, xwOffset)
              aReg := reductionUnit(a, awOffset)
              hReg := reductionUnitDB(hh, hwOffset)
            }

            val xah = xReg.value + aReg.value + hReg.value
            xah
          }

          /** Manually reduce each gateTile.
          * Each gate tile contains info for a gate
          * The result is directly reduced to c and to h
          * TODO: may need to support a way to increase the size of hiddenTile.
          */
          val i = Reg[T](0)
          val j = Reg[T](0)
          val f = Reg[T](0)
          val o = Reg[T](0)

          Parallel {
            i := sigmoid(reductionUnitXAH(0)) + bb(0, 0)
            j := activation(reductionUnitXAH(1)) + bb(0, 1)
            f := sigmoid(reductionUnitXAH(2) + forgetBias) + bb(0, 2) + forgetBias
            o := sigmoid(reductionUnitXAH(3)) + bb(0, 3)
          } 

          val updateIndex = iTile / 4
          val cEle = cc.read(0, updateIndex)
          val cEleNew = cEle * f + i * j
          val hEleNew = activation(cEleNew) * o


          Parallel {
            cc.write(0, updateIndex, cEleNew)
            hh.write(0 ,updateIndex, hEleNew)
            xx.write(0, updateIndex, hEleNew)
          }
        }
      }

      xx.flush(xout)
      cc.flush(cout) // don't need c.flush in the full app. Only need it here for validation.
      hh.flush(hout) // similarly, we don't really need h here. Just for debugging.
    }

    def testStepStagedBankedLSTM() {
      val w = loadCSVMat("concat_banked_kernel_1_3", xahSize * nDecoderLayers, gatesSize)
      val b = loadCSVMat("concat_banked_bias_1_3", batchSize * nDecoderLayers, gatesSize)
      // xa requires an extra time dimension
      // val x = loadXA("xatt", maxTime, batchSize, featureSize * 2) 
      val xInit = loadCSVMat("x", batchSize, featureSize)
      val aInit = loadCSVMat("a", batchSize, featureSize)
      val cInit = loadCSVMat("concat_c_init_1_3", batchSize * nDecoderLayers, hiddenSize)
      val hInit = loadCSVMat("concat_h_init_1_3", batchSize * nDecoderLayers, hiddenSize)
   
      val xout = DRAM[T](batchSize, featureSize)
      val cout = DRAM[T](batchSize * nDecoderLayers, hiddenSize)
      val hout = DRAM[T](batchSize * nDecoderLayers, hiddenSize)
      Accel {
        stepStagedBankedLSTMXAH(xInit, aInit, hInit, cInit, w, b, xout, cout, hout)
      }

      def saveDRAM2CSV(src: DRAM2[T], fname: java.lang.String): Unit = 
        writeCSV2D(getMatrix(src), csvfy(fname))

      saveDRAM2CSV(xout, "xout")
      saveDRAM2CSV(cout, "cout")
      saveDRAM2CSV(hout, "hout")
    }

    testStepStagedBankedLSTM()

  }
}
