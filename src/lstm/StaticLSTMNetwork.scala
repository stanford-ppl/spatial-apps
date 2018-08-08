import spatial.dsl._
import virtualized._

object StaticLSTMNetwork extends SpatialApp with DataGenerator {
  /**
  * This app implements an LSTM network with statically known sequence length and number of layers.
  * @feature Combined GEMM Ops (TensorFlow, cuDNN)
  * @feature Multi Streams for hidden state computation (cuDNN) 
  * @feature Point-wise kernel fusion (cuDNN)
  * @feature Weight matrix pre-transposing (cuDNN)
  * @feature Wavefront matrix of LSTM processing cores (cuDNN)
  * @feature Combined inputs on time axis (cuDNN)
  * @feature Fused GEMM and elementwise operator (Spatial)
  * @feature Hardware pipeline for feature computation (Spatial) 
  * @feature Hardware pipeline for implementing wavefront matrix (Spatial)
  */

  type T = FixPt[TRUE, _8, _24]
  type shiftSigType = FixPt[TRUE, _8, _0]
  type shiftTanhType = FixPt[TRUE, _8, _0]

  val nFeatures = 128
  val nHiddenUnits = 128
  val nReduce = 128
  val nGates = 4
  val nSteps = 8
  val nLayers = 2
  val forgetBias = 1
  val nUnitsPerTile = 4 
  val nXH = 2
  val nXAH = 3
  val nParallelFeatureGenerator = 1 // number of parallel computation units for feature generation 
  val innerPar = 16
  val stepPar = 1
  val layerPar = 1
  val stPar = 16
  val ldPar = 16

  val paramPath = 
    "/home/tianzhao/data/rnn/seq2seq/gnmt_decoder/test/".asInstanceOf[java.lang.String]

  @virtualize
  def csvfy(name: java.lang.String): java.lang.String = paramPath + name + ".csv" 

  @virtualize
  def getDRAMCSV2D(name: java.lang.String, dim0: Int, dim1: Int): DRAM2[T] = {
    val k = DRAM[T](dim0, dim1)
    val data = loadCSV2D[T](csvfy(name), ",", "\n")
    setMem(k, data)
    k
  }

  @virtualize
  def getDRAMCSV1D(name: java.lang.String, dim0: Int): DRAM1[T] = {
    val k = DRAM[T](dim0)
    val data = loadCSV1D[T](csvfy(name), ",")
    setMem(k, data)
    k
  }

  @virtualize
  def getDRAMCSV3D(name: java.lang.String, dim0: Int, dim1: Int,
    dim2: Int): DRAM3[T] = {
    val k = DRAM[T](dim0, dim1, dim2)
    val data = loadCSV1D[T](csvfy(name), ",")
    setMem(k, data)
    k
  }

  @virtualize
  def getDRAMCSV4D(name: java.lang.String, dim0: Int, dim1: Int,
    dim2: Int, dim3: Int): DRAM4[T] = {
    val k = DRAM[T](dim0, dim1, dim2, dim3)
    val data = loadCSV1D[T](csvfy(name), ",")
    setMem(k, data)
    k
  }

  @virtualize
  def getDRAMCSV5D(name: java.lang.String, dim0: Int, dim1: Int,
    dim2: Int, dim3: Int, dim4: Int): DRAM5[T] = {
    val k = DRAM[T](dim0, dim1, dim2, dim3, dim4)
    val data = loadCSV1D[T](csvfy(name), ",")
    setMem(k, data)
    k
  }


  @virtualize
  def getMemTensor1D(src: DRAM1[T], dim0: scala.Int): SRAM[T] = {
    /**
    * Loads an array from mem into a 1D SRAM
    * @param src: source DRAM 
    * @param dim0: size of dimension 0
    */
    val re = SRAM[T](dim0)
    re load src(0::dim0 par ldPar)
    re
  }

  @virtualize
  def getMemTensor2D(src: DRAM2[T], dim0: scala.Int,
    dim1: scala.Int): SRAM2[T] = {
    /**
    * Loads a matrix from mem into a 1D SRAM
    * @param src: source DRAM 
    * @param dim0: size of dimension 0 
    * @param dim1: size of dimension 1
    */

    val re = SRAM[T](dim0, dim1)
    re load src(0::dim0, 0::dim1 par ldPar)
    re 
  }

  @virtualize
  def getMemTensor3D(src: DRAM3[T], dim0: scala.Int, dim1: scala.Int,
    dim2: scala.Int): SRAM3[T] = {
    /**
    * Loads a cube from mem into a 3D SRAM
    * @param src: source DRAM 
    * @param dim0: size of dimension 0
    * @param dim1: size of dimension 1
    * @param dim2: size of dimension 2
    */

    val re = SRAM[T](dim0, dim1, dim2)
    re load src(0::dim0, 0::dim1, 0::dim2 par ldPar)
    re
  }

  @virtualize
  def getMemTensor4D(src: DRAM4[T], dim0: scala.Int, dim1: scala.Int, dim2: scala.Int, 
    dim3: scala.Int): SRAM4[T] = {
    /**
    * Loads a 4-D tensor into a 4-D SRAM
    * @param src: source DRAM
    * @param dim0: size of dimension 0
    * @param dim1: size of dimension 1
    * @param dim2: size of dimension 2
    * @param dim3: size of dimension 3
    */

    val re = SRAM[T](dim0, dim1, dim2, dim3)
    re load src(0::dim0, 0::dim1, 0::dim2, 0::dim3 par ldPar)
    re
  }

  @virtualize
  def getMemTensor5D(src: DRAM5[T], dim0: scala.Int, dim1: scala.Int, dim2: scala.Int,
    dim3: scala.Int, dim4: scala.Int): SRAM5[T] = {
    /**
    * Loads a 5-D tensor into a 5-D SRAM
    * @param src: source DRAM
    * @param dim0: size of dimension 0
    * @param dim1: size of dimension 1
    * @param dim2: size of dimension 2
    * @param dim3: size of dimension 3
    * @param dim4: size of dimension 4
    */

    val re = SRAM[T](dim0, dim1, dim2, dim3, dim4)
    re load src(0::dim0, 0::dim1, 0::dim2, 0::dim3, 0::dim4 par ldPar)
    re
  }

  @virtualize 
  def activation(in: T): T = {
    /** This function implements an activation function using 5-piece linear approx
    * Comment: the maximal error in this one is 18%.
    * Check the simulation script for more info.
    * 
    * @param in: input of type T
    */
    val up = (2.5).to[T]
    val mi = (0.5).to[T]
    val bi = (0.375).to[T]
    val zero = 0.to[T]
    val sftOffset = 2.to[shiftTanhType]
    val absin = abs(in)
    val div4 = absin >> sftOffset
    val li = div4 + bi
    //val absout = if (absin > up) (1.to[T])
      //else if (mi < absin && absin < up) (li)
      //else (absin)
    val absout = mux(absin > up, 1.to[T], 
      mux(mi < absin && absin < up, li, absin)
    )
    val negout = zero - absout 
    val re = mux(in < zero, negout, absout)
    re
  }

  @virtualize
  def sigmoid(in: T): T = {
    /** This function implements the hard_sigmoid function as in the tensorflow keras backend. 
    * @param in: input of type T
    */
    val up = (2.5).to[T]
    val dn = (-2.5).to[T]
    //val mu = (2.0).to[T]
    val scale = (0.2).to[T]
    val offset = (0.5).to[T]
    //val re = if (in < dn) (0.to[T]) else if (in >= up) (1.to[T]) else (scale * in + offset)
    val re = mux(in < dn, 0.to[T], mux(in >= up, 1.to[T], scale * in + offset))
    re
  }

  case class LSTMIOs(xInit: DRAM2[T], cInit: DRAM2[T], hInit: DRAM2[T], 
    biInit: DRAM2[T], bjInit: DRAM2[T], bfInit: DRAM2[T], boInit: DRAM2[T],
    wxiInit: DRAM3[T], wxjInit: DRAM3[T], wxfInit: DRAM3[T], wxoInit: DRAM3[T], 
    whiInit: DRAM3[T], whjInit: DRAM3[T], whfInit: DRAM3[T], whoInit: DRAM3[T], 
    out: DRAM2[T])

  @virtualize
  def lstmCore(io: LSTMIOs): Matrix[T] = {
    /**
    * Implements a single layer LSTM with fixed sequence length. 
    * This can be compared against cuDNN RNN performance
    * w is pre-transposed and is banked with a feature-first manner.
    * @param xInit: initial values for inputs
    * @param cInit: initial values for memory states
    * @param hInit: initial values for hidden states
    * @param wxInit: initial values for kernel with x
    * @param whInit: init values for kenrel with h
    * @param bInit: initial values for bias
    */

   /*
    * pmu count
    * x c h init 3
    * x h 2
    * w per gate 8
    * b per gate 4
    *
    * pcu
    * w pmu addr calculation 8
    * reduction 8 (2 per gate)
    *
    * */
    Accel {
      val x = getMemTensor2D(io.xInit, nSteps, nFeatures) // pack all the x's.
      val c = getMemTensor2D(io.cInit, nLayers, nFeatures) 
      val h = getMemTensor2D(io.hInit, nLayers, nFeatures)

      val wxi = getMemTensor3D(io.wxiInit, nLayers, nHiddenUnits, nFeatures)
      val wxj = getMemTensor3D(io.wxjInit, nLayers, nHiddenUnits, nFeatures)
      val wxf = getMemTensor3D(io.wxfInit, nLayers, nHiddenUnits, nFeatures)
      val wxo = getMemTensor3D(io.wxoInit, nLayers, nHiddenUnits, nFeatures)

      val whi = getMemTensor3D(io.whiInit, nLayers, nHiddenUnits, nFeatures)
      val whj = getMemTensor3D(io.whjInit, nLayers, nHiddenUnits, nFeatures)
      val whf = getMemTensor3D(io.whfInit, nLayers, nHiddenUnits, nFeatures)
      val who = getMemTensor3D(io.whoInit, nLayers, nHiddenUnits, nFeatures)

      val bi = getMemTensor2D(io.biInit, nLayers, nFeatures)
      val bj = getMemTensor2D(io.bjInit, nLayers, nFeatures)
      val bf = getMemTensor2D(io.bfInit, nLayers, nFeatures)
      val bo = getMemTensor2D(io.boInit, nLayers, nFeatures)

      Foreach (nSteps by 1 par stepPar) { iStep =>
        Foreach (nLayers by 1 par layerPar) { iLayer =>
          // Q: would this form a wavefront processor?  
          // core in a 2-D grid of (nSteps. nLayers)
          //
          Foreach (nFeatures by 1 par nParallelFeatureGenerator) { iFeature => // output feature
            val xfifo = FIFO[T](1)
            val hfifo = FIFO[T](1)
            Foreach(nHiddenUnits by 1 par innerPar) { iReduce =>
              xfifo.enq(x(iStep, iReduce))
              hfifo.enq(h(iLayer, iReduce))
            }
            def reduceGate(memx: SRAM3[T], memh: SRAM3[T]): (T, T) = {
              val xReduceReg = Reg[T](0)
              val hReduceReg = Reg[T](0)
              Reduce(xReduceReg)(nHiddenUnits by 1 par innerPar) { iReduce =>
                val xEle = xfifo.deq()
                val xProduct = xEle * memx(iLayer, iFeature, iReduce) 
                xProduct
              }{_+_}
              Reduce(hReduceReg)(nHiddenUnits by 1 par innerPar) { iReduce =>
                val hEle = hfifo.deq()
                val hProduct = hEle * memh(iLayer, iFeature, iReduce)
                hProduct
              }{_+_}

              // val result = xReduceReg.value + hReduceReg.value + b(iLayer, iGate, iFeature)
              (xReduceReg.value, hReduceReg.value)
            }

            val i = Reg[T](0)
            val j = Reg[T](0)
            val f = Reg[T](0)
            val o = Reg[T](0)
            Pipe { // 12 cu
              val (xi, hi) = reduceGate(wxi, whi)
              i := sigmoid(xi + hi + bi(iLayer, iFeature))
              val (xj, hj) = reduceGate(wxj, whj)
              j := activation(xj + hj + bj(iLayer, iFeature))
              val (xf, hf) = reduceGate(wxf, whf)
              f := sigmoid(xf + hf + bf(iLayer, iFeature) + forgetBias)
              val (xo, ho)  = reduceGate(wxo, who)
              o := sigmoid(xo + ho + bo(iLayer, iFeature))
            }

            // TODO: Do we have any dependency issues here? 

            Pipe {
              val cEleNewReg = Reg[T](0)
              val hEleNewReg = Reg[T](0)

              Pipe {
                val cEle = c(iLayer, iFeature)
                val cEleNew = cEle * f.value + i.value * j.value
                cEleNewReg := cEleNew
              }
              
              Pipe {
                c(iLayer, iFeature) = cEleNewReg.value
                h(iLayer, iFeature) = hEleNewReg.value
                x(iStep, iFeature) = activation(cEleNewReg.value) + o.value
              }
            }
          }
        }
      }

      io.out(0::nSteps, 0::nFeatures par stPar) store x
    }

    getMatrix(io.out)
  }

  @virtualize
  def setupLSTMIOs(): LSTMIOs = {
    val xInit = getDRAMCSV2D("x_cat", nSteps, nFeatures)
    val cInit = getDRAMCSV2D("c_cat", nLayers, nHiddenUnits)
    val hInit = getDRAMCSV2D("h_cat", nLayers, nHiddenUnits)

    val biInit = getDRAMCSV2D("bi_cat", nLayers, nHiddenUnits)
    val bjInit = getDRAMCSV2D("bj_cat", nLayers, nHiddenUnits)
    val bfInit = getDRAMCSV2D("bf_cat", nLayers, nHiddenUnits)
    val boInit = getDRAMCSV2D("bo_cat", nLayers, nHiddenUnits)

    val wxiInit = getDRAMCSV3D("wxi_cat", nLayers, nHiddenUnits, nReduce)
    val wxjInit = getDRAMCSV3D("wxj_cat", nLayers, nHiddenUnits, nReduce)
    val wxfInit = getDRAMCSV3D("wxf_cat", nLayers, nHiddenUnits, nReduce)
    val wxoInit = getDRAMCSV3D("wxo_cat", nLayers, nHiddenUnits, nReduce)

    val whiInit = getDRAMCSV3D("whi_cat", nLayers, nHiddenUnits, nReduce)
    val whjInit = getDRAMCSV3D("whj_cat", nLayers, nHiddenUnits, nReduce)
    val whfInit = getDRAMCSV3D("whf_cat", nLayers, nHiddenUnits, nReduce)
    val whoInit = getDRAMCSV3D("who_cat", nLayers, nHiddenUnits, nReduce)

    val out = DRAM[T](nSteps, nFeatures)
    val io = LSTMIOs(xInit, cInit, hInit,
      biInit, bjInit, bfInit, boInit,
      wxiInit, wxjInit, wxfInit, wxoInit,
      whiInit, whjInit, whfInit, whoInit,
      out)
    io
  }

  @virtualize
  def setupTestLSTMIOs(): LSTMIOs = {
    val xInit = genDRAMData2D[T](nSteps, nFeatures)
    val cInit = genDRAMData2D[T](nLayers, nHiddenUnits)
    val hInit = genDRAMData2D[T](nLayers, nHiddenUnits)

    val biInit = genDRAMData2D[T](nLayers, nHiddenUnits)
    val bjInit = genDRAMData2D[T](nLayers, nHiddenUnits)
    val bfInit = genDRAMData2D[T](nLayers, nHiddenUnits)
    val boInit = genDRAMData2D[T](nLayers, nHiddenUnits)

    val wxiInit = genDRAMData3D[T](nLayers, nHiddenUnits, nReduce)
    val wxjInit = genDRAMData3D[T](nLayers, nHiddenUnits, nReduce)
    val wxfInit = genDRAMData3D[T](nLayers, nHiddenUnits, nReduce)
    val wxoInit = genDRAMData3D[T](nLayers, nHiddenUnits, nReduce)

    val whiInit = genDRAMData3D[T](nLayers, nHiddenUnits, nReduce)
    val whjInit = genDRAMData3D[T](nLayers, nHiddenUnits, nReduce)
    val whfInit = genDRAMData3D[T](nLayers, nHiddenUnits, nReduce)
    val whoInit = genDRAMData3D[T](nLayers, nHiddenUnits, nReduce)

    val out = DRAM[T](nSteps, nFeatures)
    val io = LSTMIOs(xInit, cInit, hInit,
      biInit, bjInit, bfInit, boInit,
      wxiInit, wxjInit, wxfInit, wxoInit,
      whiInit, whjInit, whfInit, whoInit,
      out)
    io
  }

  @virtualize 
  def main(): Unit = {
    // val io = setupLSTMIOs()
    val io = setupTestLSTMIOs() 
    val result = lstmCore(io)
    printMatrix(result, "result = ")
    // Can do softmax here if needed
  }
}
