import spatial.dsl._
import virtualized._


// The linear layer does np.concatenate([a, hidden], axis=1).dot(kernel) + bias(broadcasted)
// bias needs to broadcast over each batch
//               feature_size   hidden_size     4*hidden_size           4 * hidden_size
//              +--------------+-----+       +-----------------+      +-----------------+
//              |              |     |       |  |---| |   |    | +    +-----------------+
//              |              |     |       |dp|   | |   |    |
//              |              |     |       |  |---| |   |    |
//  batch_size  |      x       |hidden   *   |    dm  |   |    |
//              |              |     |       |    |kernel |    |
//              |              |     |       |feature_size|    |
//              |              |     |       | + hidden_size   |
//              +--------------+-----+       |    |   |   |    |
//                                           |    |   |   |    |
//                                           | i  | j | f | o  |
//                                           |    |   |   |    |
//                                           |    |   |   |    |
//                                           |    |   |   |    |
//                                           +----|---|---|----+

// mem and hidden states are always in SRAMs.
trait CellImps extends SpatialApp with Activations {
  val debug: scala.Boolean = false

  @virtualize
  def sigmoid_synth[T:Type:Num](p: T) = {
    // sig_pw[T](p)
    (tanh_synth[T](p / 2.to[T]) + 1.to[T]) / 2.to[T]
    // sigmoidSim(p)
  }


  @virtualize
  def tanh_synth[T:Type:Num](p: T) = {
    val absp = abs(p)
    val absre = if (absp > (2.5).to[T]) {
      1.to[T]
    } else if (((0.5).to[T] < absp) && (absp <= (2.5).to[T])) {
      // bug: if replace div4 with the shifted result directly, spatial would infer the type of absp >> 2 as FixPt[TRUE, 0, 3]
      val div4 = absp >> 2
      val div4Offset = div4 + (0.375).to[T]
      div4Offset
    } else {
      absp
    }

    val mabre = 0.to[T] - absre
    val re = mux((p < 0.to[T]), mabre, absre)
    re
  }


  case class GateParams[T:Type:Num](tileKernel: SRAM2[T], kernel: DRAM2[T], bias: SRAM2[T], targetSRAM: SRAM2[T], biasOffset: Int)

  // Try def
  case class CellParam[T:Type:Num](paramPath: java.lang.String, cellName: java.lang.String,
                  batch_size: Int, feature_size: Int, hidden_size: Int,
                  dp: Int, dm: Int, isFusedInputs: scala.Boolean = false,
                  outerPar: Int = 1, midPar: Int = 1, innerPar: Int = 1, elePar: Int = 1) (implicit convert : Cast[String, T]) {
    val linear_output_size = 4 * hidden_size
    val kernelReduceSize = if (isFusedInputs) {
      hidden_size + feature_size * 2
    } else {
      hidden_size + feature_size
    }
    val cDRAM = DRAM[T](batch_size, hidden_size)
    val bDRAM = DRAM[T](batch_size, linear_output_size)
    val hDRAM = DRAM[T](batch_size, hidden_size)

    println(s"cell param: cellName = $cellName, linear_output_size = $linear_output_size, kernelReduceSize = $kernelReduceSize, hidden_size = $hidden_size")

    setMem(cDRAM, loadCSV2D[T](paramPath + cellName + "_c.csv", ",", "\n"))
    setMem(hDRAM, loadCSV2D[T](paramPath + cellName + "_h.csv", ",", "\n"))
    setMem(bDRAM, loadCSV2D[T](paramPath + cellName + "_bias.csv", ","))

    // TODO: since we don't have a view of DRAM, we need to split the data outside of Accel
    val kI = DRAM[T](kernelReduceSize, hidden_size)
    val kJ = DRAM[T](kernelReduceSize, hidden_size)
    val kF = DRAM[T](kernelReduceSize, hidden_size)
    val kO = DRAM[T](kernelReduceSize, hidden_size)

    setMem(kI, loadCSV2D[T](paramPath + cellName + "_kI.csv", ",", "\n"))
    setMem(kJ, loadCSV2D[T](paramPath + cellName + "_kJ.csv", ",", "\n"))
    setMem(kF, loadCSV2D[T](paramPath + cellName + "_kF.csv", ",", "\n"))
    setMem(kO, loadCSV2D[T](paramPath + cellName + "_kO.csv", ",", "\n"))
  }


  // TODO: add init callbacks
  abstract class RNNCell[T <: MetaAny[T] :Type:Num ](cellParam: CellParam[T], val x: SRAM2[T]) {
    // Defines if this cell is a residual cell
    val isResidual = false

    // set params from cell
    val batch_size = cellParam.batch_size
    val feature_size = cellParam.feature_size
    val hidden_size = cellParam.hidden_size
    val linear_output_size = cellParam.hidden_size * 4
    val dp = cellParam.dp
    val dm = cellParam.dm
    val kI = cellParam.kI
    val kJ = cellParam.kJ
    val kF = cellParam.kF
    val kO = cellParam.kO
    val bDRAM = cellParam.bDRAM
    val cDRAM = cellParam.cDRAM
    val hDRAM = cellParam.hDRAM
    val kernelReduceSize = cellParam.kernelReduceSize

    // Par factors
    val outerPar = cellParam.outerPar
    val midPar = cellParam.midPar
    val innerPar = cellParam.innerPar
    val elePar = cellParam.elePar

    // each cell requires this many SRAMs:
    // batch_size * hidden_size * 6 + hidden_size * 4 + dp * dm * 4


    // init class params
    val h = SRAM[T](batch_size, hidden_size)
    val c = SRAM[T](batch_size, hidden_size)
    val (sigI, tanhJ, sigF, sigO) = (SRAM[T](batch_size, hidden_size), SRAM[T](batch_size, hidden_size),
                                    SRAM[T](batch_size, hidden_size), SRAM[T](batch_size, hidden_size))
    val bias = SRAM[T](batch_size, hidden_size * 4)
    val (tileI, tileJ, tileF, tileO) = (SRAM[T](dp, dm), SRAM[T](dp, dm),
                                        SRAM[T](dp, dm), SRAM[T](dp, dm))
    val paramsI = GateParams[T](tileI, kI, bias, sigI, 0)
    val paramsJ = GateParams[T](tileJ, kJ, bias, tanhJ, hidden_size)
    val paramsF = GateParams[T](tileF, kF, bias, sigF, hidden_size * 2)
    val paramsO = GateParams[T](tileO, kO, bias, sigO, hidden_size * 3)

    if (debug) {
      println(s"=== $cellParam.cellName ===")
      println(s"h: $batch_size, $hidden_size")
      println(s"c: $batch_size, $hidden_size")
      println(s"kernel_gate: $cellParam.kernelReduceSize, $cellParam.hidden_size")
      println(s"bias_gate: $cellParam.linear_output_size")
      println("")
    }


    // All the weight inits go here
    @virtualize
    def init() {
      Parallel {
        // bI load bDRAM(0::1, 0::hidden_size)
        // bJ load bDRAM(0::1, hidden_size::2*hidden_size)
        // bF load bDRAM(0::1, 2*hidden_size::3*hidden_size)
        // bO load bDRAM(0::1, 3*hidden_size::linear_output_size)
        bias load bDRAM(0::batch_size, 0::linear_output_size)
        c load cDRAM(0::batch_size, 0::hidden_size)
        h load hDRAM(0::batch_size, 0::hidden_size)
      }
    }

    // Use this function to flush SRAM c and h into cDRAM and hDRAM
    @virtualize
    def flush() {
      Parallel {
        hDRAM(0::batch_size, 0::hidden_size) store h
        cDRAM(0::batch_size, 0::hidden_size) store c
      }
    }

    // TODO: Due to the nature of NMT, size of x is approx. the same as h. Can par these two.
    // A basic gate implementation
    @virtualize
    def basicGate(x: SRAM2[T], h: SRAM2[T], gateParams: GateParams[T], forgetBias: Int, activation: T => T) {
      val kernel = gateParams.kernel
      val bias = gateParams.bias
      val biasOffset = gateParams.biasOffset
      val tileKernel = gateParams.tileKernel
      val targetSRAM = gateParams.targetSRAM

      Foreach (kernelReduceSize by dp, hidden_size by dm par outerPar) { (k,j) =>
        tileKernel load kernel(k::k+dp, j::j+dm)
        Foreach (batch_size by 1 par midPar) { i =>
          Foreach (dm by 1 par innerPar) { jj =>
            val prod = Reduce(Reg[T]) (dp by 1) { kk =>
              val reduce_size_offset = k + kk
              // Rewrite in mux
              //if (reduce_size_offset < feature_size) {
                //x(i, reduce_size_offset) * tileKernel(kk, jj)
              //} else {
                //h(i, reduce_size_offset - feature_size) * tileKernel(kk, jj)
              //}
              val tempA = x(i, reduce_size_offset) * tileKernel(kk, jj)
              val tempB = h(i, reduce_size_offset - feature_size) * tileKernel(kk, jj)
              mux(reduce_size_offset < feature_size, tempA, tempB)
            } {_+_}

            val colOffset = j + jj
            val ele = prod.value + mux(k == 0, bias(0, colOffset + biasOffset), targetSRAM(i, colOffset))
            // TODO: is it possible to reduce the value based on different gates?
            //
            // Rewrite in mux
            //if (k >= kernelReduceSize - dp) {
              //targetSRAM(i, colOffset) = activation(ele + forgetBias.to[T])
            //} else {
              //targetSRAM(i, colOffset) = ele
            //}
            targetSRAM(i, colOffset) = mux(k >= kernelReduceSize - dp, activation(ele + forgetBias), ele)
          }
        }
      }
    }

    def forward(isLastIter: Boolean = false) // abstract, must implement
  }


  // This cell adds input to the outputs. It is optimized based on BasicLSTMCell
  // Warning: Residual network requires feature_size and hidden_size are the same
  case class ResidualBasicLSTMCell[T <: MetaAny[T] :Type:Num ](cellParam: CellParam[T], override val x: SRAM2[T]) extends RNNCell[T](cellParam, x) {

    override val isResidual = true

    @virtualize
    def forward(isLastIter: Boolean = false) {
      Parallel {
        basicGate(x, h, paramsI, 0, sigmoid_synth[T]) // i
        basicGate(x, h, paramsJ, 0, tanh_synth[T]) // j
        basicGate(x, h, paramsF, 1, sigmoid_synth[T]) // f
        basicGate(x, h, paramsO, 0, sigmoid_synth[T]) // o
      }

      Foreach (batch_size by 1, hidden_size by 1 par elePar) { (i,j) =>
        val new_c = c(i,j) * sigF(i,j) + sigI(i,j) * tanhJ(i,j)
        if (isLastIter) {
          h(i,j) = tanh_synth[T](new_c) * sigO(i,j) + x(i,j) // TODO: simulation
        } else {
          h(i,j) = tanh_synth[T](new_c) * sigO(i,j) // TODO: simulation
        }
        c(i,j) = new_c
      }
    }
  }


  // Bias are defined using SRAMs. The bias SRAMs will be loaded outside of the cell.
  // Kernels are in DRAMs. Therefore need to pass the DRAM2 pointers here.
  case class BasicLSTMCell[T <: MetaAny[T] :Type:Num](cellParam: CellParam[T], override val x: SRAM2[T]) extends RNNCell[T](cellParam, x) {

    @virtualize
    def forward(isLastIter: Boolean = false) {
      Parallel {
        basicGate(x, h, paramsI, 0, sigmoid_synth[T]) // i
        basicGate(x, h, paramsJ, 0, tanh_synth[T]) // j
        basicGate(x, h, paramsF, 1, sigmoid_synth[T]) // f
        basicGate(x, h, paramsO, 0, sigmoid_synth[T]) // o
      }

      Foreach (batch_size by 1, hidden_size by 1 par elePar) { (i,j) =>
        val new_c = c(i,j) * sigF(i,j) + sigI(i,j) * tanhJ(i,j)
        // h(i,j) = sigmoid_(new_c) * sigO(i,j)
        h(i,j) = tanh_synth[T](new_c) * sigO(i,j) // simulation
        c(i,j) = new_c
      }
    }
  }


  // TODO: right now Spatial doesn't support concatenating 2 SRAMs. Therefore need a way to fuse the reads.
  // concat(x, x1) gives the full inputs
  // Comment: on the input side, this time feature_size = 2 * hidden_size
  case class FusedReadsBasicLSTMCell[T <: MetaAny[T] :Type:Num](cellParam: CellParam[T], override val x: SRAM2[T], x1: SRAM2[T]) extends RNNCell[T](cellParam, x) {

    @virtualize
    def basicGate(x0: SRAM2[T], x1: SRAM2[T], h: SRAM2[T], gateParams: GateParams[T], forgetBias: Int, activation: T => T) {
      val kernel = gateParams.kernel
      val bias = gateParams.bias
      val biasOffset = gateParams.biasOffset
      val tileKernel = gateParams.tileKernel
      val targetSRAM = gateParams.targetSRAM
      val xxFeatureSize = feature_size * 2

      Foreach (kernelReduceSize by dp, hidden_size by dm par outerPar) { (k,j) =>
        tileKernel load kernel(k::k+dp, j::j+dm)
        Foreach (batch_size by 1 par midPar) { i =>
          Foreach (dm by 1 par innerPar) { jj =>
            val prod = Reduce(Reg[T]) (dp by 1) { kk =>
              val reduce_size_offset = k + kk
              if (reduce_size_offset < feature_size) {
                x0(i, reduce_size_offset) * tileKernel(kk, jj)
              } else if (feature_size <= reduce_size_offset && reduce_size_offset < xxFeatureSize) {
                x1(i, reduce_size_offset - feature_size) * tileKernel(kk, jj)
              } else {
                h(i, reduce_size_offset - xxFeatureSize) * tileKernel(kk, jj)
              }
            } {_+_}

            val colOffset = j + jj
            val ele = prod.value + mux(k == 0, bias(0, colOffset + biasOffset), targetSRAM(i, colOffset))
            // TODO: is it possible to reduce the value based on different gates?
            if (k >= kernelReduceSize - dp) {
              targetSRAM(i, colOffset) = activation(ele + forgetBias.to[T])
            } else {
              targetSRAM(i, colOffset) = ele
            }
          }
        }
      }
    }

    @virtualize
    def forward(isLastIter: Boolean = false) {
      Parallel {
        basicGate(x, x1, h, paramsI, 0, sigmoid_synth[T]) // i
        basicGate(x, x1, h, paramsJ, 0, tanh_synth[T]) // j
        basicGate(x, x1, h, paramsF, 1, sigmoid_synth[T]) // f
        basicGate(x, x1, h, paramsO, 0, sigmoid_synth[T]) // o
      }

      Foreach (batch_size by 1, hidden_size by 1 par elePar) { (i,j) =>
        val new_c = c(i,j) * sigF(i,j) + sigI(i,j) * tanhJ(i,j)
        // h(i,j) = sigmoid_(new_c) * sigO(i,j)
        h(i,j) = tanh_synth[T](new_c) * sigO(i,j) // simulation
        c(i,j) = new_c
      }
    }
  }
}
