import spatial.dsl._
import virtualized._


// The linear layer does np.concatenate([a, hidden], axis=1).dot(kernel) + bias(broadcasted)
// bias needs to broadcast over each batch
//               feature_size   hidden_size     4*hidden_size           4 * hidden_size
//              +--------------+-----+       +-----------------+      +-----------------+
//              |              |     |       |  |---| |   |    | +    +-----------------+
//              |              |     |       |dp|   | |   |    |
//              |              |     |       |  |---| |   |    |
//  batch_size  |      x       |hidden   *   |    dm  |   |    |                              => targetSRAM
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

trait CellsManualPar extends SpatialApp {
  val debug: scala.Boolean = false

  @virtualize
  def sigmoid_synth[T:Type:Num](p: T) = {
    (tanh_synth[T](p / 2.to[T]) + 1.to[T]) / 2.to[T]
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


  case class GateParams[T:Type:Num](tileKernelRows: Int, tileKernelCols: Int, kernel: DRAM2[T], bias: SRAM2[T], targetSRAM: SRAM2[T], biasOffset: Int)

  // Try def
  case class CellParam[T:Type:Num](paramPath: java.lang.String, cellName: java.lang.String,
                  batch_size: Int, feature_size: Int, hidden_size: Int,
                  dp: Int, dm: Int, isFusedInputs: scala.Boolean = false,
                  initPar: Int = 16, outerPar: Int = 1, midPar: Int = 1, innerPar: Int = 1, elePar: Int = 1) (implicit convert : Cast[String, T]) {
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

    val kI = DRAM[T](kernelReduceSize, hidden_size)
    val kJ = DRAM[T](kernelReduceSize, hidden_size)
    val kF = DRAM[T](kernelReduceSize, hidden_size)
    val kO = DRAM[T](kernelReduceSize, hidden_size)
    val kernel = DRAM[T](kernelReduceSize, hidden_size * 4)

    setMem(kI, loadCSV2D[T](paramPath + cellName + "_kI.csv", ",", "\n"))
    setMem(kJ, loadCSV2D[T](paramPath + cellName + "_kJ.csv", ",", "\n"))
    setMem(kF, loadCSV2D[T](paramPath + cellName + "_kF.csv", ",", "\n"))
    setMem(kO, loadCSV2D[T](paramPath + cellName + "_kO.csv", ",", "\n"))
    setMem(kernel, loadCSV2D[T](paramPath + cellName + "_kernel.csv", ",", "\n"))
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
    val initPar = cellParam.initPar
    val elePar = cellParam.elePar


    // init class params
    val h = SRAM[T](batch_size, hidden_size)
    val c = SRAM[T](batch_size, hidden_size)
    val (sigI, tanhJ, sigF, sigO) = (SRAM[T](batch_size, hidden_size), SRAM[T](batch_size, hidden_size),
                                    SRAM[T](batch_size, hidden_size), SRAM[T](batch_size, hidden_size))
    val bias = SRAM[T](batch_size, hidden_size * 4)
    val paramsI = GateParams[T](dp, dm, kI, bias, sigI, 0)
    val paramsJ = GateParams[T](dp, dm, kJ, bias, tanhJ, hidden_size)
    val paramsF = GateParams[T](dp, dm, kF, bias, sigF, hidden_size * 2)
    val paramsO = GateParams[T](dp, dm, kO, bias, sigO, hidden_size * 3)

    if (debug) {
      println(s"=== $cellParam.cellName ===")
      println(s"h: $batch_size, $hidden_size")
      println(s"c: $batch_size, $hidden_size")
      println(s"kernel_gate: $cellParam.kernelReduceSize, $cellParam.hidden_size")
      println(s"bias_gate: $cellParam.linear_output_size")
      println("")
    }


    @virtualize
    def init() {
      bias load bDRAM(0::batch_size, 0::linear_output_size)
      Foreach (batch_size by 1, hidden_size by 1 par initPar) { (i,j) =>
        Parallel {
          c(i,j) = 0.to[T]
          h(i,j) = 0.to[T]
        }
      }
    }


    // Use this function to flush SRAM c and h into cDRAM and hDRAM
  @virtualize
    def flush() {
      Parallel {
        cDRAM(0::batch_size, 0::hidden_size) store c
        hDRAM(0::batch_size, 0::hidden_size) store h
      }
    }

    // A basic gate implementation
    @virtualize
    def basicGate(x: SRAM2[T], h: SRAM2[T], gateParams: GateParams[T], forgetBias: Int, activation: T => T) {
      val kernel = gateParams.kernel
      val bias = gateParams.bias
      val biasOffset = gateParams.biasOffset
      val targetSRAM = gateParams.targetSRAM

      Foreach (kernelReduceSize by dp, hidden_size by dm par outerPar) { (k,j) =>
        val tileKernel = SRAM[T](gateParams.tileKernelRows, gateParams.tileKernelCols)
        tileKernel load kernel(k::k+dp, j::j+dm)
        Foreach (batch_size by 1 par midPar) { i =>
          Foreach (dm by 1 par innerPar) { jj =>
            val prod = Reduce(Reg[T]) (dp by 1) { kk =>
              val reduce_size_offset = k + kk
              if (reduce_size_offset < feature_size) {
                x(i, reduce_size_offset) * tileKernel(kk, jj)
              } else {
                h(i, reduce_size_offset - feature_size) * tileKernel(kk, jj)
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

    def forward(isLastIter: Boolean = false) // abstract, must implement
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
        h(i,j) = tanh_synth[T](new_c) * sigO(i,j) // simulation
        c(i,j) = new_c
      }
    }
  }


  // This cell adds input to the outputs. It is optimized based on BasicLSTMCell
  // Warning: Residual network requires that feature_size and hidden_size are the same
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
          h(i,j) = tanh_synth[T](new_c) * sigO(i,j) + x(i,j)
        } else {
          h(i,j) = tanh_synth[T](new_c) * sigO(i,j)
        }
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
      val targetSRAM = gateParams.targetSRAM
      val xxFeatureSize = feature_size * 2

      Foreach (kernelReduceSize by dp, hidden_size by dm par outerPar) { (k,j) =>
        val tileKernel = SRAM[T](gateParams.tileKernelRows, gateParams.tileKernelCols)
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
        h(i,j) = tanh_synth[T](new_c) * sigO(i,j) // simulation
        c(i,j) = new_c
      }
    }
  }
}
