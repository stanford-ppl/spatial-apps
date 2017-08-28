import spatial.dsl._
import org.virtualized._

// TODO: 
// - Need to move the LSTM cell description here.
//  - multi stage LSTM
//  - combine every weights into a bigger matrix
// - LSTM Sparse.
//  - How sparse are these weight matrices? 
// - Need to figure out a model to describe it.
// - Codegen from pytorch.

object LSTMForward extends SpatialApp {
  val projectDir = "/Users/tianzhao/Developers/spatial/spatial-lang/"
  type X = FixPt[TRUE, _32, _32]
  
  @virtualize
  def GateForward[T:Type:Num] (
    W_in: Matrix[T], U_in: Matrix[T],
    W_forget: Matrix[T], U_forget: Matrix[T],
    W_output: Matrix[T], U_output: Matrix[T],
    W_new_mem: Matrix[T], U_new_mem: Matrix[T],
    W_c_t_1: Matrix[T],
    x: Matrix[T], h: Matrix[T],
    /* Sizes */
    mm: Int, nn: Int, N_classes: Int
  ) = {
    val D_h = ArgIn[Int]
    val d = ArgIn[Int]
    val N = ArgIn[Int]

    setArg(D_h, mm)
    setArg(d, nn)
    setArg(N, N_classes)

    // Param weights matrices
    val xt = DRAM[T](d, N)
    val h_t_1 = DRAM[T](D_h, N)

    // Input gate DRAM
    val Wi = DRAM[T](D_h, d)
    val Ui = DRAM[T](D_h, D_h)

    // Forget gate DRAM
    val Wf = DRAM[T](D_h, d)
    val Uf = DRAM[T](D_h, D_h)

    // Output gate DRAM
    val Wo = DRAM[T](D_h, d)
    val Uo = DRAM[T](D_h, D_h)

    // New memory gate
    val Wc = DRAM[T](D_h, d)
    val Uc = DRAM[T](D_h, D_h)

    // Old memory gate
    val Wc_t_1 = DRAM[T](D_h, N)

    // Result matrix
    // val result = DRAM[T](D_h, N)
    // Result 1: Next memory
    val next_mem = DRAM[T](D_h, N)
    // Result 2: Next hidden state
    val next_hidden_state = DRAM[T](D_h, N)

    val b_N = 16                  // Stepsize for going through N classes
    val b_Dh = 16                 // Stepsize for going through hidden size
    val b_Wi_d = 16               // First matmult, col step size
    val b_Ui_Dh = 16              // Second matmult, col step size

    // For referencing the range of LUTs
    val lo = 32.to[T]
    val revPrec = 2.to[T]
    val totalSize = 128

    setMem(xt, x)
    setMem(h_t_1, h)

    // Input gate
    setMem(Wi, W_in)
    setMem(Ui, U_in)

    // Forget gate
    setMem(Wf, W_forget)
    setMem(Uf, U_forget)

    // Output gate
    setMem(Wo, W_output)
    setMem(Uo, U_output)

    // New memory gate
    setMem(Wc, W_new_mem)
    setMem(Uc, U_new_mem)

    // Old memory gate
    setMem(Wc_t_1, W_c_t_1)

    Accel {
      // 128 elements, inputs: -32 to 32, outputs: 0 to 1
      val sigmoidLUT = LUT.fromFile[T](totalSize)(projectDir + "apps/src/__128_sigmoidLUT.csv")
      def sigmoid(p: T) = { mux(p > 32.to[T], 1.to[T], mux(p < -32.to[T], -1.to[T], sigmoidLUT(((p + lo) * revPrec).to[Int])))}

      // 128 elements, inputs: -32 to 32, outputs: -1 to 1
      val tanhLUT = LUT.fromFile[T](totalSize) (projectDir + "apps/src/__128_tanhLUT.csv")
      def tanh(p: T) = { mux(p > 32.to[T], 1.to[T], mux(p < -32.to[T], -1.to[T], tanhLUT(((p + lo) * revPrec).to[Int])))}

      def tileBatchMult(tile_re: SRAM2[T], aT: DRAM2[T], x: DRAM2[T], i: Int, j: Int, tp: Int, tpp: Int, mm: Int, nn: Int) = {
        Foreach(tp by tpp) { k =>
          val tile_aT = SRAM[T](mm, tpp)
          val tile_x = SRAM[T](tpp, nn)
          Parallel {
            tile_aT load aT(i::i+mm, k::k+tpp)
            tile_x load x(k::k+tpp, j::j+nn)
          }

          Foreach(mm by 1, nn by 1) { (ii,jj) =>
            val prod_aTx = Reduce(Reg[T])(tpp by 1){ kk =>
              tile_aT(ii,kk) * tile_x(kk,jj)
            }{_+_}

            val prev_aTx = mux(k == 0, 0.to[T], tile_re(ii,jj))
            tile_re(ii,jj) = prev_aTx + prod_aTx.value
          }
        }
      }

      /*
       * The major function that describes the design
       * where Wi, x, Ui, h are DRAM locations
       * Assuming that the input matrices are transposed already
       * @param Wi: DRAM matrix
       * @param x: DRAM matrix
       * @param Ui: DRAM matrix
       * @param h: DRAM matrix
       * @param m: row num of Wi, Ui
       * @param n: col num of x, h
       * @param p: col num of Wi  / row num of x
       * @param q: col num of Ui  / row num of h
       * @param mm: tile step size of Wi, Ui row
       * @param nn: tile step size of x, h col
       * @param pp: tile step size of Wi col / x row
       * @param qq: tile step size of Ui col / h row
       */
      def forward(
        /* Input gate */
        Wi: DRAM2[T], Ui: DRAM2[T],
        /* Forget gate */
        Wf: DRAM2[T], Uf: DRAM2[T],
        /* Output gate */
        Wo: DRAM2[T], Uo: DRAM2[T],
        /* New memory gate */
        Wc: DRAM2[T], Uc: DRAM2[T],
        /* Old memory gate */
        Wc_t_1: DRAM2[T],
        /* Inputs */
        x: DRAM2[T], h: DRAM2[T],
        /* Sizes */
        m: Int, n: Int, p: Int, q: Int, mm: Int, nn: Int, pp: Int, qq: Int
      ) = {
        Foreach(m by mm, n by nn) { (i,j) =>
          // Test result
          val reg_ct = Reg[T](0.to[T])
          // val tile_re = SRAM[T](mm, nn)

          // Meaningful inputs
          val tile_WiTx = SRAM[T](mm, nn)
          val tile_UiTh = SRAM[T](mm, nn)
          val tile_WfTx = SRAM[T](mm, nn)
          val tile_UfTh = SRAM[T](mm, nn)
          val tile_WoTx = SRAM[T](mm, nn)
          val tile_UoTh = SRAM[T](mm, nn)
          val tile_WcTx = SRAM[T](mm, nn)
          val tile_UcTh = SRAM[T](mm, nn)
          val tile_Wc_t_1 = SRAM[T](mm, nn)

          // Output result 1: new memory weights
          val tile_new_mem = SRAM[T](mm, nn)
          // Output result 2: new hidden state weights
          val tile_new_hidden = SRAM[T](mm, nn)

          Parallel {
            tileBatchMult(tile_WiTx, Wi, x, i, j, p, pp, mm, nn)
            tileBatchMult(tile_UiTh, Ui, h, i, j, q, qq, mm, nn)
            tileBatchMult(tile_WfTx, Wf, x, i, j, p, pp, mm, nn)
            tileBatchMult(tile_UfTh, Uf, h, i, j, q, qq, mm, nn)
            tileBatchMult(tile_WoTx, Wo, x, i, j, p, pp, mm, nn)
            tileBatchMult(tile_UoTh, Uo, h, i, j, q, qq, mm, nn)
            tileBatchMult(tile_WcTx, Wc, x, i, j, p, pp, mm, nn)
            tileBatchMult(tile_UcTh, Uc, h, i, j, q, qq, mm, nn)
            // TODO: reconsider where to add this tile load
            tile_Wc_t_1 load Wc_t_1(i::i+mm, j::j+nn)
          }

          // Calculating the current memory and the next hidden state needs to be put
          // into two different states
          // For the whole tile, reduce the three tiles and send it back to mem
          Foreach(mm by 1, nn by 1) { (ii, jj) =>
            //  i_t    = sigmoid(tile_WiTx(ii, jj) + tile_UiTh(ii, jj))
            //  f_t    = sigmoid(tile_WfTx(ii, jj) + tile_UfTh(ii, jj))
            //  o_t    = sigmoid(tile_WoTx(ii, jj) + tile_UoTh(ii, jj))
            //  c_tl_t = tanh(tile_WcTx(ii, jj) + tile_UcTh(ii, jj))

            // c_t = f_t \times c_{t-1} + i_t \times c_tl_t
            reg_ct := sigmoid(tile_WfTx(ii, jj) + tile_UfTh(ii, jj)) * tile_Wc_t_1(ii, jj) +
                             sigmoid(tile_WiTx(ii, jj) + tile_UiTh(ii, jj)) * tanh(tile_WcTx(ii, jj) + tile_UcTh(ii, jj))
            tile_new_mem(ii, jj) = reg_ct.value

            // h_t = o_t \times tanh(c_t)
            tile_new_hidden(ii, jj) = sigmoid(tile_WoTx(ii, jj) + tile_UoTh(ii, jj)) * tanh(reg_ct.value)
          }

          next_mem(i::i+mm, j::j+nn) store tile_new_mem
          next_hidden_state(i::i+mm, j::j+nn) store tile_new_hidden
        }
      }

      /* Main function */
      forward (
        Wi, Ui,
        Wf, Uf,
        Wo, Uo,
        Wc, Uc,
        Wc_t_1,
        /* Inputs */
        xt, h_t_1,
        D_h, N, d, D_h, b_Dh, b_N, b_Wi_d, b_Ui_Dh
      )
    }

    (getMatrix(next_mem), getMatrix(next_hidden_state))
    // TODO: how to get two pieces of memory?
    // TODO: It seems that the weights can all be combined into a continuous DRAM matrix....
    // Should try that!
  }

  @virtualize
  def main() = {
    val D_h = 64
    val d = 64
    val N = 32
    val data_64_64 = projectDir + "apps/data/bi-att-flow/64_by_64_eles.csv"
    val data_64_32 = projectDir + "apps/data/bi-att-flow/64_by_32_eles.csv"

    // TODO: Get a pretrained model and fetch out weights from one of the gates
    val W_i = loadCSV2D[X](data_64_64, ",", "\n")
    val U_i = loadCSV2D[X](data_64_64, ",", "\n")
    val W_f = loadCSV2D[X](data_64_64, ",", "\n")
    val U_f = loadCSV2D[X](data_64_64, ",", "\n")
    val W_o = loadCSV2D[X](data_64_64, ",", "\n")
    val U_o = loadCSV2D[X](data_64_64, ",", "\n")
    val W_c = loadCSV2D[X](data_64_64, ",", "\n")
    val U_c = loadCSV2D[X](data_64_64, ",", "\n")
    val x_t = loadCSV2D[X](data_64_32, ",", "\n")
    val h_t_1 = loadCSV2D[X](data_64_32, ",", "\n")
    val W_c_t_1 = loadCSV2D[X](data_64_32, ",", "\n")

    val (next_mem_re, next_hidden_re) = GateForward (
      W_i, U_i,
      W_f, U_f,
      W_o, U_o,
      W_c, U_c,
      W_c_t_1,
      x_t, h_t_1,
      D_h, d, N
    )

    writeCSV2D[X](next_mem_re, projectDir + "apps/results/LSTM_Forward_Single/ct.csv", ",", "\n")
    writeCSV2D[X](next_hidden_re, projectDir + "apps/results/LSTM_Forward_Single/ht.csv", ",", "\n")
    // printArray(gateResult, "LSTM cell yields: ")

    // Calculate gold
//    val gold = Array.tabulate(D_h) { i =>
//     val Wi_Row = W_i.slice(i, 64)
//     val Ui_Row = U_i.slice(i, 64)
//     val Wf_Row = W_f.slice(i, 64)
//     val Uf_Row = U_f.slice(i, 64)
//     val Wo_Row = W_o.slice(i, 64)
//     val Uo_Row = U_o.slice(i, 64)
//     val Wc_Row = W_c.slice(i, 64)
//     val Uc_Row = U_c.slice(i, 64)
//     Array.tabulate(N) { j =>
//       val xt_Col = x_t.map(row => row(j))
//       val ht1_Col = h_t_1.map(row => row(j))
//       Wi_Row.zip(xt_Col){_*_}.reduce{_+_} + Ui_Row.zip(ht1_Col){_*_}.reduce{_+_} +
//       Wf_Row.zip(xt_Col){_*_}.reduce{_+_} + Uf_Row.zip(ht1_Col){_*_}.reduce{_+_} +
//       Wo_Row.zip(xt_Col){_*_}.reduce{_+_} + Uo_Row.zip(ht1_Col){_*_}.reduce{_+_} +
//       Wc_Row.zip(xt_Col){_*_}.reduce{_+_} + Uc_Row.zip(ht1_Col){_*_}.reduce{_+_}
//     }
//    }.flatten
//
    // printArray(gold, "Gold result is: ")
  }
}