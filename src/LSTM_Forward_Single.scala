import spatial.dsl._
import org.virtualized._

/*
 * LSTM expression:
 *
 * Reference: CS224N, Stanford
 *
 * Dimensions:
 * x_t     \in d by N         : input word vector at time t
 * W       \in Dh by d       : weights matrices to condition x_t
 * U       \in Dh by Dh     : weights matrix to condition the previous hidden state
 * i_t     \in Dh by N       : weights matrix of the input gate
 * f_t     \in Dh by N       : weights matrix of the forget gate
 * o_t     \in Dh by N       : weights matrix of the output gate
 * h_{t-1} \in Dh by N       : weights of the last hidden state
 * h_t     \in Dh by N           : weights of the current hidden state
 *
 */

/*
 * Lookup table for LSTM sigmoid
 * Reference: https://arxiv.org/pdf/1612.00694.pdf
 * Sigmoid input: min: -64, max: 64, sampling point = 2048
 */

/*
 * Foreach loop: store things in local mems instead of in DRAM
 */

/*
 * TODO:
 */

object LSTM_Forward_Single extends SpatialApp {
  import IR._

  // TODO: need to rethink of precision
  type X = FixPt[TRUE,_32,_32]

  @virtualize
  def GateForward[T:Type:Num] (
    /* Input gate */
    W_in: Matrix[T], U_in: Matrix[T],
    /* Forget gate */
    W_forget: Matrix[T], U_forget: Matrix[T],
    /* Output gate */
    W_output: Matrix[T], U_output: Matrix[T],
    /* New memory gate */
    W_new_mem: Matrix[T], U_new_mem: Matrix[T],
    /* Old memory gate */
    W_c_t_1: Matrix[T],
    /* Inputs */
    x: Matrix[T], h: Matrix[T],
    /* Sizes */
    mm: Int, nn: Int, N_classes: Int, N_steps: Int
  ) = {
    val Dh = ArgIn[Int]
    val d = ArgIn[Int]
    val N = ArgIn[Int]
    val E = ArgIn[Int]

    setArg(Dh, mm)
    setArg(d, nn)
    setArg(N, N_classes)
    setArg(E, N_steps)

    // Param weights matrices
    val xt = DRAM[T](d, N)

    // DRAM hidden and memory
    val ht = DRAM[T](Dh, N)
    val ct = DRAM[T](Dh, N)

    val Wi = DRAM[T](Dh, d)
    val Ui = DRAM[T](Dh, Dh)
    val Wf = DRAM[T](Dh, d)
    val Uf = DRAM[T](Dh, Dh)
    val Wo = DRAM[T](Dh, d)
    val Uo = DRAM[T](Dh, Dh)
    val Wc = DRAM[T](Dh, d)
    val Uc = DRAM[T](Dh, Dh)

    // Result matrix
    // val result = DRAM[T](Dh, N)
    // Result 1: Next memory
    val next_mem = DRAM[T](Dh, N)
    // Result 2: Next hidden state
    val next_hidden_state = DRAM[T](Dh, N)

    val b_N = 16                          // Stepsize for going through N classes
    val b_Dh = 16                         // Stepsize for going through hidden size
    val b_Wi_d = 16                       // First matmult, col step size
    val b_Ui_Dh = 16                      // Second matmult, col step size

    // For referencing the range of LUTs
    val lo = 32.to[T]
    val revPrec = 2.to[T]
    val totalSize = 128

    setMem(xt, x)

    // Input gate
    setMem(Wi, W_in)
    setMem(Ui, U_in)
    setMem(Wf, W_forget)
    setMem(Uf, U_forget)
    setMem(Wo, W_output)
    setMem(Uo, U_output)
    setMem(Wc, W_new_mem)
    setMem(Uc, U_new_mem)

    setMem(ct, W_c_t_1)
    setMem(ht, h)

    Accel {
      /*
       * LUT table for sigmoid function:
       * Input lower bound: -32.0
       * Input upper bound: 32.0
       * Output lower bound: 0
       * Output upper bound: 1
       * Number of samples: 128
       * Precision: 64 / 128 = 0.5
       */
      val sigmoidLUT = LUT.fromFile[T](totalSize)("/home/tianzhao/spatial-LSTM/spatial-lang/apps/src/__128_sigmoidLUT.csv")
      def sigmoid(p: T) = { sigmoidLUT(((p + lo) * revPrec).to[Int]) }

      val tanhLUT = LUT.fromFile[T](totalSize) ("/home/tianzhao/spatial-LSTM/spatial-lang/apps/src/__128_tanhLUT.csv")
      def tanh(p: T) = { tanhLUT(((p + lo) * revPrec).to[Int]) }

      // TODO: use the dims from @api in SRAMs.scala
      def tileInnerMatMult(tile_re: SRAM2[T], tile_aT: SRAM2[T], tile_x: SRAM2[T], nRow: Int, nEdge: Int, nCol: Int, nInitIdx: Int) = {
          Foreach(nRow by 1, nCol by 1) { (ii,jj) =>
            val prod_aTx = Reduce(Reg[T])(nEdge by 1){ kk =>
              tile_aT(ii,kk) * tile_x(kk,jj)
            }{_+_}

            // TODO: if the initial values are set properly, is this mux still needed?
            val prev_aTx = mux(k == 0, 0.to[T], tile_re(ii,jj))
            tile_re(ii, jj) = prev_aTx + prod_aTx.value
          }
      }

      def tileGateLinearMultInput(tile_re: SRAM2[T], aT: DRAM2[T], x: DRAM2[T], i: Int, j: Int, tp: Int, tpp: Int, mm: Int, nn: Int) = {
        Foreach(tp by tpp) { k =>
          val tile_aT = SRAM[T](mm, tpp)
          val tile_x = SRAM[T](tpp, nn)
          Parallel {
            tile_aT load aT(i::i+mm, k::k+tpp)
            tile_x load x(k::k+tpp, j::j+nn)
          }

          tileInnerMatMult(tile_re, tile_aT, tile_x, mm, tpp, nn, k)
        }
      }

      def tileGateLinearMultHidden(tile_re: SRAM2[T], uT: DRAM2[T], tile_ht: SRAM2[T], tile_ht_nRow: Int, tile_ht_nCol: Int, i: Int, j: Int) = {
        Pipe {
          val tile_uT = SRAM[T](tile_ht_nRow, tile_ht_nCol)
          tile_uT load uT(i::i+tile_ht_nRow, j::j+tile_ht_nCol)
          tileInnerMatMult(tile_re, tile_uT, tile_ht, tile_ht_nRow, 
        }
      }

      /*
       * The major function that describes the design
       * where Wi, x, Ui, h are DRAM locations
       * Assuming that the input matrices are transposed already
       * It is fair to assume that the hidden state and memory state matrix is small
       * enough to be hosted on the SRAM
       * @param Wi: weights of input gate
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
        /* Gate weights */
        Wi: DRAM2[T], Ui: DRAM2[T], Wf: DRAM2[T], Uf: DRAM2[T], Wo: DRAM2[T], Uo: DRAM2[T], Wc: DRAM2[T], Uc: DRAM2[T],
        /* memory inputs */
        tile_ct: SRAM2[T], tile_ht: SRAM2[T],
        /* word vectors */
        xt: DRAM2[T],
        /* Sizes */
        m: Int, n: Int, p: Int, q: Int, mm: Int, nn: Int, pp: Int, qq: Int
      ) = {
        Foreach(m by mm, n by nn) { (i,j) =>
          // Test result
          val reg_ct = Reg[T](0.to[T])
          val tile_WiTx = SRAM[T](mm, nn)
          val tile_UiTh = SRAM[T](mm, nn)
          val tile_WfTx = SRAM[T](mm, nn)
          val tile_UfTh = SRAM[T](mm, nn)
          val tile_WoTx = SRAM[T](mm, nn)
          val tile_UoTh = SRAM[T](mm, nn)
          val tile_WcTx = SRAM[T](mm, nn)
          val tile_UcTh = SRAM[T](mm, nn)

//          Parallel {
//            List((tile_WiTx, Wi, xt, p, pp), (tile_UiTh, Ui, h, q, qq),
//                 (tile_WfTx, Wf, xt, p, pp), (tile_UfTh, Uf, h, q, qq),
//                 (tile_WoTx, Wo, xt, p, pp), (tile_UoTh, Uo, h, q, qq),
//                 (tile_WcTx, Wc, xt, p, pp), (tile_UcTh, Uc, h, q, qq)
//                 ).foreach { case (tile, w1, x1, len, step) => tileBatchMult(tile, w1, x1, i, j, p, pp, mm, nn) }
//          }

          Foreach(mm by 1, nn by 1) { (ii, jj) =>
            // TODO: can we parallize these three lines?
            // c_t = f_t \times c_{t-1} + i_t \times c_tl_t
//            reg_ct := sigmoid(tile_WfTx(ii, jj) + tile_UfTh(ii, jj)) * tile_Wc_t_1(ii, jj) +
//                             sigmoid(tile_WiTx(ii, jj) + tile_UiTh(ii, jj)) * tanh(tile_WcTx(ii, jj) + tile_UcTh(ii, jj))
//            tile_new_mem(ii, jj) = reg_ct.value
//
//            // h_t = o_t \times tanh(c_t)
//            tile_new_hidden(ii, jj) = sigmoid(tile_WoTx(ii, jj) + tile_UoTh(ii, jj)) * tanh(reg_ct.value)
//

//              tile_new_mem(ii, jj) = tile_WfTx(ii, jj) + tile_UfTh(ii, jj)
//              tile_new_hidden(ii, jj) = tile_WiTx(ii, jj) + tile_UiTh(ii, jj)
//
            // TODO:  is a pipe needed here?
            reg_ct := (tile_WfTx(ii, jj) + tile_UfTh(ii, jj)) * tile_ct(ii, jj) +
                             (tile_WiTx(ii, jj) + tile_UiTh(ii, jj)) * (tile_WcTx(ii, jj) + tile_UcTh(ii, jj))
            tile_ct(ii, jj) = reg_ct.value
            tile_ht(ii, jj) = (tile_WoTx(ii, jj) + tile_UoTh(ii, jj)) * (reg_ct.value)
          }
        }
      }

      val tile_ct = SRAM[T](Dh, N)
      val tile_ht = SRAM[T](Dh, N)
      Pipe {
        Parallel {
          tile_ct load ct(0::Dh, 0::N)
          tile_ht load ht(0::Dh, 0::N)
        }
      }

      Sequential.Foreach(E by 1) { e =>
        forward (Wi, Ui, Wf, Uf, Wo, Uo, Wc, Uc,
                  tile_ct, tile_ht, xt, Dh, N, d, Dh, b_Dh, b_N, b_Wi_d, b_Ui_Dh)
      }

      Pipe {
        Parallel {
          ct(0::Dh, 0::N) store tile_ct
          ht(0::Dh, 0::N) store tile_ht
        }
      }
    }

    (getMatrix(ct), getMatrix(ht))
  }

  @virtualize
  def main() = {
    val Dh = 64
    val d = 64
    val N = 32
    val N_steps = 4

    val W_i =     loadCSV2D[X]("/home/tianzhao/data/64_by_64_basic_eles.csv", ",", "\n")
    val U_i =     loadCSV2D[X]("/home/tianzhao/data/64_by_64_basic_eles.csv", ",", "\n")
    val W_f =     loadCSV2D[X]("/home/tianzhao/data/64_by_64_basic_eles.csv", ",", "\n")
    val U_f =     loadCSV2D[X]("/home/tianzhao/data/64_by_64_basic_eles.csv", ",", "\n")
    val W_o =     loadCSV2D[X]("/home/tianzhao/data/64_by_64_basic_eles.csv", ",", "\n")
    val U_o =     loadCSV2D[X]("/home/tianzhao/data/64_by_64_basic_eles.csv", ",", "\n")
    val W_c =     loadCSV2D[X]("/home/tianzhao/data/64_by_64_basic_eles.csv", ",", "\n")
    val U_c =     loadCSV2D[X]("/home/tianzhao/data/64_by_64_basic_eles.csv", ",", "\n")
    val x_t =     loadCSV2D[X]("/home/tianzhao/data/64_by_32_basic_eles.csv", ",", "\n")
    val h_t_1 =   loadCSV2D[X]("/home/tianzhao/data/64_by_32_basic_eles.csv", ",", "\n")
    val W_c_t_1 = loadCSV2D[X]("/home/tianzhao/data/64_by_32_basic_eles.csv", ",", "\n")

    val (nextMem, nextHidden) = GateForward (
      /* Weights */
      W_i, U_i, W_f, U_f, W_o, U_o, W_c, U_c, W_c_t_1,
      /* Inputs */
      x_t, h_t_1,
      /* Sizes */
      Dh, d, N, N_steps
    )

    writeCSV2D[X](nextMem, "/home/tianzhao/spatial-LSTM/spatial-lang/apps/results/LSTM_Forward_Single/nextMem.csv", ",", "\n")
    writeCSV2D[X](nextHidden, "/home/tianzhao/spatial-LSTM/spatial-lang/apps/results/LSTM_Forward_Single/nextHidden.csv", ",", "\n")
  }
}
