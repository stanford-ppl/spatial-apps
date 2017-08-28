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

//                                                                          +-----+
//                                                                          |     |
//                                                                          |     |
//                                                                          |     |
//                                                                          |     |
//                                                                          |     |
//                        +-----+                                          d|  x  |
//                        |     |                                           |     |
//                        |     |                                           |     |
//                     D_h| tile|                                           |     |
//                        |  h  |                                           |     |
//                        |     |                                           |     |
//                 D_h    |     |                            d              |     |
//          +-------------+-----+      +       +----------------------------+-----+
//          |             |  N                 |                            |  N
//          |             |                    |                            |
//          |             |                    |                            |
//          |     U       |D_h                 |             W              |D_h
//          |             |                    |                            |
//          |             |                    |                            |
//          |             |                    |                            |
//          +-------------+                    +----------------------------+
//
object LSTM_Forward extends SpatialApp {
  import IR._

  val Dh = 4
  val d = 4
  val N = 2
  val E = 4
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
    c_t_1: Matrix[T],
    /* Inputs */
    x: Matrix[T], h_t_1: Matrix[T]
  ) = {

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

    val next_mem = DRAM[T](Dh, N)
    val next_hidden_state = DRAM[T](Dh, N)

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

    setMem(ct, c_t_1)
    setMem(ht, h_t_1)

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

      def tileMult(tile_re: SRAM2[T], tile_aT: SRAM2[T], tile_x: SRAM2[T], nRow: Int, nEdge: Int, nCol: Int, k: Int) = {
        Foreach(nRow by 1, nCol by 1) { (ii,jj) =>
          val prod_aTx = Reduce(Reg[T])(nEdge by 1){ kk =>
            tile_aT(ii,kk) * tile_x(kk,jj)
          }{_+_}

          val prev_aTx = mux(k == 0, 0.to[T], tile_re(ii,jj))
          tile_re(ii, jj) = prev_aTx + prod_aTx.value
        }
      }

      // TODO1: can we merge Uh and Wx, given that Uh and Wx results are rougly the same size and can be
      // put into one tile?
      def Uh(re: SRAM2[T], h: SRAM2[T], u: DRAM2[T]) = {
        Foreach(Dh by N) { i =>
          val tileU = SRAM[T](N, Dh)
          tileU load u(i::i+N, 0::0+Dh)
          tileMult(re, tileU, h, N, Dh, N, 0)
        }
      }

      def Wx(re: SRAM2[T], c: SRAM2[T], w: DRAM2[T], x: DRAM2[T]) = {
        Foreach(Dh by N, d by Dh) { (i,k) =>
          val tileW = SRAM[T](N, Dh)
          val tilex = SRAM[T](Dh, N)

          Parallel {
            tileW load w(i::i+N, k::k+Dh)
            tilex load x(k::k+Dh, 0::0+N)
          }

          tileMult(re, tileW, tilex, N, Dh, N, k)
        }
      }

      def forward(
        /* Gate weights */
        Wi: DRAM2[T], Ui: DRAM2[T], Wf: DRAM2[T], Uf: DRAM2[T], Wo: DRAM2[T], Uo: DRAM2[T], Wc: DRAM2[T], Uc: DRAM2[T],
        /* memory inputs */
        tile_ct: SRAM2[T], tile_ht: SRAM2[T],
        /* word vectors */
        xt: DRAM2[T]
      ) = {
        val reg_ct = Reg[T](0.to[T])
        val tile_WiTx = SRAM[T](Dh, N)
        val tile_UiTh = SRAM[T](Dh, N)
        val tile_WfTx = SRAM[T](Dh, N)
        val tile_UfTh = SRAM[T](Dh, N)
        val tile_WoTx = SRAM[T](Dh, N)
        val tile_UoTh = SRAM[T](Dh, N)
        val tile_WcTx = SRAM[T](Dh, N)
        val tile_UcTh = SRAM[T](Dh, N)

        Parallel {
          Wx(tile_WiTx, tile_ct, Wi, xt)
          Wx(tile_WfTx, tile_ct, Wf, xt)
          Wx(tile_WoTx, tile_ct, Wo, xt)
          Uh(tile_UiTh, tile_ht, Ui)
          Uh(tile_UfTh, tile_ht, Uf)
          Uh(tile_UoTh, tile_ht, Uo)
        }

        Foreach(Dh by 1, N by 1) { (ii, jj) =>
          // TODO: can we parallize these three lines?
          // c_t = f_t \times c_{t-1} + i_t \times c_tl_t
//          reg_ct := sigmoid(tile_WfTx(ii, jj) + tile_UfTh(ii, jj)) * tile_Wc_t_1(ii, jj) +
//                           sigmoid(tile_WiTx(ii, jj) + tile_UiTh(ii, jj)) * tanh(tile_WcTx(ii, jj) + tile_UcTh(ii, jj))
//          tile_new_mem(ii, jj) = reg_ct.value
//
//          // h_t = o_t \times tanh(c_t)
//          tile_new_hidden(ii, jj) = sigmoid(tile_WoTx(ii, jj) + tile_UoTh(ii, jj)) * tanh(reg_ct.value)
//
//          tile_new_mem(ii, jj) = tile_WfTx(ii, jj) + tile_UfTh(ii, jj)
//          tile_new_hidden(ii, jj) = tile_WiTx(ii, jj) + tile_UiTh(ii, jj)
//
//          // TODO:  is a pipe needed here?
          reg_ct := (tile_WfTx(ii, jj) + tile_UfTh(ii, jj)) * tile_ct(ii, jj) +
                           (tile_WiTx(ii, jj) + tile_UiTh(ii, jj)) * (tile_WcTx(ii, jj) + tile_UcTh(ii, jj))
          tile_ct(ii, jj) = reg_ct.value
          tile_ht(ii, jj) = (tile_WoTx(ii, jj) + tile_UoTh(ii, jj)) * (reg_ct.value)
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
        forward(Wi, Ui, Wf, Uf, Wo, Uo, Wc, Uc, tile_ct, tile_ht, xt)
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
    val W_i =     loadCSV2D[X]("/home/tianzhao/data/4_by_4_basic_eles.csv", ",", "\n")
    val U_i =     loadCSV2D[X]("/home/tianzhao/data/4_by_4_basic_eles.csv", ",", "\n")
    val W_f =     loadCSV2D[X]("/home/tianzhao/data/4_by_4_basic_eles.csv", ",", "\n")
    val U_f =     loadCSV2D[X]("/home/tianzhao/data/4_by_4_basic_eles.csv", ",", "\n")
    val W_o =     loadCSV2D[X]("/home/tianzhao/data/4_by_4_basic_eles.csv", ",", "\n")
    val U_o =     loadCSV2D[X]("/home/tianzhao/data/4_by_4_basic_eles.csv", ",", "\n")
    val W_c =     loadCSV2D[X]("/home/tianzhao/data/4_by_4_basic_eles.csv", ",", "\n")
    val U_c =     loadCSV2D[X]("/home/tianzhao/data/4_by_4_basic_eles.csv", ",", "\n")
    val x_t =     loadCSV2D[X]("/home/tianzhao/data/4_by_2_basic_eles.csv", ",", "\n")
    val h_t_1 =   loadCSV2D[X]("/home/tianzhao/data/4_by_2_basic_eles.csv", ",", "\n")
    val c_t_1 =   loadCSV2D[X]("/home/tianzhao/data/4_by_2_basic_eles.csv", ",", "\n")

    val (nextMem, nextHidden) = GateForward (
      /* Weights */
      W_i, U_i, W_f, U_f, W_o, U_o, W_c, U_c, c_t_1,
      /* Inputs */
      x_t, h_t_1
    )

    writeCSV2D[X](nextMem, "/home/tianzhao/spatial-LSTM/spatial-lang/apps/results/LSTM_Forward_Single/ct.csv", ",", "\n")
    writeCSV2D[X](nextHidden, "/home/tianzhao/spatial-LSTM/spatial-lang/apps/results/LSTM_Forward_Single/ht.csv", ",", "\n")
  }
}
