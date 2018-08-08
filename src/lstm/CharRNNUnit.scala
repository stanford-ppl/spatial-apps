import spatial.dsl._
import virtualized._


object CharRNNUnit extends SpatialApp with CharRNNParams with Activations {
  type T = FixPt[TRUE, _16, _16]

  @virtualize
  def main() {
    val lout = rnn_size*4
    val i2h_1w = DRAM[T](input_size, lout)
    val i2h_1b = DRAM[T](lout)
    val i2h_2w = DRAM[T](rnn_size, lout)
    val i2h_2b = DRAM[T](lout)
    val h2h_1w = DRAM[T](rnn_size, lout)
    val h2h_1b = DRAM[T](lout)
    val h2h_2w = DRAM[T](rnn_size, lout)
    val h2h_2b = DRAM[T](lout)
    val decoder_w_ = DRAM[T](rnn_size, input_size)
    val decoder_b_ = DRAM[T](input_size)
    val c_ = DRAM[T](batch_size, rnn_size)
    val h_ = DRAM[T](batch_size, rnn_size)
    val inputs = DRAM[T](seq_length, batch_size, input_size)
    val drams_2D = List(i2h_1w, i2h_2w, h2h_1w, h2h_2w, decoder_w_, c_, h_, inputs)
    drams_2D.zipWithIndex.foreach { case(e, idx) =>
      setMem(e, loadCSV2D[T](dataPaths_2D(idx), ",", "\n"))
    }

    val drams_1D = List(i2h_1b, i2h_2b, h2h_1b, h2h_2b, decoder_b_)
    drams_1D.zipWithIndex.foreach { case(e, idx) =>
      setMem(e, loadCSV1D[T](dataPaths_1D(idx), ","))
    }

    val results = DRAM[T](seq_length, input_size, batch_size)

    Accel {
      val i2h1w = SRAM[T](input_size, lout)
      val i2h1b = SRAM[T](lout)
      val h2h1w = SRAM[T](rnn_size, lout)
      val h2h1b = SRAM[T](lout)
      val i2h2w = SRAM[T](rnn_size, lout)
      val i2h2b = SRAM[T](lout)
      val h2h2w = SRAM[T](rnn_size, lout)
      val h2h2b = SRAM[T](lout)
      val decoder_w = SRAM[T](rnn_size, input_size)
      val decoder_b = SRAM[T](input_size)
      val c = SRAM[T](batch_size, rnn_size)
      val h = SRAM[T](batch_size, rnn_size)
      val x = SRAM[T](batch_size, input_size)
      val in_gate = SRAM[T](batch_size, rnn_size)
      val forget_gate = SRAM[T](batch_size, rnn_size)
      val out_gate = SRAM[T](batch_size, rnn_size)
      val in_transform = SRAM[T](batch_size, rnn_size)

      i2h1w load i2h_1w(0::input_size, 0::lout)
      i2h1b load i2h_1b(0::lout)
      h2h1w load h2h_1w(0::rnn_size, 0::lout)
      h2h1b load h2h_1b(0::lout)
      i2h2w load i2h_2w(0::rnn_size, 0::lout)
      i2h2b load i2h_2b(0::lout)
      h2h2w load h2h_2w(0::rnn_size, 0::lout)
      h2h2b load h2h_2b(0::lout)
      decoder_w load decoder_w_(0::rnn_size, 0::lout)
      decoder_b load decoder_b_(0::lout)
      c load c_ (0::batch_size, 0::rnn_size)
      h load h_ (0::batch_size, 0::rnn_size)

      Sequential.Foreach(seq_length by 1) { idx =>
        x load inputs(idx, 0::batch_size, 0::input_size)

        // stage 1
        Foreach (batch_size by 1, lout by 1) { (i, j) =>
          val i2h_prod = Reduce(Reg[T])(input_size by 1) { k => x(i, k) * i2h1w(k, j) } {_+_}
          val i2h_ele = i2h1b(j) + i2h_prod.value
          val h2h_prod = Reduce(Reg[T])(rnn_size by 1) { k => h(i, k) * h2h1w(k, j) } {_+_}
          val ele = h2h1b(j) + h2h_prod.value + i2h_ele
          if (j < rnn_size)
            in_gate(i, j) = sigmoid_(ele)
          else if (rnn_size <= j && j < rnn_size * 2)
            forget_gate(i, j - rnn_size) = sigmoid_(ele)
          else if (rnn_size * 2 <= j && j < rnn_size * 3)
            out_gate(i, j - rnn_size * 2) = sigmoid_(ele)
          else
            in_transform(i, j - rnn_size * 3) = tanh_(ele)
        }

        Foreach (batch_size by 1, rnn_size by 1) { (i, j) =>
          val cc = c(i, j) * forget_gate(i, j) + in_gate(i, j) * in_transform(i, j)
          c(i, j) = cc
          h(i, j) = tanh_(cc) * out_gate(i, j)
        }

        // decoder for h. Don't keep intermediate hidden and memory states
        Foreach(batch_size by 1, input_size by 1) { (i, j) =>
          val proj = Reduce(Reg[T])(rnn_size by 1) { k => h(i, k) * decoder_w(k, j) } {_+_}
          h(i, j) = proj + decoder_b(j)
        }

        results(idx, 0::batch_size, 0::rnn_size) store h
      }
    }

    val decoded_re = getMem(h_)
    // Provides a distribution over each char in the batch
    writeCSV1D[T](decoded_re, "/result.csv")
  }
}
