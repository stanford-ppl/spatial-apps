import spatial.dsl._
import org.virtualized._


object MatMult_inner extends SpatialApp { // Regression (Dense) // Args: 32 128 128
  type X = FixPt[TRUE,_16,_16]

  val innerPar = 16
  val midPar = 2
  val outerPar = 2

  val tsm = 16
  val tsn = 64
  val tsp = 64

  @virtualize
  def MatMult_inner[T:Type:Num](A: Array[T], B: Array[T], mm: Int, nn: Int, pp: Int) = {
    val M = ArgIn[Int]
    val N = ArgIn[Int]
    val P = ArgIn[Int]
    setArg(M,mm)
    setArg(N,nn)
    setArg(P,pp)

    val a = DRAM[T](M, P)
    val b = DRAM[T](P, N)
    val c = DRAM[T](M, N)

    val bm = tsm (1 -> 1536)
    val bn = tsn (64 -> 64 -> 1536)
    val bp = tsp (64 -> 64 -> 1536)

    val op = outerPar (1 -> 6)
    val mp = midPar   (1 -> 64)
    val ip = innerPar (1 -> 64)
    val px = 1 (1 -> 1) // Cannot parallelize accum across k blocks

    setMem(a, A)
    setMem(b, B)

    Accel {
      Foreach(M by bm, N by bn par op){(i,j) =>
        val tileC = SRAM[T](bm, bn)

        Foreach(P by bp par px){k =>
          val tileA = SRAM[T](bm, bp)
          val tileB = SRAM[T](bp, bn)
          Parallel {
            tileA load a(i::i+bm, k::k+bp par 1) // Reads M*N*P times
            tileB load b(k::k+bp, j::j+bn par 1)
          }
          Foreach(bm by 1, bn by 1 par mp){ (ii,jj) =>    // MetaPipe?
            val prod = Reduce(Reg[T])(bp by 1 par ip){kk => tileA(ii, kk) * tileB(kk, jj) }{_+_}
            val prev = mux(k == 0, 0.to[T], tileC(ii,jj))
            tileC(ii,jj) = prev + prod.value // Is a unit pipe that should be recognized as accum
          }
        }
        c(i::i+bm, j::j+bn) store tileC // Writes M*N times
      }
    }
    getMem(c)
  }

  @virtualize
  def main() = {
    val M = args(0).to[Int]
    val N = args(1).to[Int]
    val P = args(2).to[Int]

    val a = Array.tabulate(M){ i => Array.tabulate(P){ j => ((i*P + j)%8).to[X] } }
    val b = Array.tabulate(P){ i => Array.tabulate(N){ j => ((i*N + j)%8).to[X] } }
    // val a = Array.fill(M){ Array.fill(P){random[T](100)} }
    // val b = Array.fill(P){ Array.fill(N){random[T](100)} }

    val result = MatMult_inner(a.flatten, b.flatten, M, N, P)

    val gold = Array.tabulate(M){i =>
      val aRow = a(i)
      Array.tabulate(N){j =>
        val bCol = b.map{row => row(j)}
        aRow.zip(bCol){_*_}.reduce{_+_}
      }
    }.flatten

    val gold_cksum = gold.map(a => a).reduce{_+_}
    val result_cksum = result.map(a => a).reduce{_+_}
    printArray(gold, "Gold: ")
    printArray(result, "Result: ")
    println("expected cksum: " + gold_cksum)
    println("result cksum:   " + result_cksum)

    // (0 until M*N) foreach { i => assert(result(i) == gold(i)) }

    val cksum = result_cksum == gold_cksum
    println("PASS: " + cksum + " (MatMult_inner) * Remember to fix GEMM_MemoryHierarchy once issue #159 is fixed!")

  }
}


object NW extends SpatialApp { // Regression (Dense) // Args: tcgacgaaataggatgacagcacgttctcgtattagagggccgcggtacaaaccaaatgctgcggcgtacagggcacggggcgctgttcgggagatcgggggaatcgtggcgtgggtgattcgccggc ttcgagggcgcgtgtcgcggtccatcgacatgcccggtcggtgggacgtgggcgcctgatatagaggaatgcgattggaaggtcggacgggtcggcgagttgggcccggtgaatctgccatggtcgat


 /*

  Needleman-Wunsch Genetic Alignment algorithm

    LETTER KEY:         Scores                   Ptrs
      a = 0                   T  T  C  G                T  T  C  G
      c = 1                0 -1 -2 -3 -4 ...         0  ←  ←  ←  ← ...
      g = 2             T -1  1  0 -1 -2          T  ↑  ↖  ←  ←  ←
      t = 3             C -2  0 -1  1  0          C  ↑  ↑  ↑  ↖  ←
      - = 4             G -3 -2 -2  0  2          G  ↑  ↑  ↑  ↑  ↖
      _ = 5             A -4 -3 -3 -1  1          A  ↑  ↑  ↑  ↑  ↖
                           .                         .
                           .                         .
                           .                         .

    PTR KEY:
      ← = 0 = skipB
      ↑ = 1 = skipA
      ↖ = 2 = align




 */

  @struct case class nw_tuple(score: Int16, ptr: Int16)

  @virtualize
  def main() = {

    // FSM setup
    val traverseState = 0
    val padBothState = 1
    val doneState = 2

    val a = argon.lang.String.char2num("a")
    val c = argon.lang.String.char2num("c")
    val g = argon.lang.String.char2num("g")
    val t = argon.lang.String.char2num("t")
    val d = argon.lang.String.char2num("-")
    val dash = ArgIn[Int8]
    setArg(dash,d)
    val underscore = argon.lang.String.char2num("_")

    val par_load = 1
    val par_store = 2
    val row_par = 2 (1 -> 1 -> 8)

    val SKIPB = 0
    val SKIPA = 1
    val ALIGN = 2
    val MATCH_SCORE = 1
    val MISMATCH_SCORE = -1
    val GAP_SCORE = -1
    // val seqa_string = "tcgacgaaataggatgacagcacgttctcgtattagagggccgcggtacaaaccaaatgctgcggcgtacagggcacggggcgctgttcgggagatcgggggaatcgtggcgtgggtgattcgccggc".toText
    // val seqb_string = "ttcgagggcgcgtgtcgcggtccatcgacatgcccggtcggtgggacgtgggcgcctgatatagaggaatgcgattggaaggtcggacgggtcggcgagttgggcccggtgaatctgccatggtcgat".toText
    val seqa_string = args(0).to[MString] //"tcgacgaaataggatgacagcacgttctcgtattagagggccgcggtacaaaccaaatgctgcggcgtacagggcacggggcgctgttcgggagatcgggggaatcgtggcgtgggtgattcgccggc"
    val seqb_string = args(1).to[MString] //"ttcgagggcgcgtgtcgcggtccatcgacatgcccggtcggtgggacgtgggcgcctgatatagaggaatgcgattggaaggtcggacgggtcggcgagttgggcccggtgaatctgccatggtcgat"
    val measured_length = seqa_string.length
    val length = ArgIn[Int]
    val lengthx2 = ArgIn[Int]
    setArg(length, measured_length)
    setArg(lengthx2, 2*measured_length)
    val max_length = 512
    assert(max_length >= length, "Cannot have string longer than 512 elements")

    val seqa_bin = argon.lang.String.string2num(seqa_string)
    // Array.tabulate[Int](seqa_string.length){i =>
    //   val char = seqa_string(i)
    //   if (char == "a") {0.to[Int]}
    //   else if (char == "c") {1.to[Int]}
    //   else if (char == "g") {2.to[Int]}
    //   else if (char == "t") {3.to[Int]}
    //   else {6.to[Int]}
    // } // TODO: Support c++ types with 2 bits in dram
    val seqb_bin = argon.lang.String.string2num(seqb_string)
    // Array.tabulate[Int](seqb_string.length){i =>
    //   val char = seqb_string(i)
    //   if (char == "a") {0.to[Int]}
    //   else if (char == "c") {1.to[Int]}
    //   else if (char == "g") {2.to[Int]}
    //   else if (char == "t") {3.to[Int]}
    //   else {6.to[Int]}
    // } // TODO: Support c++ types with 2 bits in dram

    val seqa_dram_raw = DRAM[Int8](length)
    val seqb_dram_raw = DRAM[Int8](length)
    val seqa_dram_aligned = DRAM[Int8](lengthx2)
    val seqb_dram_aligned = DRAM[Int8](lengthx2)
    setMem(seqa_dram_raw, seqa_bin)
    setMem(seqb_dram_raw, seqb_bin)

    Accel{
      val seqa_sram_raw = SRAM[Int8](max_length)
      val seqb_sram_raw = SRAM[Int8](max_length)
      val seqa_fifo_aligned = FIFO[Int8](max_length*2)
      val seqb_fifo_aligned = FIFO[Int8](max_length*2)

      seqa_sram_raw load seqa_dram_raw(0::length par par_load)
      seqb_sram_raw load seqb_dram_raw(0::length par par_load)

      val score_matrix = SRAM[nw_tuple](max_length+1,max_length+1)

      // Build score matrix
      Foreach(length+1 by 1 par row_par){ r =>
        val this_body = r % row_par
        Sequential.Foreach(-this_body until length+1 by 1) { c => // Bug #151, should be able to remove previous_result reg when fixed
          val previous_result = Reg[nw_tuple]
          val update = if (r == 0) (nw_tuple(-c.as[Int16], 0)) else if (c == 0) (nw_tuple(-r.as[Int16], 1)) else {
            val match_score = mux(seqa_sram_raw(c-1) == seqb_sram_raw(r-1), MATCH_SCORE.to[Int16], MISMATCH_SCORE.to[Int16])
            val from_top = score_matrix(r-1, c).score + GAP_SCORE
            val from_left = previous_result.score + GAP_SCORE
            val from_diag = score_matrix(r-1, c-1).score + match_score
            mux(from_left >= from_top && from_left >= from_diag, nw_tuple(from_left, SKIPB), mux(from_top >= from_diag, nw_tuple(from_top,SKIPA), nw_tuple(from_diag, ALIGN)))
          }
          previous_result := update
          if (c >= 0) {score_matrix(r,c) = update}
          // score_matrix(r,c) = update
        }
      }

      // Read score matrix
      val b_addr = Reg[Int](0)
      val a_addr = Reg[Int](0)
      Parallel{b_addr := length; a_addr := length}
      val done_backtrack = Reg[Bit](false)
      FSM[Int](state => state != doneState) { state =>
        if (state == traverseState) {
          if (score_matrix(b_addr,a_addr).ptr == ALIGN.to[Int16]) {
            seqa_fifo_aligned.enq(seqa_sram_raw(a_addr-1), !done_backtrack)
            seqb_fifo_aligned.enq(seqb_sram_raw(b_addr-1), !done_backtrack)
            done_backtrack := b_addr == 1.to[Int] || a_addr == 1.to[Int]
            b_addr :-= 1
            a_addr :-= 1
          } else if (score_matrix(b_addr,a_addr).ptr == SKIPA.to[Int16]) {
            seqb_fifo_aligned.enq(seqb_sram_raw(b_addr-1), !done_backtrack)
            seqa_fifo_aligned.enq(dash, !done_backtrack)
            done_backtrack := b_addr == 1.to[Int]
            b_addr :-= 1
          } else {
            seqa_fifo_aligned.enq(seqa_sram_raw(a_addr-1), !done_backtrack)
            seqb_fifo_aligned.enq(dash, !done_backtrack)
            done_backtrack := a_addr == 1.to[Int]
            a_addr :-= 1
          }
        } else if (state == padBothState) {
          seqa_fifo_aligned.enq(underscore, !seqa_fifo_aligned.full) // I think this FSM body either needs to be wrapped in a body or last enq needs to be masked or else we are full before FSM sees full
          seqb_fifo_aligned.enq(underscore, !seqb_fifo_aligned.full)
        } else {}
      } { state =>
        mux(state == traverseState && ((b_addr == 0.to[Int]) || (a_addr == 0.to[Int])), padBothState,
          mux(seqa_fifo_aligned.full || seqb_fifo_aligned.full, doneState, state))// Safe to assume they fill at same time?
      }

      Parallel{
        seqa_dram_aligned(0::length*2 par par_store) store seqa_fifo_aligned
        seqb_dram_aligned(0::length*2 par par_store) store seqb_fifo_aligned
      }

    }

    val seqa_aligned_result = getMem(seqa_dram_aligned)
    val seqb_aligned_result = getMem(seqb_dram_aligned)
    val seqa_aligned_string = argon.lang.String.num2string(seqa_aligned_result)
    val seqb_aligned_string = argon.lang.String.num2string(seqb_aligned_result)

    // val seqa_gold_string = "cggccgcttag-tgggtgcggtgctaagggggctagagggcttg-tc-gcggggcacgggacatgcg--gcg-t--cgtaaaccaaacat-g-gcgccgggag-attatgctcttgcacg-acag-ta----g-gat-aaagc---agc-t_________________________________________________________________________________________________________".toText
    // val seqb_gold_string = "--------tagct-ggtaccgt-ctaa-gtggc--ccggg-ttgagcggctgggca--gg-c-tg-gaag-gttagcgt-aaggagatatagtccg-cgggtgcagggtg-gctggcccgtacagctacctggcgctgtgcgcgggagctt_________________________________________________________________________________________________________".toText

    // val seqa_gold_bin = argon.lang.String.string2num(seqa_gold_string)
    // Array.tabulate[Int](seqa_gold_string.length){i =>
    //   val char = seqa_gold_string(i)
    //   if (char == "a") {0.to[Int]}
    //   else if (char == "c") {1.to[Int]}
    //   else if (char == "g") {2.to[Int]}
    //   else if (char == "t") {3.to[Int]}
    //   else if (char == "-") {4.to[Int]}
    //   else if (char == "_") {5.to[Int]}
    //   else {6.to[Int]}
    // }
    // val seqb_gold_bin = argon.lang.String.string2num(seqb_gold_string)
    // Array.tabulate[Int](seqb_gold_string.length){i =>
    //   val char = seqb_gold_string(i)
    //   if (char == "a") {0.to[Int]}
    //   else if (char == "c") {1.to[Int]}
    //   else if (char == "g") {2.to[Int]}
    //   else if (char == "t") {3.to[Int]}
    //   else if (char == "-") {4.to[Int]}
    //   else if (char == "_") {5.to[Int]}
    //   else {6.to[Int]}
    // }

    // Pass if >75% match
    val matches = seqa_aligned_result.zip(seqb_aligned_result){(a,b) => if ((a == b) || (a == dash) || (b == dash)) 1 else 0}.reduce{_+_}
    val cksum = matches.to[Float] > 0.75.to[Float]*measured_length.to[Float]*2

    println("Result A: " + seqa_aligned_string)
    // println("Gold A:   " + seqa_gold_string)
    println("Result B: " + seqb_aligned_string)
    // println("Gold B:   " + seqb_gold_string)
    println("Found " + matches + " matches out of " + measured_length*2 + " elements")
    // val cksumA = seqa_aligned_string == seqa_gold_string //seqa_aligned_result.zip(seqa_gold_bin){_==_}.reduce{_&&_}
    // val cksumB = seqb_aligned_string == seqb_gold_string //seqb_aligned_result.zip(seqb_gold_bin){_==_}.reduce{_&&_}
    // val cksum = cksumA && cksumB
    println("PASS: " + cksum + " (NW)")
  }
}


object GEMM extends SpatialApp {

  @virtualize
  def main() {

    type T = FixPt[TRUE,_24,_8]
    val tileM = 16
    val tileN = 16
    val tileK = 16

    val M = ArgIn[Int]
    val N = ArgIn[Int]
    val K = ArgIn[Int]
    setArg(M,args(0).to[Int])
    setArg(N,args(1).to[Int])
    setArg(K,args(2).to[Int])

    val a_data = (0::args(0).to[Int], 0::args(2).to[Int]){(i,j) => random[T](3)}
    val b_data = (0::args(2).to[Int], 0::args(1).to[Int]){(i,j) => random[T](3)}
    val c_init = (0::args(0).to[Int], 0::args(1).to[Int]){(i,j) => 0.to[T]}
    val a = DRAM[T](M, K)
    val b = DRAM[T](K, N)
    val c = DRAM[T](M, N)

    setMem(a, a_data)
    setMem(b, b_data)
    setMem(c, c_init)

    Accel {
      Foreach(K by tileK){kk =>
        val numel_k = min(tileK.to[Int], K - kk)
        Foreach(M by tileM){mm =>
          val numel_m = min(tileM.to[Int], M - mm)
          val tileA_sram = SRAM[T](tileM, tileK)
          tileA_sram load a(mm::mm+numel_m, kk::kk+numel_k)
          Foreach(N by tileN){nn =>
            val numel_n = min(tileN.to[Int], N - nn)
            val tileB_sram = SRAM[T](tileK, tileN)
            val tileC_sram = SRAM.buffer[T](tileM, tileN)
            tileB_sram load b(kk::kk+numel_k, nn::nn+numel_n par 8)
            tileC_sram load c(mm::mm+numel_m, nn::nn+numel_n)

            MemFold(tileC_sram)(numel_k by 1 par 2){k =>
              val tileK_local = SRAM[T](tileM, tileN)
              Foreach(numel_m by 1, numel_n by 1){(i,j) =>
                      tileK_local(i,j) = tileA_sram(i,k) * tileB_sram(k,j)
              }
              tileK_local
            }{_+_}

            c(mm::mm+numel_m, nn::nn+numel_n) store tileC_sram
          }
        }
      }
    }

    val accel_matrix = getMatrix(c)

    val gold_matrix = (0::args(0).to[Int], 0::args(1).to[Int]){(i,j) =>
      Array.tabulate(args(2).to[Int]){k => a_data(i,k) * b_data(k,j)}.reduce{_+_}
    }

    printMatrix(accel_matrix, "Received: ")
    printMatrix(gold_matrix, "Wanted: ")
    val cksum = accel_matrix.zip(gold_matrix){_==_}.reduce{_&&_}
    println("Pass? " + cksum)
  }
}


object Sobel extends SpatialApp { // Regression (Dense) // Args: 200 160
  val Kh = 3
  val Kw = 3
  val Cmax = 160

  @virtualize
  def convolve[T:Type:Num](image: Matrix[T]): Matrix[T] = {
    val B = 16 (1 -> 1 -> 16)

    val R = ArgIn[Int]
    val C = ArgIn[Int]
    setArg(R, image.rows)
    setArg(C, image.cols)
    bound(R) = 256
    bound(C) = 160

    val lb_par = 1
    val par_store = 1
    val row_stride = 10 (3 -> 3 -> 500)
    val row_par = 1
    val par_Kh = 1
    val par_Kw = 1

    val img = DRAM[T](R, C)
    val imgOut = DRAM[T](R, C)

    setMem(img, image)

    Accel {
      Foreach(R by row_stride par row_par){ rr =>
        val rows_todo = min(row_stride, R - rr)
        val lb = LineBuffer[T](Kh, Cmax)
        val sr = RegFile[T](Kh, Kw)
        val lineOut = SRAM[T](Cmax)
        val kh = LUT[T](3,3)(1.to[T], 0.to[T], -1.to[T],
                             2.to[T], 0.to[T], -2.to[T],
                             1.to[T], 0.to[T], -1.to[T])
        val kv = LUT[T](3,3)(1.to[T],  2.to[T],  1.to[T],
                             0.to[T],  0.to[T],  0.to[T],
                            -1.to[T], -2.to[T], -1.to[T])

        Foreach(-2 until rows_todo) { r =>
          // println(" r is " + r)
          val ldaddr = if ((r.to[Index]+rr.to[Index]) < 0.to[Index] || (r.to[Index]+rr.to[Index]) > R.value) 0.to[Index] else {r.to[Index]+rr.to[Index]}
          lb load img(ldaddr, 0::C par lb_par)

          Foreach(0 until C) { c =>
            Pipe{sr.reset(c == 0)}

            Foreach(0 until Kh par Kh){i => sr(i, *) <<= lb(i, c) }

            // val accum = Reg[Tup2[T,T]](pack(0.to[T], 0.to[T]))

            // Reduce(accum)(Kh by 1, Kh by 1 par 3) {(i,j) =>
            //   val pxl = sr(i,j)
            //   pack(pxl * kh(i,j), pxl * kv(i,j))
            // }{(a,b) => pack(a._1 + b._1, a._2 + b._2)}

            val accum = List.tabulate(Kh){i => List.tabulate(Kh){j =>
              val pxl = sr(i,j)
              pack(pxl * kh(i,j), pxl * kv(i,j))
            }}.flatten.reduce{(a,b) => pack(a._1 + b._1, a._2 + b._2)}

            lineOut(c) = mux(r.to[Index] + rr.to[Index] < 2.to[Index] || r.to[Index] + rr.to[Index] >= R-2, 0.to[T], abs(accum._1) + abs(accum._2))// Technically should be sqrt(horz**2 + vert**2)
            // println("lineout c = " + mux(r.to[Index] + rr.to[Index] < 2.to[Index], 0.to[T], abs(horz.value) + abs(vert.value)))
          }

          if (r.to[Index]+rr.to[Index] < R && r.to[Index] >= 0.to[Index]) {
            // println("storing to row " + {r+rr} + " from " + r + " " + rr)
            // Foreach(0 until C){kk => print(" " + lineOut(kk))}
            // println(" ")
            imgOut(r.to[Index]+rr.to[Index], 0::C par par_store) store lineOut
          }
        }

      }
    }

    getMatrix(imgOut)

  }

  @virtualize
  def main() {
    val R = args(0).to[Int] //1895
    val C = args(1).to[Int] //1024
    val border = 3
    // val image = (0::R, 0::C){(i,j) => if (j > 3 && i > 3 && j < 11 && i < 11) 256 else 0 }
    val image = (0::R, 0::C){(i,j) => if (j > border && j < C-border && i > border && i < C - border) i*16 else 0}
    val ids = (0::R, 0::C){(i,j) => if (i < 2) 0 else 1}

    val kh = List((List(1,2,1), List(0,0,0), List(-1,-2,-1)))
    val kv = List((List(1,0,-1), List(2,0,-2), List(1,0,-1)))

    val output = convolve(image)

    /*
      Filters:
      1   2   1
      0   0   0
     -1  -2  -1

      1   0  -1
      2   0  -2
      1   0  -1

    */
    val gold = (0::R, 0::C){(i,j) =>
      if (i >= R-2) {
        0
      } else if (i >= 2 && j >= 2) {
        val px00 = image(i,j)
        val px01 = image(i,j-1)
        val px02 = image(i,j-2)
        val px10 = image(i-1,j)
        val px11 = image(i-1,j-1)
        val px12 = image(i-1,j-2)
        val px20 = image(i-2,j)
        val px21 = image(i-2,j-1)
        val px22 = image(i-2,j-2)
        abs(px00 * 1 + px01 * 2 + px02 * 1 - px20 * 1 - px21 * 2 - px22 * 1) + abs(px00 * 1 - px02 * 1 + px10 * 2 - px12 * 2 + px20 * 1 - px22 * 1)
      } else {
        0
      }
      // Shift result down by 2 and over by 2 because of the way accel is written

    };

    // // This contains the "weird scheduling bug"
    printMatrix(image, "Image")
    printMatrix(gold, "Gold")
    printMatrix(output, "Output")

    val gold_sum = gold.map{g => g}.reduce{_+_}
    val output_sum = output.zip(ids){case (o,i) => i * o}.reduce{_+_}
    println("gold " + gold_sum + " =?= output " + output_sum)
    val cksum = gold_sum == output_sum
    // val cksum = gold.zip(output){(g, o) => g == o}.reduce{_&&_}
    println("PASS: " + cksum + " (Sobel)")
  }
}


// FSM
object BasicCondFSM extends SpatialApp { // Regression (Unit) // Args: none


  @virtualize
  def main() {
    val dram = DRAM[Int](32)
    Accel {
      val bram = SRAM[Int](32)
      val reg = Reg[Int](0)
      reg := 16
      FSM[Int]{state => state < 32} { state =>
        if (state < 16) {
          if (state < 8) {
            bram(31 - state) = state // 16:31 [7, 6, ... 0]
          } else {
            bram(31 - state) = state+1 // 16:31 [16, 15, ... 9]
          }
        }
        else {
          bram(state - 16) = if (state == 16) 17 else if (state == 17) reg.value else state // Test const, regread, and bound Mux1H
        }
      }{state => state + 1}

      dram(0::32 par 16) store bram
    }
    val result = getMem(dram)
    val gold = Array[Int](17, 16, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28,
                          29, 30, 31, 16, 15, 14, 13, 12, 11, 10, 9, 7, 6, 5, 4, 3, 2, 1, 0)
    printArray(result, "Result")
    printArray(gold, "Gold")
    // for (i <- 0 until 32){ assert(result(i) == gold(i)) }
    val cksum = gold.zip(result){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (BasicCondFSM)")
  }
}


// MemReduce
object SimpleMemReduce extends SpatialApp { // Regression (Unit) // Args: none

  val N = 16.to[Int]

  @virtualize
  def main() {

    val out = DRAM[Int](16)
    val out2 = DRAM[Int](16)

    Accel {
      val a = SRAM[Int](16)
      MemReduce(a)(-5 until 0 by 1){i =>
        val tmp = SRAM[Int](16)
        Foreach(16 by 1) { j => tmp(j) = 1}
        tmp
      }{_+_}
      val b = SRAM[Int](16)
      Foreach(15 until -1 by -1){i => b(i) = 2}
      out store a
      out2 store b
    }
    val result = getMem(out)
    val result2 = getMem(out2)

    val gold = Array.tabulate(16){i => 5.to[Int]}
    val gold2 = Array.tabulate(16){i => 2.to[Int]}
    printArray(gold, "expected: ")
    printArray(result, "result:   ")
    printArray(gold2, "expected: ")
    printArray(result2, "result:   ")

    val cksum = gold.zip(result){_==_}.reduce{_&&_} && gold2.zip(result2){_==_}.reduce{_&&_}
    println("PASS: " + cksum + " (SimpleMemReduce)")
  }
}
