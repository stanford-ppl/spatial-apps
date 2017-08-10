import spatial.dsl._
import org.virtualized._
import spatial.targets._

// No opportunities for par
object SW1 extends SpatialApp { // Regression (Dense) // Args: tcgacgaaataggatgacagcacgttctcgtattagagggccgcggtacaaaccaaatgctgcggcgtacagggcacggggcgctgttcgggagatcgggggaatcgtggcgtgggtgattcgccggc ttcgagggcgcgtgtcgcggtccatcgacatgcccggtcggtgggacgtgggcgcctgatatagaggaatgcgattggaaggtcggacgggtcggcgagttgggcccggtgaatctgccatggtcgat
  override val target = Zynq


 /*
  
  Smith-Waterman Genetic Alignment algorithm                                                  
  
  This is just like NW algorithm, except negative scores are capped at 0, backwards traversal starts at highest score from any 
     element on the perimeter, and end when score is 0


    [SIC] NW diagram
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

  @struct case class sw_tuple(score: Int16, ptr: Int16)
  @struct case class entry_tuple(row: Index, col: Index, score: Int16)

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

    val par_load = 16
    val par_store = 16
    val row_par = 3 (1 -> 1 -> 8)

    val SKIPB = 0
    val SKIPA = 1
    val ALIGN = 2
    val MATCH_SCORE = 2
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
    val max_length = 320 
    assert(max_length >= length, "Cannot have string longer than 256 elements")

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

      val score_matrix = SRAM[sw_tuple](max_length+1,max_length+1)

      val entry_point = Reg[entry_tuple]
      // Build score matrix
      Reduce(entry_point)(length+1 by 1 par row_par){ r =>
        val possible_entry_point = Reg[entry_tuple]
        val this_body = r % row_par
        Sequential.Foreach(-this_body until length+1 by 1) { c => // Bug #151, should be able to remove previous_result reg when fixed
          val previous_result = Reg[sw_tuple]
          val update = if (r == 0) (sw_tuple(0, 0)) else if (c == 0) (sw_tuple(0, 1)) else {
            val match_score = mux(seqa_sram_raw(c-1) == seqb_sram_raw(r-1), MATCH_SCORE.to[Int16], MISMATCH_SCORE.to[Int16])
            val from_top = score_matrix(r-1, c).score + GAP_SCORE
            val from_left = previous_result.score + GAP_SCORE
            val from_diag = score_matrix(r-1, c-1).score + match_score
            mux(from_left >= from_top && from_left >= from_diag, sw_tuple(from_left, SKIPB), mux(from_top >= from_diag, sw_tuple(from_top,SKIPA), sw_tuple(from_diag, ALIGN)))
          }
          previous_result := update
          if ((c == length || r == length) && possible_entry_point.score < update.score) possible_entry_point := entry_tuple(r, c, update.score)
          if (c >= 0) {score_matrix(r,c) = sw_tuple(max(0, update.score),update.ptr)}
          // score_matrix(r,c) = update
        }
        possible_entry_point
      }{(a,b) => mux(a.score > b.score, a, b)}

      // Read score matrix
      val b_addr = Reg[Int](0)
      val a_addr = Reg[Int](0)
      Parallel{b_addr := entry_point.row; a_addr := entry_point.col}
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
        mux(state == traverseState && (score_matrix(b_addr,a_addr).score == 0.to[Int16]), doneState, state) 
      }

      Parallel{
        seqa_dram_aligned(0::seqa_fifo_aligned.numel par par_store) store seqa_fifo_aligned
        seqb_dram_aligned(0::seqb_fifo_aligned.numel par par_store) store seqb_fifo_aligned
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
    println("PASS: " + cksum + " (SW)")



  }
}

object SW2 extends SpatialApp { // Regression (Dense) // Args: tcgacgaaataggatgacagcacgttctcgtattagagggccgcggtacaaaccaaatgctgcggcgtacagggcacggggcgctgttcgggagatcgggggaatcgtggcgtgggtgattcgccggc ttcgagggcgcgtgtcgcggtccatcgacatgcccggtcggtgggacgtgggcgcctgatatagaggaatgcgattggaaggtcggacgggtcggcgagttgggcccggtgaatctgccatggtcgat
  override val target = Zynq


 /*
  
  Smith-Waterman Genetic Alignment algorithm                                                  
  
  This is just like NW algorithm, except negative scores are capped at 0, backwards traversal starts at highest score from any 
     element on the perimeter, and end when score is 0


    [SIC] NW diagram
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

  @struct case class sw_tuple(score: Int16, ptr: Int16)
  @struct case class entry_tuple(row: Index, col: Index, score: Int16)

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

    val par_load = 16
    val par_store = 16
    val row_par = 3 (1 -> 1 -> 8)

    val SKIPB = 0
    val SKIPA = 1
    val ALIGN = 2
    val MATCH_SCORE = 2
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
    val max_length = 336
    assert(max_length >= length, "Cannot have string longer than 256 elements")

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

      val score_matrix = SRAM[sw_tuple](max_length+1,max_length+1)

      val entry_point = Reg[entry_tuple]
      // Build score matrix
      Reduce(entry_point)(length+1 by 1 par row_par){ r =>
        val possible_entry_point = Reg[entry_tuple]
        val this_body = r % row_par
        Sequential.Foreach(-this_body until length+1 by 1) { c => // Bug #151, should be able to remove previous_result reg when fixed
          val previous_result = Reg[sw_tuple]
          val update = if (r == 0) (sw_tuple(0, 0)) else if (c == 0) (sw_tuple(0, 1)) else {
            val match_score = mux(seqa_sram_raw(c-1) == seqb_sram_raw(r-1), MATCH_SCORE.to[Int16], MISMATCH_SCORE.to[Int16])
            val from_top = score_matrix(r-1, c).score + GAP_SCORE
            val from_left = previous_result.score + GAP_SCORE
            val from_diag = score_matrix(r-1, c-1).score + match_score
            mux(from_left >= from_top && from_left >= from_diag, sw_tuple(from_left, SKIPB), mux(from_top >= from_diag, sw_tuple(from_top,SKIPA), sw_tuple(from_diag, ALIGN)))
          }
          previous_result := update
          if ((c == length || r == length) && possible_entry_point.score < update.score) possible_entry_point := entry_tuple(r, c, update.score)
          if (c >= 0) {score_matrix(r,c) = sw_tuple(max(0, update.score),update.ptr)}
          // score_matrix(r,c) = update
        }
        possible_entry_point
      }{(a,b) => mux(a.score > b.score, a, b)}

      // Read score matrix
      val b_addr = Reg[Int](0)
      val a_addr = Reg[Int](0)
      Parallel{b_addr := entry_point.row; a_addr := entry_point.col}
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
        mux(state == traverseState && (score_matrix(b_addr,a_addr).score == 0.to[Int16]), doneState, state) 
      }

      Parallel{
        seqa_dram_aligned(0::seqa_fifo_aligned.numel par par_store) store seqa_fifo_aligned
        seqb_dram_aligned(0::seqb_fifo_aligned.numel par par_store) store seqb_fifo_aligned
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
    println("PASS: " + cksum + " (SW)")



  }
}

object SW3 extends SpatialApp { // Regression (Dense) // Args: tcgacgaaataggatgacagcacgttctcgtattagagggccgcggtacaaaccaaatgctgcggcgtacagggcacggggcgctgttcgggagatcgggggaatcgtggcgtgggtgattcgccggc ttcgagggcgcgtgtcgcggtccatcgacatgcccggtcggtgggacgtgggcgcctgatatagaggaatgcgattggaaggtcggacgggtcggcgagttgggcccggtgaatctgccatggtcgat
  override val target = Zynq


 /*
  
  Smith-Waterman Genetic Alignment algorithm                                                  
  
  This is just like NW algorithm, except negative scores are capped at 0, backwards traversal starts at highest score from any 
     element on the perimeter, and end when score is 0


    [SIC] NW diagram
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

  @struct case class sw_tuple(score: Int16, ptr: Int16)
  @struct case class entry_tuple(row: Index, col: Index, score: Int16)

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

    val par_load = 16
    val par_store = 16
    val row_par = 4 (1 -> 1 -> 8)

    val SKIPB = 0
    val SKIPA = 1
    val ALIGN = 2
    val MATCH_SCORE = 2
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
    val max_length = 352
    assert(max_length >= length, "Cannot have string longer than 256 elements")

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

      val score_matrix = SRAM[sw_tuple](max_length+1,max_length+1)

      val entry_point = Reg[entry_tuple]
      // Build score matrix
      Reduce(entry_point)(length+1 by 1 par row_par){ r =>
        val possible_entry_point = Reg[entry_tuple]
        val this_body = r % row_par
        Sequential.Foreach(-this_body until length+1 by 1) { c => // Bug #151, should be able to remove previous_result reg when fixed
          val previous_result = Reg[sw_tuple]
          val update = if (r == 0) (sw_tuple(0, 0)) else if (c == 0) (sw_tuple(0, 1)) else {
            val match_score = mux(seqa_sram_raw(c-1) == seqb_sram_raw(r-1), MATCH_SCORE.to[Int16], MISMATCH_SCORE.to[Int16])
            val from_top = score_matrix(r-1, c).score + GAP_SCORE
            val from_left = previous_result.score + GAP_SCORE
            val from_diag = score_matrix(r-1, c-1).score + match_score
            mux(from_left >= from_top && from_left >= from_diag, sw_tuple(from_left, SKIPB), mux(from_top >= from_diag, sw_tuple(from_top,SKIPA), sw_tuple(from_diag, ALIGN)))
          }
          previous_result := update
          if ((c == length || r == length) && possible_entry_point.score < update.score) possible_entry_point := entry_tuple(r, c, update.score)
          if (c >= 0) {score_matrix(r,c) = sw_tuple(max(0, update.score),update.ptr)}
          // score_matrix(r,c) = update
        }
        possible_entry_point
      }{(a,b) => mux(a.score > b.score, a, b)}

      // Read score matrix
      val b_addr = Reg[Int](0)
      val a_addr = Reg[Int](0)
      Parallel{b_addr := entry_point.row; a_addr := entry_point.col}
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
        mux(state == traverseState && (score_matrix(b_addr,a_addr).score == 0.to[Int16]), doneState, state) 
      }

      Parallel{
        seqa_dram_aligned(0::seqa_fifo_aligned.numel par par_store) store seqa_fifo_aligned
        seqb_dram_aligned(0::seqb_fifo_aligned.numel par par_store) store seqb_fifo_aligned
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
    println("PASS: " + cksum + " (SW)")



  }
}


// good
object MD_Grid1 extends SpatialApp { // Regression (Dense) // Args: none
  override val target = Zynq


 /*
  
  Moleckaler Dynamics via the grid, a digital frontier
                                                                             
                                                                             
                                                                             
                                                                             
                                                                             
                            ←      BLOCK_SIDE     →                        
                ↗                                                                                                
                          __________________________________   
       BLOCK_SIDE        /                                  /|  
                        /                                  / |  
        ↙              /                                  /  |  
                      /_________________________________ /   |  
                     |           b1                     |    |  
           ↑         |        ..  ..  ..                |    |  
                     |       - - - - - -                |    |  
                     |      :``::``::``:                |    |  
           B         |    b1:..::__::..:b1              |    |  
           L         |      - - /_/| - -                |    |  
           O         |     :``:|b0||:``:                |    |  
           C         |   b1:..:|__|/:..:b1              |    |  
           K         |      - - - -  - -                |    |  
           |         |     :``::``: :``:                |    |  
           S         |   b1:..::..: :..:b1              |    |  
           I         |          b1                      |    |  
           D         |                                  |   /   
           E         |                                  |  /   
                     |                                  | /    
           ↓         |                                  |/     
                      ``````````````````````````````````       * Each b0 contains up to "density" number of atoms
                                                               * For each b0, and then for each atom in b0, compute this atom's
                                                                    interactions with all atoms in the adjacent (27) b1's
                                                               * One of the b1's will actually be b0, so skip this contribution                      
                                                                                                           
 */

  // Max pos seems to be about 19
  type T = FixPt[TRUE, _12, _20]
  @struct case class XYZ(x: T, y: T, z: T) 

  @virtualize
  def main() = {

    val N_ATOMS = 256
    val DOMAIN_EDGE = 20
    val BLOCK_SIDE = 4
    val density = 10
    val density_aligned = density + (8 - (density % 8))
    val lj1 = 1.5.to[T]
    val lj2 = 2.to[T]

    val par_load = 16 // Wider data type
    val par_store = 16 // Wider data type
    val loop_grid0_x = 1 // Temporarily broken because of some issue around #200
    val loop_grid0_y = 1 (1 -> 1 -> 16) 
    val loop_grid0_z = 1 (1 -> 1 -> 16)
    val loop_grid1_x = 1 (1 -> 1 -> 16)
    val loop_grid1_y = 1 (1 -> 1 -> 16)
    val loop_grid1_z = 4 (1 -> 1 -> 16)
    val loop_p =       2 (1 -> 1 -> 16)
    val loop_q =       2 (1 -> 1 -> 16)

    val raw_npoints = Array[Int](4,4,3,4,5,5,2,1,1,8,4,8,3,3,7,5,4,5,6,2,2,4,4,3,3,4,7,2,3,2,
                                 2,1,7,1,3,7,6,3,3,4,3,4,5,5,6,4,2,5,7,6,5,4,3,3,5,4,4,4,3,2,3,2,7,5)
    val npoints_data = raw_npoints.reshape(BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE)

    val raw_dvec = loadCSV1D[T]("/remote/regression/data/machsuite/grid_dvec.csv", "\n")
    // Strip x,y,z vectors from raw_dvec
    val dvec_x_data = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => raw_dvec(i*BLOCK_SIDE*BLOCK_SIDE*density*3 + j*BLOCK_SIDE*density*3 + k*density*3 + 3*l)}
    val dvec_y_data = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => raw_dvec(i*BLOCK_SIDE*BLOCK_SIDE*density*3 + j*BLOCK_SIDE*density*3 + k*density*3 + 3*l+1)}
    val dvec_z_data = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => raw_dvec(i*BLOCK_SIDE*BLOCK_SIDE*density*3 + j*BLOCK_SIDE*density*3 + k*density*3 + 3*l+2)}

    val dvec_x_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
    val dvec_y_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
    val dvec_z_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
    val force_x_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density_aligned)
    val force_y_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density_aligned)
    val force_z_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density_aligned)
    val npoints_dram = DRAM[Int](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE)

    setMem(dvec_x_dram, dvec_x_data)
    setMem(dvec_y_dram, dvec_y_data)
    setMem(dvec_z_dram, dvec_z_data)
    setMem(npoints_dram, npoints_data)

    Accel{
      val dvec_x_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
      val dvec_y_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
      val dvec_z_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
      val npoints_sram = SRAM[Int](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE)
      val force_x_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density_aligned)
      val force_y_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density_aligned)
      val force_z_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density_aligned)

      dvec_x_sram load dvec_x_dram(0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density par par_load)
      dvec_y_sram load dvec_y_dram(0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density par par_load)
      dvec_z_sram load dvec_z_dram(0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density par par_load)
      npoints_sram load npoints_dram(0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE par par_load)

      // Iterate over each block
      Foreach(BLOCK_SIDE by 1 par loop_grid0_x){b0x => Foreach(BLOCK_SIDE by 1 par loop_grid0_y){b0y => Foreach(BLOCK_SIDE by 1 par loop_grid0_z){b0z => 
        // Iterate over each point in this block, considering boundaries
        val b0_cube_forces = SRAM[XYZ](density)
        val b1x_start = max(0.to[Int],b0x-1.to[Int])
        val b1x_end = min(BLOCK_SIDE.to[Int], b0x+2.to[Int])
        val b1y_start = max(0.to[Int],b0y-1.to[Int])
        val b1y_end = min(BLOCK_SIDE.to[Int], b0y+2.to[Int])
        val b1z_start = max(0.to[Int],b0z-1.to[Int])
        val b1z_end = min(BLOCK_SIDE.to[Int], b0z+2.to[Int])
        MemReduce(b0_cube_forces)(b1x_start until b1x_end by 1, b1y_start until b1y_end by 1, b1z_start until b1z_end by 1 par loop_grid1_z) { (b1x, b1y, b1z) => 
          val b1_cube_contributions = SRAM.buffer[XYZ](density)
          // Iterate over points in b0
          val p_range = npoints_sram(b0x, b0y, b0z)
          val q_range = npoints_sram(b1x, b1y, b1z)
          Foreach(0 until p_range par loop_p) { p_idx =>
            val px = dvec_x_sram(b0x, b0y, b0z, p_idx)
            val py = dvec_y_sram(b0x, b0y, b0z, p_idx)
            val pz = dvec_z_sram(b0x, b0y, b0z, p_idx)
            val q_sum = Reg[XYZ](XYZ(0.to[T], 0.to[T], 0.to[T]))
            Reduce(q_sum)(0 until q_range par loop_q) { q_idx => 
              val qx = dvec_x_sram(b1x, b1y, b1z, q_idx)
              val qy = dvec_y_sram(b1x, b1y, b1z, q_idx)
              val qz = dvec_z_sram(b1x, b1y, b1z, q_idx)
              val tmp = if ( !(b0x == b1x && b0y == b1y && b0z == b1z && p_idx == q_idx) ) { // Skip self
                val delta = XYZ(px - qx, py - qy, pz - qz)
                val r2inv = (1.0.to[FixPt[TRUE,_4,_12]] / ( (delta.x*delta.x + delta.y*delta.y + delta.z*delta.z).to[FixPt[TRUE,_4,_12]] )).to[T]
                // Assume no cutoff and aways account for all nodes in area
                val r6inv = r2inv * r2inv * r2inv;
                val potential = r6inv*(lj1*r6inv - lj2);
                val force = r2inv*potential;
                XYZ(delta.x*force, delta.y*force, delta.z*force)
              } else {
                XYZ(0.to[T], 0.to[T], 0.to[T])
              }
              tmp
            }{(a,b) => XYZ(a.x + b.x, a.y + b.y, a.z + b.z)}
            // println(" " + b1x + "," + b1y + "," + b1z + " " + b0x + "," + b0y + "," + b0z + " = " + q_sum )
            b1_cube_contributions(p_idx) = q_sum
          }
          Foreach(p_range until density) { i => b1_cube_contributions(i) = XYZ(0.to[T], 0.to[T], 0.to[T]) } // Zero out untouched interactions          
          b1_cube_contributions
        }{(a,b) => XYZ(a.x + b.x, a.y + b.y, a.z + b.z)}

        Foreach(0 until density) { i => 
          force_x_sram(b0x,b0y,b0z,i) = b0_cube_forces(i).x
          force_y_sram(b0x,b0y,b0z,i) = b0_cube_forces(i).y
          force_z_sram(b0x,b0y,b0z,i) = b0_cube_forces(i).z
        }
      }}}
      force_x_dram(0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density_aligned par par_load) store force_x_sram
      force_y_dram(0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density_aligned par par_load) store force_y_sram
      force_z_dram(0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density_aligned par par_load) store force_z_sram

    }

    // No need to align after bug #195 fixed
    val force_x_received_aligned = getTensor4(force_x_dram)
    val force_y_received_aligned = getTensor4(force_y_dram)
    val force_z_received_aligned = getTensor4(force_z_dram)
    val force_x_received = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => force_x_received_aligned(i,j,k,l)}
    val force_y_received = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => force_y_received_aligned(i,j,k,l)}
    val force_z_received = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => force_z_received_aligned(i,j,k,l)}
    val raw_force_gold = loadCSV1D[T]("/remote/regression/data/machsuite/grid_gold.csv", "\n")
    val force_x_gold = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => raw_force_gold(i*BLOCK_SIDE*BLOCK_SIDE*density*3 + j*BLOCK_SIDE*density*3 + k*density*3 + 3*l)}
    val force_y_gold = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => raw_force_gold(i*BLOCK_SIDE*BLOCK_SIDE*density*3 + j*BLOCK_SIDE*density*3 + k*density*3 + 3*l+1)}
    val force_z_gold = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => raw_force_gold(i*BLOCK_SIDE*BLOCK_SIDE*density*3 + j*BLOCK_SIDE*density*3 + k*density*3 + 3*l+2)}


    printTensor4(force_x_gold, "Gold x:")
    printTensor4(force_x_received, "Received x:")
    printTensor4(force_y_gold, "Gold y:")
    printTensor4(force_y_received, "Received y:")
    printTensor4(force_z_gold, "Gold z:")
    printTensor4(force_z_received, "Received z:")

    val margin = 0.001.to[T]
    val cksumx = force_x_gold.zip(force_x_received){case (a,b) => abs(a - b) < margin}.reduce{_&&_}
    val cksumy = force_y_gold.zip(force_y_received){case (a,b) => abs(a - b) < margin}.reduce{_&&_}
    val cksumz = force_z_gold.zip(force_z_received){case (a,b) => abs(a - b) < margin}.reduce{_&&_}
    println("X: " + cksumx + ", Y:" + cksumy + ", Z: " + cksumz)
    val cksum = cksumx && cksumy && cksumz
    println("PASS: " + cksum + " (MD_Grid)")
  }
}      
object MD_Grid2 extends SpatialApp { // Regression (Dense) // Args: none
  override val target = Zynq


 /*
  
  Moleckaler Dynamics via the grid, a digital frontier
                                                                             
                                                                             
                                                                             
                                                                             
                                                                             
                            ←      BLOCK_SIDE     →                        
                ↗                                                                                                
                          __________________________________   
       BLOCK_SIDE        /                                  /|  
                        /                                  / |  
        ↙              /                                  /  |  
                      /_________________________________ /   |  
                     |           b1                     |    |  
           ↑         |        ..  ..  ..                |    |  
                     |       - - - - - -                |    |  
                     |      :``::``::``:                |    |  
           B         |    b1:..::__::..:b1              |    |  
           L         |      - - /_/| - -                |    |  
           O         |     :``:|b0||:``:                |    |  
           C         |   b1:..:|__|/:..:b1              |    |  
           K         |      - - - -  - -                |    |  
           |         |     :``::``: :``:                |    |  
           S         |   b1:..::..: :..:b1              |    |  
           I         |          b1                      |    |  
           D         |                                  |   /   
           E         |                                  |  /   
                     |                                  | /    
           ↓         |                                  |/     
                      ``````````````````````````````````       * Each b0 contains up to "density" number of atoms
                                                               * For each b0, and then for each atom in b0, compute this atom's
                                                                    interactions with all atoms in the adjacent (27) b1's
                                                               * One of the b1's will actually be b0, so skip this contribution                      
                                                                                                           
 */

  // Max pos seems to be about 19
  type T = FixPt[TRUE, _12, _20]
  @struct case class XYZ(x: T, y: T, z: T) 

  @virtualize
  def main() = {

    val N_ATOMS = 256
    val DOMAIN_EDGE = 20
    val BLOCK_SIDE = 4
    val density = 10
    val density_aligned = density + (8 - (density % 8))
    val lj1 = 1.5.to[T]
    val lj2 = 2.to[T]

    val par_load = 16 // Wider data type
    val par_store = 16 // Wider data type
    val loop_grid0_x = 1 // Temporarily broken because of some issue around #200
    val loop_grid0_y = 1 (1 -> 1 -> 16) 
    val loop_grid0_z = 2 (1 -> 1 -> 16)
    val loop_grid1_x = 1 (1 -> 1 -> 16)
    val loop_grid1_y = 1 (1 -> 1 -> 16)
    val loop_grid1_z = 3 (1 -> 1 -> 16)
    val loop_p =       2 (1 -> 1 -> 16)
    val loop_q =       3 (1 -> 1 -> 16)

    val raw_npoints = Array[Int](4,4,3,4,5,5,2,1,1,8,4,8,3,3,7,5,4,5,6,2,2,4,4,3,3,4,7,2,3,2,
                                 2,1,7,1,3,7,6,3,3,4,3,4,5,5,6,4,2,5,7,6,5,4,3,3,5,4,4,4,3,2,3,2,7,5)
    val npoints_data = raw_npoints.reshape(BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE)

    val raw_dvec = loadCSV1D[T]("/remote/regression/data/machsuite/grid_dvec.csv", "\n")
    // Strip x,y,z vectors from raw_dvec
    val dvec_x_data = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => raw_dvec(i*BLOCK_SIDE*BLOCK_SIDE*density*3 + j*BLOCK_SIDE*density*3 + k*density*3 + 3*l)}
    val dvec_y_data = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => raw_dvec(i*BLOCK_SIDE*BLOCK_SIDE*density*3 + j*BLOCK_SIDE*density*3 + k*density*3 + 3*l+1)}
    val dvec_z_data = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => raw_dvec(i*BLOCK_SIDE*BLOCK_SIDE*density*3 + j*BLOCK_SIDE*density*3 + k*density*3 + 3*l+2)}

    val dvec_x_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
    val dvec_y_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
    val dvec_z_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
    val force_x_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density_aligned)
    val force_y_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density_aligned)
    val force_z_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density_aligned)
    val npoints_dram = DRAM[Int](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE)

    setMem(dvec_x_dram, dvec_x_data)
    setMem(dvec_y_dram, dvec_y_data)
    setMem(dvec_z_dram, dvec_z_data)
    setMem(npoints_dram, npoints_data)

    Accel{
      val dvec_x_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
      val dvec_y_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
      val dvec_z_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
      val npoints_sram = SRAM[Int](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE)
      val force_x_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density_aligned)
      val force_y_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density_aligned)
      val force_z_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density_aligned)

      dvec_x_sram load dvec_x_dram(0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density par par_load)
      dvec_y_sram load dvec_y_dram(0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density par par_load)
      dvec_z_sram load dvec_z_dram(0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density par par_load)
      npoints_sram load npoints_dram(0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE par par_load)

      // Iterate over each block
      Foreach(BLOCK_SIDE by 1 par loop_grid0_x){b0x => Foreach(BLOCK_SIDE by 1 par loop_grid0_y){b0y => Foreach(BLOCK_SIDE by 1 par loop_grid0_z){b0z => 
        // Iterate over each point in this block, considering boundaries
        val b0_cube_forces = SRAM[XYZ](density)
        val b1x_start = max(0.to[Int],b0x-1.to[Int])
        val b1x_end = min(BLOCK_SIDE.to[Int], b0x+2.to[Int])
        val b1y_start = max(0.to[Int],b0y-1.to[Int])
        val b1y_end = min(BLOCK_SIDE.to[Int], b0y+2.to[Int])
        val b1z_start = max(0.to[Int],b0z-1.to[Int])
        val b1z_end = min(BLOCK_SIDE.to[Int], b0z+2.to[Int])
        MemReduce(b0_cube_forces)(b1x_start until b1x_end by 1, b1y_start until b1y_end by 1, b1z_start until b1z_end by 1 par loop_grid1_z) { (b1x, b1y, b1z) => 
          val b1_cube_contributions = SRAM.buffer[XYZ](density)
          // Iterate over points in b0
          val p_range = npoints_sram(b0x, b0y, b0z)
          val q_range = npoints_sram(b1x, b1y, b1z)
          Foreach(0 until p_range par loop_p) { p_idx =>
            val px = dvec_x_sram(b0x, b0y, b0z, p_idx)
            val py = dvec_y_sram(b0x, b0y, b0z, p_idx)
            val pz = dvec_z_sram(b0x, b0y, b0z, p_idx)
            val q_sum = Reg[XYZ](XYZ(0.to[T], 0.to[T], 0.to[T]))
            Reduce(q_sum)(0 until q_range par loop_q) { q_idx => 
              val qx = dvec_x_sram(b1x, b1y, b1z, q_idx)
              val qy = dvec_y_sram(b1x, b1y, b1z, q_idx)
              val qz = dvec_z_sram(b1x, b1y, b1z, q_idx)
              val tmp = if ( !(b0x == b1x && b0y == b1y && b0z == b1z && p_idx == q_idx) ) { // Skip self
                val delta = XYZ(px - qx, py - qy, pz - qz)
                val r2inv = (1.0.to[FixPt[TRUE,_4,_12]] / ( (delta.x*delta.x + delta.y*delta.y + delta.z*delta.z).to[FixPt[TRUE,_4,_12]] )).to[T]
                // Assume no cutoff and aways account for all nodes in area
                val r6inv = r2inv * r2inv * r2inv;
                val potential = r6inv*(lj1*r6inv - lj2);
                val force = r2inv*potential;
                XYZ(delta.x*force, delta.y*force, delta.z*force)
              } else {
                XYZ(0.to[T], 0.to[T], 0.to[T])
              }
              tmp
            }{(a,b) => XYZ(a.x + b.x, a.y + b.y, a.z + b.z)}
            // println(" " + b1x + "," + b1y + "," + b1z + " " + b0x + "," + b0y + "," + b0z + " = " + q_sum )
            b1_cube_contributions(p_idx) = q_sum
          }
          Foreach(p_range until density) { i => b1_cube_contributions(i) = XYZ(0.to[T], 0.to[T], 0.to[T]) } // Zero out untouched interactions          
          b1_cube_contributions
        }{(a,b) => XYZ(a.x + b.x, a.y + b.y, a.z + b.z)}

        Foreach(0 until density) { i => 
          force_x_sram(b0x,b0y,b0z,i) = b0_cube_forces(i).x
          force_y_sram(b0x,b0y,b0z,i) = b0_cube_forces(i).y
          force_z_sram(b0x,b0y,b0z,i) = b0_cube_forces(i).z
        }
      }}}
      force_x_dram(0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density_aligned par par_load) store force_x_sram
      force_y_dram(0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density_aligned par par_load) store force_y_sram
      force_z_dram(0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density_aligned par par_load) store force_z_sram

    }

    // No need to align after bug #195 fixed
    val force_x_received_aligned = getTensor4(force_x_dram)
    val force_y_received_aligned = getTensor4(force_y_dram)
    val force_z_received_aligned = getTensor4(force_z_dram)
    val force_x_received = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => force_x_received_aligned(i,j,k,l)}
    val force_y_received = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => force_y_received_aligned(i,j,k,l)}
    val force_z_received = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => force_z_received_aligned(i,j,k,l)}
    val raw_force_gold = loadCSV1D[T]("/remote/regression/data/machsuite/grid_gold.csv", "\n")
    val force_x_gold = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => raw_force_gold(i*BLOCK_SIDE*BLOCK_SIDE*density*3 + j*BLOCK_SIDE*density*3 + k*density*3 + 3*l)}
    val force_y_gold = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => raw_force_gold(i*BLOCK_SIDE*BLOCK_SIDE*density*3 + j*BLOCK_SIDE*density*3 + k*density*3 + 3*l+1)}
    val force_z_gold = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => raw_force_gold(i*BLOCK_SIDE*BLOCK_SIDE*density*3 + j*BLOCK_SIDE*density*3 + k*density*3 + 3*l+2)}


    printTensor4(force_x_gold, "Gold x:")
    printTensor4(force_x_received, "Received x:")
    printTensor4(force_y_gold, "Gold y:")
    printTensor4(force_y_received, "Received y:")
    printTensor4(force_z_gold, "Gold z:")
    printTensor4(force_z_received, "Received z:")

    val margin = 0.001.to[T]
    val cksumx = force_x_gold.zip(force_x_received){case (a,b) => abs(a - b) < margin}.reduce{_&&_}
    val cksumy = force_y_gold.zip(force_y_received){case (a,b) => abs(a - b) < margin}.reduce{_&&_}
    val cksumz = force_z_gold.zip(force_z_received){case (a,b) => abs(a - b) < margin}.reduce{_&&_}
    println("X: " + cksumx + ", Y:" + cksumy + ", Z: " + cksumz)
    val cksum = cksumx && cksumy && cksumz
    println("PASS: " + cksum + " (MD_Grid)")
  }
}      

object MD_Grid3 extends SpatialApp { // Regression (Dense) // Args: none
  override val target = Zynq


 /*
  
  Moleckaler Dynamics via the grid, a digital frontier
                                                                             
                                                                             
                                                                             
                                                                             
                                                                             
                            ←      BLOCK_SIDE     →                        
                ↗                                                                                                
                          __________________________________   
       BLOCK_SIDE        /                                  /|  
                        /                                  / |  
        ↙              /                                  /  |  
                      /_________________________________ /   |  
                     |           b1                     |    |  
           ↑         |        ..  ..  ..                |    |  
                     |       - - - - - -                |    |  
                     |      :``::``::``:                |    |  
           B         |    b1:..::__::..:b1              |    |  
           L         |      - - /_/| - -                |    |  
           O         |     :``:|b0||:``:                |    |  
           C         |   b1:..:|__|/:..:b1              |    |  
           K         |      - - - -  - -                |    |  
           |         |     :``::``: :``:                |    |  
           S         |   b1:..::..: :..:b1              |    |  
           I         |          b1                      |    |  
           D         |                                  |   /   
           E         |                                  |  /   
                     |                                  | /    
           ↓         |                                  |/     
                      ``````````````````````````````````       * Each b0 contains up to "density" number of atoms
                                                               * For each b0, and then for each atom in b0, compute this atom's
                                                                    interactions with all atoms in the adjacent (27) b1's
                                                               * One of the b1's will actually be b0, so skip this contribution                      
                                                                                                           
 */

  // Max pos seems to be about 19
  type T = FixPt[TRUE, _12, _20]
  @struct case class XYZ(x: T, y: T, z: T) 

  @virtualize
  def main() = {

    val N_ATOMS = 256
    val DOMAIN_EDGE = 20
    val BLOCK_SIDE = 4
    val density = 10
    val density_aligned = density + (8 - (density % 8))
    val lj1 = 1.5.to[T]
    val lj2 = 2.to[T]

    val par_load = 16 // Wider data type
    val par_store = 16 // Wider data type
    val loop_grid0_x = 1 // Temporarily broken because of some issue around #200
    val loop_grid0_y = 1 (1 -> 1 -> 16) 
    val loop_grid0_z = 1 (1 -> 1 -> 16)
    val loop_grid1_x = 1 (1 -> 1 -> 16)
    val loop_grid1_y = 2 (1 -> 1 -> 16)
    val loop_grid1_z = 1 (1 -> 1 -> 16)
    val loop_p =       2 (1 -> 1 -> 16)
    val loop_q =       8 (1 -> 1 -> 16)

    val raw_npoints = Array[Int](4,4,3,4,5,5,2,1,1,8,4,8,3,3,7,5,4,5,6,2,2,4,4,3,3,4,7,2,3,2,
                                 2,1,7,1,3,7,6,3,3,4,3,4,5,5,6,4,2,5,7,6,5,4,3,3,5,4,4,4,3,2,3,2,7,5)
    val npoints_data = raw_npoints.reshape(BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE)

    val raw_dvec = loadCSV1D[T]("/remote/regression/data/machsuite/grid_dvec.csv", "\n")
    // Strip x,y,z vectors from raw_dvec
    val dvec_x_data = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => raw_dvec(i*BLOCK_SIDE*BLOCK_SIDE*density*3 + j*BLOCK_SIDE*density*3 + k*density*3 + 3*l)}
    val dvec_y_data = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => raw_dvec(i*BLOCK_SIDE*BLOCK_SIDE*density*3 + j*BLOCK_SIDE*density*3 + k*density*3 + 3*l+1)}
    val dvec_z_data = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => raw_dvec(i*BLOCK_SIDE*BLOCK_SIDE*density*3 + j*BLOCK_SIDE*density*3 + k*density*3 + 3*l+2)}

    val dvec_x_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
    val dvec_y_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
    val dvec_z_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
    val force_x_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density_aligned)
    val force_y_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density_aligned)
    val force_z_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density_aligned)
    val npoints_dram = DRAM[Int](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE)

    setMem(dvec_x_dram, dvec_x_data)
    setMem(dvec_y_dram, dvec_y_data)
    setMem(dvec_z_dram, dvec_z_data)
    setMem(npoints_dram, npoints_data)

    Accel{
      val dvec_x_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
      val dvec_y_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
      val dvec_z_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
      val npoints_sram = SRAM[Int](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE)
      val force_x_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density_aligned)
      val force_y_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density_aligned)
      val force_z_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density_aligned)

      dvec_x_sram load dvec_x_dram(0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density par par_load)
      dvec_y_sram load dvec_y_dram(0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density par par_load)
      dvec_z_sram load dvec_z_dram(0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density par par_load)
      npoints_sram load npoints_dram(0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE par par_load)

      // Iterate over each block
      Foreach(BLOCK_SIDE by 1 par loop_grid0_x){b0x => Foreach(BLOCK_SIDE by 1 par loop_grid0_y){b0y => Foreach(BLOCK_SIDE by 1 par loop_grid0_z){b0z => 
        // Iterate over each point in this block, considering boundaries
        val b0_cube_forces = SRAM[XYZ](density)
        val b1x_start = max(0.to[Int],b0x-1.to[Int])
        val b1x_end = min(BLOCK_SIDE.to[Int], b0x+2.to[Int])
        val b1y_start = max(0.to[Int],b0y-1.to[Int])
        val b1y_end = min(BLOCK_SIDE.to[Int], b0y+2.to[Int])
        val b1z_start = max(0.to[Int],b0z-1.to[Int])
        val b1z_end = min(BLOCK_SIDE.to[Int], b0z+2.to[Int])
        MemReduce(b0_cube_forces)(b1x_start until b1x_end by 1, b1y_start until b1y_end by 1, b1z_start until b1z_end by 1 par loop_grid1_z) { (b1x, b1y, b1z) => 
          val b1_cube_contributions = SRAM.buffer[XYZ](density)
          // Iterate over points in b0
          val p_range = npoints_sram(b0x, b0y, b0z)
          val q_range = npoints_sram(b1x, b1y, b1z)
          Foreach(0 until p_range par loop_p) { p_idx =>
            val px = dvec_x_sram(b0x, b0y, b0z, p_idx)
            val py = dvec_y_sram(b0x, b0y, b0z, p_idx)
            val pz = dvec_z_sram(b0x, b0y, b0z, p_idx)
            val q_sum = Reg[XYZ](XYZ(0.to[T], 0.to[T], 0.to[T]))
            Reduce(q_sum)(0 until q_range par loop_q) { q_idx => 
              val qx = dvec_x_sram(b1x, b1y, b1z, q_idx)
              val qy = dvec_y_sram(b1x, b1y, b1z, q_idx)
              val qz = dvec_z_sram(b1x, b1y, b1z, q_idx)
              val tmp = if ( !(b0x == b1x && b0y == b1y && b0z == b1z && p_idx == q_idx) ) { // Skip self
                val delta = XYZ(px - qx, py - qy, pz - qz)
                val r2inv = (1.0.to[FixPt[TRUE,_4,_12]] / ( (delta.x*delta.x + delta.y*delta.y + delta.z*delta.z).to[FixPt[TRUE,_4,_12]] )).to[T]
                // Assume no cutoff and aways account for all nodes in area
                val r6inv = r2inv * r2inv * r2inv;
                val potential = r6inv*(lj1*r6inv - lj2);
                val force = r2inv*potential;
                XYZ(delta.x*force, delta.y*force, delta.z*force)
              } else {
                XYZ(0.to[T], 0.to[T], 0.to[T])
              }
              tmp
            }{(a,b) => XYZ(a.x + b.x, a.y + b.y, a.z + b.z)}
            // println(" " + b1x + "," + b1y + "," + b1z + " " + b0x + "," + b0y + "," + b0z + " = " + q_sum )
            b1_cube_contributions(p_idx) = q_sum
          }
          Foreach(p_range until density) { i => b1_cube_contributions(i) = XYZ(0.to[T], 0.to[T], 0.to[T]) } // Zero out untouched interactions          
          b1_cube_contributions
        }{(a,b) => XYZ(a.x + b.x, a.y + b.y, a.z + b.z)}

        Foreach(0 until density) { i => 
          force_x_sram(b0x,b0y,b0z,i) = b0_cube_forces(i).x
          force_y_sram(b0x,b0y,b0z,i) = b0_cube_forces(i).y
          force_z_sram(b0x,b0y,b0z,i) = b0_cube_forces(i).z
        }
      }}}
      force_x_dram(0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density_aligned par par_load) store force_x_sram
      force_y_dram(0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density_aligned par par_load) store force_y_sram
      force_z_dram(0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density_aligned par par_load) store force_z_sram

    }

    // No need to align after bug #195 fixed
    val force_x_received_aligned = getTensor4(force_x_dram)
    val force_y_received_aligned = getTensor4(force_y_dram)
    val force_z_received_aligned = getTensor4(force_z_dram)
    val force_x_received = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => force_x_received_aligned(i,j,k,l)}
    val force_y_received = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => force_y_received_aligned(i,j,k,l)}
    val force_z_received = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => force_z_received_aligned(i,j,k,l)}
    val raw_force_gold = loadCSV1D[T]("/remote/regression/data/machsuite/grid_gold.csv", "\n")
    val force_x_gold = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => raw_force_gold(i*BLOCK_SIDE*BLOCK_SIDE*density*3 + j*BLOCK_SIDE*density*3 + k*density*3 + 3*l)}
    val force_y_gold = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => raw_force_gold(i*BLOCK_SIDE*BLOCK_SIDE*density*3 + j*BLOCK_SIDE*density*3 + k*density*3 + 3*l+1)}
    val force_z_gold = (0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::BLOCK_SIDE, 0::density){(i,j,k,l) => raw_force_gold(i*BLOCK_SIDE*BLOCK_SIDE*density*3 + j*BLOCK_SIDE*density*3 + k*density*3 + 3*l+2)}


    printTensor4(force_x_gold, "Gold x:")
    printTensor4(force_x_received, "Received x:")
    printTensor4(force_y_gold, "Gold y:")
    printTensor4(force_y_received, "Received y:")
    printTensor4(force_z_gold, "Gold z:")
    printTensor4(force_z_received, "Received z:")

    val margin = 0.001.to[T]
    val cksumx = force_x_gold.zip(force_x_received){case (a,b) => abs(a - b) < margin}.reduce{_&&_}
    val cksumy = force_y_gold.zip(force_y_received){case (a,b) => abs(a - b) < margin}.reduce{_&&_}
    val cksumz = force_z_gold.zip(force_z_received){case (a,b) => abs(a - b) < margin}.reduce{_&&_}
    println("X: " + cksumx + ", Y:" + cksumy + ", Z: " + cksumz)
    val cksum = cksumx && cksumy && cksumz
    println("PASS: " + cksum + " (MD_Grid)")
  }
}      


object GEMM_Blocked1 extends SpatialApp { // Regression (Dense) // Args: none
  override val target = Zynq
                                                                                                  
                                                                                                  
 /*                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      


         # Loops jj and kk

                                                                                jj                                       
                                                                  ...............↓............................. 
                                                                 .               .          .                 . 
                                                                 .               . ← tile → .                 . 
                                                                 .               .          .                 . 
                                                                 .      B        .          .                 . 
                                                                 .               .          .                 . 
                                                              kk .               .          .                 . 
                                                               ↳ .................__________................... 
                                                                 .               |          |                 . 
                                                                 .               | b_sram   |      ↑          . 
                                                                 .               |          |      tile       . 
                                                                 .               |          |      ↓          . 
                                                                 ................|__________|.................. 
                                                                 .               .          .                 . 
                                                                 .               .    |     .                 . 
                                                                 .               .    |     .                 . 
                                                                 .               .    ↓     .                 . 
                                                                 .               .          .                 . 
                                                                 ..............................................
                    kk ---→                                                                                           
      _______________↓____________________________                ................__________...................          
     |               |          |                 |              .               |          |                  .      ↑   
     |               |          |                 |              .               |          |                  .      |   
     |               | ← tile → |                 |              .               |          |                  .      |   
     |      A        |          |                 |              .               |          |                  .      |   
     |               |          |                 |              .           C   |          |                  .      |   
     |               |          |                 |              .               |  c_col   |                  .      |   
     |               |          |                 |              .               |          |                  .      |   
     |               |          |                 |              .               |          |                  .      |    
     |               |          |                 |              .               |          |                  .
     |               |          |                 |              .               |          |                  .     dim  
     |               |          |                 |              .               |          |                  .         
     |               |          |                 |              .               |          |                  .      |   
     |               |          |                 |              .               |          |                  .      |   
     |               |          |                 |              .               |          |                  .      |   
     |               |          |                 |              .               |          |                  .      |   
     |               |          |                 |              .               |          |                  .      |   
     |               |          |                 |              .               |          |                  .      |   
     |_______________|__________|_________________|              ................|__________|...................      ↓   
                                                                                             
                                                                  ←----------------- dim -------------------→

        # Loop i                                                          
                                          
                                                                               jj                                       
                                                                 ...............↓............................. 
                                                                .               .          .                 . 
                                                                .               . ← tile → .                 . 
                                                                .               .          .                 . 
                                                                .      B        .          .                 . 
                                                                .               .          .                 . 
                                                             kk .               .          .                 . 
                                                              ↳ .................__________................... 
                                                                .               |          |                 . 
                                                                .               | b_sram   |      ↑          . 
                                                                .               |          |      tile       . 
                                                                .               |          |      ↓          . 
                                                                ................|__________|.................. 
                                                                .               .          .                 . 
                                                                .               .          .                 . 
                                                                .               .          .                 . 
                                                                .               .          .                 . 
                                                                .               .          .                 . 
                                                                ..............................................
                      kk                                                                                               
        ...............↓.............................           ..............................................          
       .               .          .                 .          .               .          .                  .     
       .               .          .                 .          .               .          .                  .     
       .               . ← tile → .                 .          .               .          .                  .     
       .      A        .          .                 .          .               .          .                  .     
       .               .          .                 .          .           C   .          .                  .     
     i .               .          .                 .          .               .          .                  .     
     ↳ .................__________...................          .................__________....................     
       .               |_a_sram___|                 .          .               |__c_tmp___|                  .     
       .```````````````.          .`````````````````.          .```````````````.          .``````````````````.
       .               .    |     .                 .          .               .          .                  .     
       .               .    |     .                 .          .               .          .                  .     
       .               .    ↓     .                 .          .               .          .                  .     
       .               .          .                 .          .               .          .                  .     
       .               .          .                 .          .               .          .                  .     
       .               .          .                 .          .               .          .                  .     
       .               .          .                 .          .               .          .                  .     
       .               .          .                 .          .               .          .                  .     
       ..............................................          ...............................................     
                                                                                           
                                                                

        
        # Loop k
                                                                               jj                                       
                                                                 ...............↓............................. 
                                                                .               .          .                 . 
                                                                .               . ← tile → .                 . 
                                                                .               .          .                 . 
                                                                .      B        .          .                 . 
                                                                .               .          .                 . 
                                                             kk .               .          .                 . 
                                                              ↳ .................__________................... 
                                                                .             k |          |                 . 
                                                                .             ↳ | b_sram   |      ↑          . 
                                                                .               |          |      tile       . 
                                                                .               |          |      ↓          . 
                                                                ................|__________|.................. 
                                                                .               .          .                 . 
                                                                .               .          .                 . 
                                                                .               .          .                 . 
                                                                .               .          .                 . 
                                                                .               .          .                 . 
                                                                ..............................................
                      kk                                                                                               
        ...............↓.............................            ..............................................         
       .               .          .                 .           .               .          .                  .   
       .               .          .                 .           .               .          .                  .   
       .               . ← tile → .                 .           .               .          .                  .   
       .      A        .          .                 .           .               .          .                  .   
       .               .          .                 .           .           C   .          .                  .   
     i .               .          .                 .           .               .          .                  .   
     ↳ .................__________...............raw_values....           .................__________....................   
       .               |_O________|                 .           .               |__c_tmp___|                  .   
       .```````````````. ↑        .`````````````````.           .```````````````.          .``````````````````.
       .               . k  -->   .                 .           .               .          .                  .   
       .               .          .                 .           .               .          .                  .   
       .               .          .                 .           .               .          .                  .   
       .               .          .                 .           .               .          .                  .   
       .               .          .                 .           .               .          .                  .   
       .               .          .                 .           .               .          .                  .   
       .               .          .                 .           .               .          .                  .   
       .               .          .                 .           .               .          .                  .   
       ..............................................           ...............................................   
                                                                                           
                                                              


            # Loop j
                                                                              jj                                       
                                                                ...............↓............................. 
                                                               .               .          .                 . 
                                                               .               . ← tile → .                 . 
                                                               .               .          .                 . 
                                                               .      B        .          .                 . 
                                                               .               .          .                 . 
                                                            kk .               .  j -->   .                 . 
                                                             ↳ .................__↓_______................... 
                                                               .             k |          |                 . 
                                                               .             ↳ |  O       |      ↑          . 
                                                               .               |          |      tile       . 
                                                               .               |  b_sram  |      ↓          . 
                                                               ................|__________|.................. 
                                                               .               .          .                 . 
                                                               .               .          .                 . 
                                                               .               .          .                 . 
                                                               .               .          .                 . 
                                                               .               .          .                 . 
                                                               ..............................................
                     kk                                                                                               
       ...............↓.............................           ..............................................         
      .               .          .                 .          .               .          .                  .     
      .               .          .                 .          .               .          .                  .     
      .               . ← tile → .                 .          .               .          .                  .     
      .      A        .          .                 .          .               .          .                  .     
      .               .          .                 .          .           C   .          .                  .     
    i .               .          .                 .          .               .          .                  .     
    ↳ .................__________...................          .................__________....................     
      .               |_O________|                 .          .               |__O_-->___|                  .     
      .```````````````. ↑        .`````````````````.          .```````````````.          .``````````````````.
      .               . k        .                 .          .               .          .                  .     
      .               .          .                 .          .               .          .                  .     
      .               .          .                 .          .               .          .                  .     
      .               .          .                 .          .               .          .                  .     
      .               .          .                 .          .               .          .                  .     
      .               .          .                 .          .               .          .                  .     
      .               .          .                 .          .               .          .                  .     
      .               .          .                 .          .               .          .                  .     
      ..............................................          ...............................................     
                                                                                          
                                                                
    CONCERNS: We need to figure out how HLS is actually managing the srams, or make our management better  
              We cannot do unaligned stores yet, so tilesize of 8 won't work unless we keep ts 16 of c_sram onchip                                                                                          
 */
  type T = FixPt[TRUE,_16,_16] // Fatter type so that tileSize is burst aligned

  @virtualize
  def main() = {

    val dim_arg = args(0).to[Int]
    val dim = ArgIn[Int]
    setArg(dim, dim_arg)
    val tileSize = 16 (16 -> 16 -> 128)
    val i_tileSize = 64 (64 -> 16 -> 128)
    val par_load = 16
    val par_store = 16
    val loop_jj    = 1 // (1 -> 1 -> dim/tileSize) // THIS PAR DOES NOT WORK UNTIL BUG #205 IS FIXED
    val loop_ii    = 1 // not sure if this one works
    val loop_kk    = 1 (1 -> 1 -> 8)
    val loop_i     = 2 (1 -> 1 -> 32)
    val loop_k     = 1 (1 -> 1 -> 16)
    val loop_j     = 3 (1 -> 1 -> 16)
    val reduce_col = 4 (1 -> 1 -> 16)
    val reduce_tmp = 4 (1 -> 1 -> 16)

    // val a_data = loadCSV1D[T]("/remote/regression/data/machsuite/gemm_a.csv", "\n").reshape(dim,dim)
    // val b_data = loadCSV1D[T]("/remote/regression/data/machsuite/gemm_b.csv", "\n").reshape(dim,dim)
    val a_data = (0::dim_arg,0::dim_arg){(i,j) => random[T](5)}
    val b_data = (0::dim_arg,0::dim_arg){(i,j) => random[T](5)}
    val c_init = (0::dim_arg, 0::dim_arg){(i,j) => 0.to[T]}
    val a_dram = DRAM[T](dim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](dim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)
    setMem(c_dram, c_init)

    Accel{

      Foreach(dim by i_tileSize par loop_ii) { ii => // this loop defenitilely cant be parallelized right now
        Foreach(dim by tileSize par loop_jj) { jj => 
          val c_col = SRAM[T](i_tileSize,tileSize)
          MemReduce(c_col par reduce_col)(dim by tileSize par loop_kk) { kk => 
            val c_col_partial = SRAM[T](i_tileSize,tileSize)
            val b_sram = SRAM[T](tileSize,tileSize)
            b_sram load b_dram(kk::kk.to[Index]+tileSize, jj::jj.to[Index]+tileSize par par_load)
            Foreach(i_tileSize by 1 par loop_i) { i => 
              val a_sram = SRAM[T](tileSize)
              a_sram load a_dram(ii+i, kk::kk.to[Index]+tileSize)
              val c_tmp = SRAM[T](tileSize)
              MemReduce(c_tmp par reduce_tmp)(tileSize by 1 par loop_k) { k => 
                val c_tmp_partial = SRAM[T](tileSize)
                val temp_a = a_sram(k)
                Foreach(tileSize by 1 par loop_j) { j => 
                  c_tmp_partial(j) = b_sram(k, j) * temp_a
                }
                c_tmp_partial
              }{_+_}
            Foreach(tileSize by 1){cpy => c_col_partial(i,cpy) = c_tmp(cpy)}
            }
          c_col_partial
          }{_+_}
          c_dram(ii::ii.to[Index]+i_tileSize, jj::jj.to[Index]+tileSize par par_store) store c_col
        }
      }
    }

    // val c_gold = loadCSV1D[T]("/remote/regression/data/machsuite/gemm_gold.csv", "\n").reshape(dim,dim)
    val c_gold = (0::dim_arg,0::dim_arg){(i,j) => 
      Array.tabulate(dim_arg){k => a_data(i,k) * b_data(k,j)}.reduce{_+_}
    }
    val c_result = getMatrix(c_dram)

    printMatrix(c_gold, "C Gold: ")
    printMatrix(c_result, "C Result: ")

    val margin = 0.5.to[T]
    val cksum = c_gold.zip(c_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (GEMM_Blocked)")
  }
}
object GEMM_Blocked2 extends SpatialApp { // Regression (Dense) // Args: none
  override val target = Zynq
                                                                                                  
                                                                                                  
 /*                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      


         # Loops jj and kk

                                                                                jj                                       
                                                                  ...............↓............................. 
                                                                 .               .          .                 . 
                                                                 .               . ← tile → .                 . 
                                                                 .               .          .                 . 
                                                                 .      B        .          .                 . 
                                                                 .               .          .                 . 
                                                              kk .               .          .                 . 
                                                               ↳ .................__________................... 
                                                                 .               |          |                 . 
                                                                 .               | b_sram   |      ↑          . 
                                                                 .               |          |      tile       . 
                                                                 .               |          |      ↓          . 
                                                                 ................|__________|.................. 
                                                                 .               .          .                 . 
                                                                 .               .    |     .                 . 
                                                                 .               .    |     .                 . 
                                                                 .               .    ↓     .                 . 
                                                                 .               .          .                 . 
                                                                 ..............................................
                    kk ---→                                                                                           
      _______________↓____________________________                ................__________...................          
     |               |          |                 |              .               |          |                  .      ↑   
     |               |          |                 |              .               |          |                  .      |   
     |               | ← tile → |                 |              .               |          |                  .      |   
     |      A        |          |                 |              .               |          |                  .      |   
     |               |          |                 |              .           C   |          |                  .      |   
     |               |          |                 |              .               |  c_col   |                  .      |   
     |               |          |                 |              .               |          |                  .      |   
     |               |          |                 |              .               |          |                  .      |    
     |               |          |                 |              .               |          |                  .
     |               |          |                 |              .               |          |                  .     dim  
     |               |          |                 |              .               |          |                  .         
     |               |          |                 |              .               |          |                  .      |   
     |               |          |                 |              .               |          |                  .      |   
     |               |          |                 |              .               |          |                  .      |   
     |               |          |                 |              .               |          |                  .      |   
     |               |          |                 |              .               |          |                  .      |   
     |               |          |                 |              .               |          |                  .      |   
     |_______________|__________|_________________|              ................|__________|...................      ↓   
                                                                                             
                                                                  ←----------------- dim -------------------→

        # Loop i                                                          
                                          
                                                                               jj                                       
                                                                 ...............↓............................. 
                                                                .               .          .                 . 
                                                                .               . ← tile → .                 . 
                                                                .               .          .                 . 
                                                                .      B        .          .                 . 
                                                                .               .          .                 . 
                                                             kk .               .          .                 . 
                                                              ↳ .................__________................... 
                                                                .               |          |                 . 
                                                                .               | b_sram   |      ↑          . 
                                                                .               |          |      tile       . 
                                                                .               |          |      ↓          . 
                                                                ................|__________|.................. 
                                                                .               .          .                 . 
                                                                .               .          .                 . 
                                                                .               .          .                 . 
                                                                .               .          .                 . 
                                                                .               .          .                 . 
                                                                ..............................................
                      kk                                                                                               
        ...............↓.............................           ..............................................          
       .               .          .                 .          .               .          .                  .     
       .               .          .                 .          .               .          .                  .     
       .               . ← tile → .                 .          .               .          .                  .     
       .      A        .          .                 .          .               .          .                  .     
       .               .          .                 .          .           C   .          .                  .     
     i .               .          .                 .          .               .          .                  .     
     ↳ .................__________...................          .................__________....................     
       .               |_a_sram___|                 .          .               |__c_tmp___|                  .     
       .```````````````.          .`````````````````.          .```````````````.          .``````````````````.
       .               .    |     .                 .          .               .          .                  .     
       .               .    |     .                 .          .               .          .                  .     
       .               .    ↓     .                 .          .               .          .                  .     
       .               .          .                 .          .               .          .                  .     
       .               .          .                 .          .               .          .                  .     
       .               .          .                 .          .               .          .                  .     
       .               .          .                 .          .               .          .                  .     
       .               .          .                 .          .               .          .                  .     
       ..............................................          ...............................................     
                                                                                           
                                                                

        
        # Loop k
                                                                               jj                                       
                                                                 ...............↓............................. 
                                                                .               .          .                 . 
                                                                .               . ← tile → .                 . 
                                                                .               .          .                 . 
                                                                .      B        .          .                 . 
                                                                .               .          .                 . 
                                                             kk .               .          .                 . 
                                                              ↳ .................__________................... 
                                                                .             k |          |                 . 
                                                                .             ↳ | b_sram   |      ↑          . 
                                                                .               |          |      tile       . 
                                                                .               |          |      ↓          . 
                                                                ................|__________|.................. 
                                                                .               .          .                 . 
                                                                .               .          .                 . 
                                                                .               .          .                 . 
                                                                .               .          .                 . 
                                                                .               .          .                 . 
                                                                ..............................................
                      kk                                                                                               
        ...............↓.............................            ..............................................         
       .               .          .                 .           .               .          .                  .   
       .               .          .                 .           .               .          .                  .   
       .               . ← tile → .                 .           .               .          .                  .   
       .      A        .          .                 .           .               .          .                  .   
       .               .          .                 .           .           C   .          .                  .   
     i .               .          .                 .           .               .          .                  .   
     ↳ .................__________...............raw_values....           .................__________....................   
       .               |_O________|                 .           .               |__c_tmp___|                  .   
       .```````````````. ↑        .`````````````````.           .```````````````.          .``````````````````.
       .               . k  -->   .                 .           .               .          .                  .   
       .               .          .                 .           .               .          .                  .   
       .               .          .                 .           .               .          .                  .   
       .               .          .                 .           .               .          .                  .   
       .               .          .                 .           .               .          .                  .   
       .               .          .                 .           .               .          .                  .   
       .               .          .                 .           .               .          .                  .   
       .               .          .                 .           .               .          .                  .   
       ..............................................           ...............................................   
                                                                                           
                                                              


            # Loop j
                                                                              jj                                       
                                                                ...............↓............................. 
                                                               .               .          .                 . 
                                                               .               . ← tile → .                 . 
                                                               .               .          .                 . 
                                                               .      B        .          .                 . 
                                                               .               .          .                 . 
                                                            kk .               .  j -->   .                 . 
                                                             ↳ .................__↓_______................... 
                                                               .             k |          |                 . 
                                                               .             ↳ |  O       |      ↑          . 
                                                               .               |          |      tile       . 
                                                               .               |  b_sram  |      ↓          . 
                                                               ................|__________|.................. 
                                                               .               .          .                 . 
                                                               .               .          .                 . 
                                                               .               .          .                 . 
                                                               .               .          .                 . 
                                                               .               .          .                 . 
                                                               ..............................................
                     kk                                                                                               
       ...............↓.............................           ..............................................         
      .               .          .                 .          .               .          .                  .     
      .               .          .                 .          .               .          .                  .     
      .               . ← tile → .                 .          .               .          .                  .     
      .      A        .          .                 .          .               .          .                  .     
      .               .          .                 .          .           C   .          .                  .     
    i .               .          .                 .          .               .          .                  .     
    ↳ .................__________...................          .................__________....................     
      .               |_O________|                 .          .               |__O_-->___|                  .     
      .```````````````. ↑        .`````````````````.          .```````````````.          .``````````````````.
      .               . k        .                 .          .               .          .                  .     
      .               .          .                 .          .               .          .                  .     
      .               .          .                 .          .               .          .                  .     
      .               .          .                 .          .               .          .                  .     
      .               .          .                 .          .               .          .                  .     
      .               .          .                 .          .               .          .                  .     
      .               .          .                 .          .               .          .                  .     
      .               .          .                 .          .               .          .                  .     
      ..............................................          ...............................................     
                                                                                          
                                                                
    CONCERNS: We need to figure out how HLS is actually managing the srams, or make our management better  
              We cannot do unaligned stores yet, so tilesize of 8 won't work unless we keep ts 16 of c_sram onchip                                                                                          
 */
  type T = FixPt[TRUE,_16,_16] // Fatter type so that tileSize is burst aligned

  @virtualize
  def main() = {

    val dim_arg = args(0).to[Int]
    val dim = ArgIn[Int]
    setArg(dim, dim_arg)
    val tileSize = 16 (16 -> 16 -> 128)
    val i_tileSize = 64 (64 -> 16 -> 128)
    val par_load = 16
    val par_store = 16
    val loop_jj    = 1 // (1 -> 1 -> dim/tileSize) // THIS PAR DOES NOT WORK UNTIL BUG #205 IS FIXED
    val loop_ii    = 1 // not sure if this one works
    val loop_kk    = 1 (1 -> 1 -> 8)
    val loop_i     = 1 (1 -> 1 -> 32)
    val loop_k     = 4 (1 -> 1 -> 16)
    val loop_j     = 2 (1 -> 1 -> 16)
    val reduce_col = 2 (1 -> 1 -> 16)
    val reduce_tmp = 4 (1 -> 1 -> 16)

    // val a_data = loadCSV1D[T]("/remote/regression/data/machsuite/gemm_a.csv", "\n").reshape(dim,dim)
    // val b_data = loadCSV1D[T]("/remote/regression/data/machsuite/gemm_b.csv", "\n").reshape(dim,dim)
    val a_data = (0::dim_arg,0::dim_arg){(i,j) => random[T](5)}
    val b_data = (0::dim_arg,0::dim_arg){(i,j) => random[T](5)}
    val c_init = (0::dim_arg, 0::dim_arg){(i,j) => 0.to[T]}
    val a_dram = DRAM[T](dim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](dim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)
    setMem(c_dram, c_init)

    Accel{

      Foreach(dim by i_tileSize par loop_ii) { ii => // this loop defenitilely cant be parallelized right now
        Foreach(dim by tileSize par loop_jj) { jj => 
          val c_col = SRAM[T](i_tileSize,tileSize)
          MemReduce(c_col par reduce_col)(dim by tileSize par loop_kk) { kk => 
            val c_col_partial = SRAM[T](i_tileSize,tileSize)
            val b_sram = SRAM[T](tileSize,tileSize)
            b_sram load b_dram(kk::kk.to[Index]+tileSize, jj::jj.to[Index]+tileSize par par_load)
            Foreach(i_tileSize by 1 par loop_i) { i => 
              val a_sram = SRAM[T](tileSize)
              a_sram load a_dram(ii+i, kk::kk.to[Index]+tileSize)
              val c_tmp = SRAM[T](tileSize)
              MemReduce(c_tmp par reduce_tmp)(tileSize by 1 par loop_k) { k => 
                val c_tmp_partial = SRAM[T](tileSize)
                val temp_a = a_sram(k)
                Foreach(tileSize by 1 par loop_j) { j => 
                  c_tmp_partial(j) = b_sram(k, j) * temp_a
                }
                c_tmp_partial
              }{_+_}
            Foreach(tileSize by 1){cpy => c_col_partial(i,cpy) = c_tmp(cpy)}
            }
          c_col_partial
          }{_+_}
          c_dram(ii::ii.to[Index]+i_tileSize, jj::jj.to[Index]+tileSize par par_store) store c_col
        }
      }
    }

    // val c_gold = loadCSV1D[T]("/remote/regression/data/machsuite/gemm_gold.csv", "\n").reshape(dim,dim)
    val c_gold = (0::dim_arg,0::dim_arg){(i,j) => 
      Array.tabulate(dim_arg){k => a_data(i,k) * b_data(k,j)}.reduce{_+_}
    }
    val c_result = getMatrix(c_dram)

    printMatrix(c_gold, "C Gold: ")
    printMatrix(c_result, "C Result: ")

    val margin = 0.5.to[T]
    val cksum = c_gold.zip(c_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (GEMM_Blocked)")
  }
}

object GEMM_Blocked3 extends SpatialApp { // Regression (Dense) // Args: none
  override val target = Zynq
                                                                                                  
                                                                                                  
 /*                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      


         # Loops jj and kk

                                                                                jj                                       
                                                                  ...............↓............................. 
                                                                 .               .          .                 . 
                                                                 .               . ← tile → .                 . 
                                                                 .               .          .                 . 
                                                                 .      B        .          .                 . 
                                                                 .               .          .                 . 
                                                              kk .               .          .                 . 
                                                               ↳ .................__________................... 
                                                                 .               |          |                 . 
                                                                 .               | b_sram   |      ↑          . 
                                                                 .               |          |      tile       . 
                                                                 .               |          |      ↓          . 
                                                                 ................|__________|.................. 
                                                                 .               .          .                 . 
                                                                 .               .    |     .                 . 
                                                                 .               .    |     .                 . 
                                                                 .               .    ↓     .                 . 
                                                                 .               .          .                 . 
                                                                 ..............................................
                    kk ---→                                                                                           
      _______________↓____________________________                ................__________...................          
     |               |          |                 |              .               |          |                  .      ↑   
     |               |          |                 |              .               |          |                  .      |   
     |               | ← tile → |                 |              .               |          |                  .      |   
     |      A        |          |                 |              .               |          |                  .      |   
     |               |          |                 |              .           C   |          |                  .      |   
     |               |          |                 |              .               |  c_col   |                  .      |   
     |               |          |                 |              .               |          |                  .      |   
     |               |          |                 |              .               |          |                  .      |    
     |               |          |                 |              .               |          |                  .
     |               |          |                 |              .               |          |                  .     dim  
     |               |          |                 |              .               |          |                  .         
     |               |          |                 |              .               |          |                  .      |   
     |               |          |                 |              .               |          |                  .      |   
     |               |          |                 |              .               |          |                  .      |   
     |               |          |                 |              .               |          |                  .      |   
     |               |          |                 |              .               |          |                  .      |   
     |               |          |                 |              .               |          |                  .      |   
     |_______________|__________|_________________|              ................|__________|...................      ↓   
                                                                                             
                                                                  ←----------------- dim -------------------→

        # Loop i                                                          
                                          
                                                                               jj                                       
                                                                 ...............↓............................. 
                                                                .               .          .                 . 
                                                                .               . ← tile → .                 . 
                                                                .               .          .                 . 
                                                                .      B        .          .                 . 
                                                                .               .          .                 . 
                                                             kk .               .          .                 . 
                                                              ↳ .................__________................... 
                                                                .               |          |                 . 
                                                                .               | b_sram   |      ↑          . 
                                                                .               |          |      tile       . 
                                                                .               |          |      ↓          . 
                                                                ................|__________|.................. 
                                                                .               .          .                 . 
                                                                .               .          .                 . 
                                                                .               .          .                 . 
                                                                .               .          .                 . 
                                                                .               .          .                 . 
                                                                ..............................................
                      kk                                                                                               
        ...............↓.............................           ..............................................          
       .               .          .                 .          .               .          .                  .     
       .               .          .                 .          .               .          .                  .     
       .               . ← tile → .                 .          .               .          .                  .     
       .      A        .          .                 .          .               .          .                  .     
       .               .          .                 .          .           C   .          .                  .     
     i .               .          .                 .          .               .          .                  .     
     ↳ .................__________...................          .................__________....................     
       .               |_a_sram___|                 .          .               |__c_tmp___|                  .     
       .```````````````.          .`````````````````.          .```````````````.          .``````````````````.
       .               .    |     .                 .          .               .          .                  .     
       .               .    |     .                 .          .               .          .                  .     
       .               .    ↓     .                 .          .               .          .                  .     
       .               .          .                 .          .               .          .                  .     
       .               .          .                 .          .               .          .                  .     
       .               .          .                 .          .               .          .                  .     
       .               .          .                 .          .               .          .                  .     
       .               .          .                 .          .               .          .                  .     
       ..............................................          ...............................................     
                                                                                           
                                                                

        
        # Loop k
                                                                               jj                                       
                                                                 ...............↓............................. 
                                                                .               .          .                 . 
                                                                .               . ← tile → .                 . 
                                                                .               .          .                 . 
                                                                .      B        .          .                 . 
                                                                .               .          .                 . 
                                                             kk .               .          .                 . 
                                                              ↳ .................__________................... 
                                                                .             k |          |                 . 
                                                                .             ↳ | b_sram   |      ↑          . 
                                                                .               |          |      tile       . 
                                                                .               |          |      ↓          . 
                                                                ................|__________|.................. 
                                                                .               .          .                 . 
                                                                .               .          .                 . 
                                                                .               .          .                 . 
                                                                .               .          .                 . 
                                                                .               .          .                 . 
                                                                ..............................................
                      kk                                                                                               
        ...............↓.............................            ..............................................         
       .               .          .                 .           .               .          .                  .   
       .               .          .                 .           .               .          .                  .   
       .               . ← tile → .                 .           .               .          .                  .   
       .      A        .          .                 .           .               .          .                  .   
       .               .          .                 .           .           C   .          .                  .   
     i .               .          .                 .           .               .          .                  .   
     ↳ .................__________...............raw_values....           .................__________....................   
       .               |_O________|                 .           .               |__c_tmp___|                  .   
       .```````````````. ↑        .`````````````````.           .```````````````.          .``````````````````.
       .               . k  -->   .                 .           .               .          .                  .   
       .               .          .                 .           .               .          .                  .   
       .               .          .                 .           .               .          .                  .   
       .               .          .                 .           .               .          .                  .   
       .               .          .                 .           .               .          .                  .   
       .               .          .                 .           .               .          .                  .   
       .               .          .                 .           .               .          .                  .   
       .               .          .                 .           .               .          .                  .   
       ..............................................           ...............................................   
                                                                                           
                                                              


            # Loop j
                                                                              jj                                       
                                                                ...............↓............................. 
                                                               .               .          .                 . 
                                                               .               . ← tile → .                 . 
                                                               .               .          .                 . 
                                                               .      B        .          .                 . 
                                                               .               .          .                 . 
                                                            kk .               .  j -->   .                 . 
                                                             ↳ .................__↓_______................... 
                                                               .             k |          |                 . 
                                                               .             ↳ |  O       |      ↑          . 
                                                               .               |          |      tile       . 
                                                               .               |  b_sram  |      ↓          . 
                                                               ................|__________|.................. 
                                                               .               .          .                 . 
                                                               .               .          .                 . 
                                                               .               .          .                 . 
                                                               .               .          .                 . 
                                                               .               .          .                 . 
                                                               ..............................................
                     kk                                                                                               
       ...............↓.............................           ..............................................         
      .               .          .                 .          .               .          .                  .     
      .               .          .                 .          .               .          .                  .     
      .               . ← tile → .                 .          .               .          .                  .     
      .      A        .          .                 .          .               .          .                  .     
      .               .          .                 .          .           C   .          .                  .     
    i .               .          .                 .          .               .          .                  .     
    ↳ .................__________...................          .................__________....................     
      .               |_O________|                 .          .               |__O_-->___|                  .     
      .```````````````. ↑        .`````````````````.          .```````````````.          .``````````````````.
      .               . k        .                 .          .               .          .                  .     
      .               .          .                 .          .               .          .                  .     
      .               .          .                 .          .               .          .                  .     
      .               .          .                 .          .               .          .                  .     
      .               .          .                 .          .               .          .                  .     
      .               .          .                 .          .               .          .                  .     
      .               .          .                 .          .               .          .                  .     
      .               .          .                 .          .               .          .                  .     
      ..............................................          ...............................................     
                                                                                          
                                                                
    CONCERNS: We need to figure out how HLS is actually managing the srams, or make our management better  
              We cannot do unaligned stores yet, so tilesize of 8 won't work unless we keep ts 16 of c_sram onchip                                                                                          
 */
  type T = FixPt[TRUE,_16,_16] // Fatter type so that tileSize is burst aligned

  @virtualize
  def main() = {

    val dim_arg = args(0).to[Int]
    val dim = ArgIn[Int]
    setArg(dim, dim_arg)
    val tileSize = 16 (16 -> 16 -> 128)
    val i_tileSize = 64 (64 -> 16 -> 128)
    val par_load = 16
    val par_store = 16
    val loop_jj    = 1 // (1 -> 1 -> dim/tileSize) // THIS PAR DOES NOT WORK UNTIL BUG #205 IS FIXED
    val loop_ii    = 1 // not sure if this one works
    val loop_kk    = 1 (1 -> 1 -> 8)
    val loop_i     = 1 (1 -> 1 -> 32)
    val loop_k     = 1 (1 -> 1 -> 16)
    val loop_j     = 6 (1 -> 1 -> 16)
    val reduce_col = 4 (1 -> 1 -> 16)
    val reduce_tmp = 4 (1 -> 1 -> 16)

    // val a_data = loadCSV1D[T]("/remote/regression/data/machsuite/gemm_a.csv", "\n").reshape(dim,dim)
    // val b_data = loadCSV1D[T]("/remote/regression/data/machsuite/gemm_b.csv", "\n").reshape(dim,dim)
    val a_data = (0::dim_arg,0::dim_arg){(i,j) => random[T](5)}
    val b_data = (0::dim_arg,0::dim_arg){(i,j) => random[T](5)}
    val c_init = (0::dim_arg, 0::dim_arg){(i,j) => 0.to[T]}
    val a_dram = DRAM[T](dim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](dim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)
    setMem(c_dram, c_init)

    Accel{

      Foreach(dim by i_tileSize par loop_ii) { ii => // this loop defenitilely cant be parallelized right now
        Foreach(dim by tileSize par loop_jj) { jj => 
          val c_col = SRAM[T](i_tileSize,tileSize)
          MemReduce(c_col par reduce_col)(dim by tileSize par loop_kk) { kk => 
            val c_col_partial = SRAM[T](i_tileSize,tileSize)
            val b_sram = SRAM[T](tileSize,tileSize)
            b_sram load b_dram(kk::kk.to[Index]+tileSize, jj::jj.to[Index]+tileSize par par_load)
            Foreach(i_tileSize by 1 par loop_i) { i => 
              val a_sram = SRAM[T](tileSize)
              a_sram load a_dram(ii+i, kk::kk.to[Index]+tileSize)
              val c_tmp = SRAM[T](tileSize)
              MemReduce(c_tmp par reduce_tmp)(tileSize by 1 par loop_k) { k => 
                val c_tmp_partial = SRAM[T](tileSize)
                val temp_a = a_sram(k)
                Foreach(tileSize by 1 par loop_j) { j => 
                  c_tmp_partial(j) = b_sram(k, j) * temp_a
                }
                c_tmp_partial
              }{_+_}
            Foreach(tileSize by 1){cpy => c_col_partial(i,cpy) = c_tmp(cpy)}
            }
          c_col_partial
          }{_+_}
          c_dram(ii::ii.to[Index]+i_tileSize, jj::jj.to[Index]+tileSize par par_store) store c_col
        }
      }
    }

    // val c_gold = loadCSV1D[T]("/remote/regression/data/machsuite/gemm_gold.csv", "\n").reshape(dim,dim)
    val c_gold = (0::dim_arg,0::dim_arg){(i,j) => 
      Array.tabulate(dim_arg){k => a_data(i,k) * b_data(k,j)}.reduce{_+_}
    }
    val c_result = getMatrix(c_dram)

    printMatrix(c_gold, "C Gold: ")
    printMatrix(c_result, "C Result: ")

    val margin = 0.5.to[T]
    val cksum = c_gold.zip(c_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (GEMM_Blocked)")
  }
}


object SPMV_CRS extends SpatialApp { // Regression (Sparse) // Args: none
  override val target = Zynq


 /*                                                                                                  
   Sparse Matrix is the IEEE 494 bus interconnect matrix from UF Sparse Datasets   

    Datastructures in CRS:
              0__1__2__3__4__5_
      rowid: |__|__|__|__|__|__|
               |  |____   |_____________                       
              _↓_______`↓_______________`↓_______
      cols:  |___________________________________|
              _↓________↓________________↓_______
     values: |___________________________________|    

        Use cols to gather from vector:
              ____________________________________________
     vector: |____________________________________________|


   Concerns:
      Machsuite assumes everything fits on chip?  So there are no gathers...  Setting tilesize to problem size for now
 */

  type T = FixPt[TRUE,_16,_16]
  @virtualize
  def main() = {


    val raw_values = loadCSV1D[T]("/remote/regression/data/machsuite/crs_values.csv", "\n")
    val raw_cols = loadCSV1D[Int]("/remote/regression/data/machsuite/crs_cols.csv", "\n")
    val raw_rowid = loadCSV1D[Int]("/remote/regression/data/machsuite/crs_rowid.csv", "\n")
    val raw_vec = loadCSV1D[T]("/remote/regression/data/machsuite/crs_vec.csv", "\n")

    val NNZ_size = raw_values.length
    val N_size = raw_vec.length
    val NNZ = ArgIn[Int]
    val N = ArgIn[Int]
    val Np1 = ArgIn[Int]
    setArg(NNZ,NNZ_size)
    setArg(N,N_size)
    setArg(Np1, N_size+1)

    val values_dram = DRAM[T](NNZ) 
    val cols_dram = DRAM[Int](NNZ) 
    val rowid_dram = DRAM[Int](Np1) 
    val vec_dram = DRAM[T](N) 
    val result_dram = DRAM[T](N)

    val par_load = 16
    val par_store = 16
    val tileSize = 494
    val tile_par = 2 (1 -> 1 -> 16)
    val pt_par = 4 (1 -> 1 -> 16)
    val red_par = 8 (1 -> 1 -> 16)

    setMem(values_dram, raw_values)
    setMem(cols_dram, raw_cols)
    setMem(rowid_dram, raw_rowid)
    setMem(vec_dram, raw_vec)

    Accel {
      Foreach(N/tileSize by 1 par tile_par) { tile =>
        val rowid_sram = SRAM[Int](tileSize+1) // Should be tileSize+1 and result_sram should be tileSize
        val result_sram = SRAM[T](tileSize)

        rowid_sram load rowid_dram(tile*(tileSize+1) :: (tile+1)*(tileSize+1) par par_load)
        Foreach(tileSize by 1 par pt_par) { i => 
          val cols_sram = SRAM[Int](tileSize)
          val values_sram = SRAM[T](tileSize)
          val vec_sram = SRAM[T](tileSize)

          val start_id = rowid_sram(i)
          val stop_id = rowid_sram(i+1)
          Parallel{
            cols_sram load cols_dram(start_id :: stop_id par par_load)
            values_sram load values_dram(start_id :: stop_id par par_load)
          }
          vec_sram gather vec_dram(cols_sram, stop_id - start_id)
          println("row " + {i + tile})
          val element = Reduce(Reg[T](0))(stop_id - start_id by 1 par red_par) { j => 
            // println(" partial from " + j + " = " + {values_sram(j) * vec_sram(j)})
            values_sram(j) * vec_sram(j)
          }{_+_}
          result_sram(i) = element
        }
        result_dram(tile*tileSize :: (tile+1)*tileSize par par_store) store result_sram
      }
    }

    val data_gold = loadCSV1D[T]("/remote/regression/data/machsuite/crs_gold.csv", "\n")
    val data_result = getMem(result_dram)

    printArray(data_gold, "Gold: ")
    printArray(data_result, "Result: ")

    val margin = 0.2.to[T] // Scala does not stay in bounds as tightly as chisel
    val cksum = data_gold.zip(data_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}

    println("PASS: " + cksum + " (SPMV_CRS) * Fix gather on arbitrary width elements (64 for better prec here), issue #126")


  }
}


object PageRank extends SpatialApp { // Regression (Sparse) // Args: 50 0.125
  override val target = Zynq

  type Elem = FixPt[TRUE,_16,_16] // Float
  type X = FixPt[TRUE,_16,_16] // Float

  /*
    Currently testing with DIMACS10 Chesapeake dataset from UF Sparse Matrix collection

  */
  val margin = 0.3f

  @virtualize
  def main() {
    val sparse_data = loadCSV2D[Int]("/remote/regression/data/machsuite/pagerank_chesapeake.csv", " ", "\n").transpose
    val rows = sparse_data(0,0)
    val node1_list = Array.tabulate(sparse_data.cols - 1){i => sparse_data(0, i+1)-1} // Every page is 1-indexed...
    val node2_list = Array.tabulate(sparse_data.cols - 1){i => sparse_data(1, i+1)-1} // Every page is 1-indexed...
    // Preprocess to get frontier sizes.  We can do this on chip also if we wanted
    println("Matrix has " + rows + " rows")
    val edgeLens = Array.tabulate(rows){i => Array.tabulate(node1_list.length){j => 
      if (node1_list(j) == i) 1 else 0
    }.reduce{_+_}}
    val edgeIds = Array.tabulate(rows){i => 
      var id = -1
      Array.tabulate(node1_list.length){j => 
        if (id == -1 && node1_list(j) == i) {
          id = j
          j
        } else { 0 }
    }.reduce{_+_}}

    // printArray(node1_list, "node1_list:")
    // printArray(node2_list, "node2_list:")
    printArray(edgeLens, "edgeLens: ")
    printArray(edgeIds, "edgeIds: ")

    val tileSize = 16 (16 -> 16 -> 128)
    val par_load = 16
    val par_store = 16
    val tile_par = 2 (1 -> 1 -> 12)
    val page_par = 2 (1 -> 1 -> 16)

    // Arguments
    val itersIN = args(0).to[Int]
    val dampIN = args(1).to[X]

    val iters = ArgIn[Int]
    val NP    = ArgIn[Int]
    val damp  = ArgIn[X]
    val NE    = ArgIn[Int]
    setArg(iters, itersIN)
    setArg(NP, rows)
    setArg(damp, dampIN)
    setArg(NE, node2_list.length)

    val OCpages    = DRAM[X](NP)
    val OCedges    = DRAM[Int](NE)    // srcs of edges
    val OCedgeLens   = DRAM[Int](NP)    // counts for each edge
    val OCedgeIds   = DRAM[Int](NP) // Start index of edges

    val pagesInit = Array.tabulate(NP){i => 4.to[X]}

    setMem(OCpages, pagesInit)
    setMem(OCedges, node2_list)
    setMem(OCedgeLens, edgeLens)
    setMem(OCedgeIds, edgeIds)

    Accel {
      Sequential.Foreach(iters by 1){iter => 
        // Step through each tile
        Foreach(NP by tileSize par tile_par){page => 
          val local_pages = SRAM.buffer[X](tileSize)
          val local_edgeIds = SRAM[Int](tileSize)
          val local_edgeLens = SRAM[Int](tileSize)
          val pages_left = min(tileSize.to[Int], NP-page)
          local_pages load OCpages(page::page+pages_left par par_load)
          local_edgeLens load OCedgeLens(page::page+pages_left par par_load)
          local_edgeIds load OCedgeIds(page::page+pages_left par par_load)
          // Process each page in local tile
          Sequential.Foreach(pages_left by 1 par page_par){local_page => 
            // Fetch edge list for this page
            val edgeList = FIFO[Int](128)
            val id = local_edgeIds(local_page)
            val len = local_edgeLens(local_page)
            edgeList load OCedges(id::id+len)
            // Triage between edges that exist in local tiles and off chip
            val nearPages = FIFO[Int](128)
            val farPages1 = FIFO[Int](128)
            val farPages2 = FIFO[Int](128)
            Foreach(edgeList.numel by 1){i => 
              val tmp = edgeList.deq()
              if (tmp >= page && tmp < page+pages_left) {
                nearPages.enq(tmp - page)
              } else {
                farPages1.enq(tmp)
                farPages2.enq(tmp)
              }
            }
            // Fetch off chip info
            val local_farPages = FIFO[X](128)
            val local_farEdgeLens = FIFO[Int](128)
            local_farPages gather OCpages(farPages1) // Need fifos 1 and 2 because fifo is consumed during gather
            local_farEdgeLens gather OCedgeLens(farPages2)

            // Do math to find new rank
            val pagerank = Pipe(ii=7).Reduce(Reg[X](0))(len by 1){i => 
              if (nearPages.empty) {
                println("page: " + page + ", local_page: " + local_page + " deq from far")
                local_farPages.deq() / local_farEdgeLens.deq().to[X]
              } else {
                val addr = nearPages.deq()
                println("page: " + page + ", local_page: " + local_page + " deq from near addr " + addr)
                local_pages(addr) / local_edgeLens(addr).to[X]
              }
            }{_+_}

            // Write new rank
            local_pages(local_page) = pagerank * damp + (1.to[X] - damp)
          }
          OCpages(page::page+pages_left par par_store) store local_pages
        }
      }
    }

    val result = getMem(OCpages)

    val gold = Array.empty[X](NP)
    // Init
    for (i <- 0 until NP) {
      gold(i) = pagesInit(i)
    }

    // Really bad imperative version
    for (ep <- 0 until iters) {
      // println("Iter " + ep)
      for (i <- 0 until NP) {
        val numEdges = edgeLens(i)
        val startId = edgeIds(i)
        val iterator = Array.tabulate(numEdges){kk => startId + kk}
        val these_edges = iterator.map{j => node2_list(j)}
        val these_pages = these_edges.map{j => gold(j)}
        val these_counts = these_edges.map{j => edgeLens(j)}
        val pr = these_pages.zip(these_counts){ (p,c) =>
          // println("page " + i + " doing " + p + " / " + c)
          p/c.to[X]
        }.reduce{_+_}
        // println("new pr for " + i + " is " + pr)
        gold(i) = pr*dampIN + (1.to[X]-dampIN)
      }
    }

    println("PageRank on DIMACS10 Chesapeake dataset downloaded from UF Sparse Matrix collection")
    printArray(gold, "gold: ")
    printArray(result, "result: ")
    val cksum = result.zip(gold){ case (o, g) => (g < (o + margin.to[X])) && g > (o - margin.to[X])}.reduce{_&&_}
    println("PASS: " + cksum + " (PageRank)")

  }

}

object BlackScholes extends SpatialApp {
  override val target = Zynq

  type T = Float//FixPt[TRUE,_32,_32]
  val margin = 0.2f // Validates true if within +/- margin

  final val inv_sqrt_2xPI = 0.39894228040143270286f.to[T]

  @virtualize
  def CNDF(x: T): T = {
    val ax = abs(x)

    val xNPrimeofX = exp_taylor((ax ** 2) * -0.05f.to[T]) * inv_sqrt_2xPI
    val xK2 = 1.to[T] / ((ax * 0.2316419f.to[T]) + 1.0f.to[T])

    val xK2_2 = xK2 ** 2
    val xK2_3 = xK2_2 * xK2
    val xK2_4 = xK2_3 * xK2
    val xK2_5 = xK2_4 * xK2

    val xLocal_10 = xK2 * 0.319381530f.to[T]
    val xLocal_20 = xK2_2 * -0.356563782f.to[T]
    val xLocal_30 = xK2_3 * 1.781477937f.to[T]
    val xLocal_31 = xK2_4 * -1.821255978f.to[T]
    val xLocal_32 = xK2_5 * 1.330274429f.to[T]

    val xLocal_21 = xLocal_20 + xLocal_30
    val xLocal_22 = xLocal_21 + xLocal_31
    val xLocal_23 = xLocal_22 + xLocal_32
    val xLocal_1 = xLocal_23 + xLocal_10

    val xLocal0 = xLocal_1 * xNPrimeofX
    val xLocal  = -xLocal0 + 1.0f.to[T]

    mux(x < 0.0f.to[T], xLocal0, xLocal)
  }

  @virtualize
  def BlkSchlsEqEuroNoDiv(sptprice: T, strike: T, rate: T,
    volatility: T, time: T, otype: Int): T = {

    val xLogTerm = log_taylor( sptprice / strike )
    sptprice*strike
    val xPowerTerm = (volatility ** 2) * 0.5f.to[T]
    val xNum = (rate + xPowerTerm) * time + xLogTerm
    val xDen = volatility * sqrt_approx(time)

    val xDiv = xNum / (xDen ** 2)
    val nofXd1 = CNDF(xDiv)
    val nofXd2 = CNDF(xDiv - xDen)

    val futureValueX = strike * exp_taylor(-rate * time)

    val negNofXd1 = -nofXd1 + 1.0f.to[T]
    val negNofXd2 = -nofXd2 + 1.0f.to[T]

    val optionPrice1 = (sptprice * nofXd1) - (futureValueX * nofXd2)
    val optionPrice2 = (futureValueX * negNofXd2) - (sptprice * negNofXd1)
    mux(otype == 0, optionPrice2, optionPrice1)
  }

  @virtualize
  def blackscholes(
    stypes:      Array[Int],
    sprices:     Array[T],
    sstrike:     Array[T],
    srate:       Array[T],
    svolatility: Array[T],
    stimes:      Array[T]
  ): Array[T] = {
    val B  = 32 (32 -> 96 -> 19200)
    val OP = 2 (1 -> 2)
    val IP = 16 (1 -> 96)
    val par_load = 8
    val par_store = 8

    val size = stypes.length; bound(size) = 9995328

    val N = ArgIn[Int]
    setArg(N, size)

    val types    = DRAM[Int](N)
    val prices   = DRAM[T](N)
    val strike   = DRAM[T](N)
    val rate     = DRAM[T](N)
    val vol      = DRAM[T](N)
    val times    = DRAM[T](N)
    val optprice = DRAM[T](N)
    setMem(types, stypes)
    setMem(prices, sprices)
    setMem(strike, sstrike)
    setMem(rate, srate)
    setMem(vol, svolatility)
    setMem(times, stimes)

    Accel {
      Foreach(N by B par OP) { i =>
        val typeBlk   = SRAM[Int](B)
        val priceBlk  = SRAM[T](B)
        val strikeBlk = SRAM[T](B)
        val rateBlk   = SRAM[T](B)
        val volBlk    = SRAM[T](B)
        val timeBlk   = SRAM[T](B)
        val optpriceBlk = SRAM[T](B)

        Parallel {
          typeBlk   load types(i::i+B par par_load)
          priceBlk  load prices(i::i+B par par_load)
          strikeBlk load strike(i::i+B par par_load)
          rateBlk   load rate(i::i+B par par_load)
          volBlk    load vol(i::i+B par par_load)
          timeBlk   load times(i::i+B par par_load)
        }

        Foreach(B par IP){ j =>
          val price = BlkSchlsEqEuroNoDiv(priceBlk(j), strikeBlk(j), rateBlk(j), volBlk(j), timeBlk(j), typeBlk(j))
          optpriceBlk(j) = price
        }
        optprice(i::i+B par par_store) store optpriceBlk
      }
    }
    getMem(optprice)
  }

  @virtualize
  def main(): Unit = {
    val N = args(0).to[Int]

    val types  = Array.fill(N)(1 + random[Int](2))
    val prices = Array.fill(N)(1 + random[T])
    val strike = Array.fill(N)(1 + random[T])
    val rate   = Array.fill(N)(1 + random[T])
    val vol    = Array.fill(N)(1 + random[T])
    val time   = Array.fill(N)(1 + random[T])

    val out = blackscholes(types, prices, strike, rate, vol, time)

    val gold = Array.tabulate(N){i => 
      BlkSchlsEqEuroNoDiv(prices(i), strike(i), rate(i), vol(i), time(i), types(i))
    }
    printArray(out, "result: ")
    printArray(gold, "gold: ")

    val cksum = out.zip(gold){ case (o, g) => (g < (o + margin.to[T])) && g > (o - margin.to[T])}.reduce{_&&_}
    println("PASS: " + cksum + " (BlackSholes)")


  }
}

// good
object TPCHQ61 extends SpatialApp { // Regression (Dense) // Args: 3840
  override val target = Zynq
/*


*/

  type FT = Int

  val MIN_DATE = 0
  val MAX_DATE = 9999
  val MIN_DISC = 0
  val MAX_DISC = 9999
  val margin = 1


  @virtualize
  def tpchq6[T:Type:Num](datesIn: Array[Int], quantsIn: Array[Int], disctsIn: Array[T], pricesIn: Array[T]): T = {
    val dataSize = ArgIn[Int]
    setArg(dataSize, datesIn.length)

    val dates  = DRAM[Int](dataSize)
    val quants = DRAM[Int](dataSize)
    val discts = DRAM[T](dataSize)
    val prices = DRAM[T](dataSize)
    val minDateIn = MIN_DATE
    val maxDateIn = MAX_DATE
    val out = ArgOut[T]

    val ts = 768 (96 -> 96 -> 192000)
    val op = 2 (1 -> 2)
    val par_load = 8
    val par_store = 8
    val ip = 8 (1 -> 384)

    setMem(dates, datesIn)
    setMem(quants, quantsIn)
    setMem(discts, disctsIn)
    setMem(prices, pricesIn)

    Accel {
      val minDate = minDateIn
      val maxDate = maxDateIn

      val acc = Reg[T]
      Reduce(acc)(dataSize by ts par op){ i =>
        val datesTile  = SRAM[Int](ts)
        val quantsTile = SRAM[Int](ts)
        val disctsTile = SRAM[T](ts)
        val pricesTile = SRAM[T](ts)
        Parallel {
          datesTile  load dates(i::i+ts par par_load)
          quantsTile load quants(i::i+ts par par_load)
          disctsTile load discts(i::i+ts par par_load)
          pricesTile load prices(i::i+ts par par_load)
        }
        Reduce(Reg[T])(ts par ip){ j =>
          val date  = datesTile(j)
          val disct = disctsTile(j)
          val quant = quantsTile(j)
          val price = pricesTile(j)
          val valid = date > minDate && date < maxDate && disct >= MIN_DISC.to[T] && disct <= MAX_DISC.to[T] && quant < 24
          mux(valid, price * disct, 0.to[T])
        }{_+_}
      }{_+_}

      out := acc
    }
    getArg(out)
  }

  @virtualize
  def main() {
    val N = args(0).to[Int]

    // val dates  = Array.fill(N){random[Int](20) + 65}
    // val quants = Array.fill(N){random[Int](25) }
    // // val discts = Array.fill(N){random[FT] * 0.05f + 0.02f}
    // // val prices = Array.fill(N){random[FT] * 1000f}
    // val discts = Array.fill(N){random[FT] /*/ 100000*/}
    // val prices = Array.fill(N){random[FT] /*/ 100000*/}

    val dates  = Array.tabulate[Int](N){i => i % 256 } // Standard array
    val quants = Array.tabulate[Int](N){i => i % 256 } // Standard array
    val discts = Array.tabulate[FT](N){i => i % 256 } // Standard array
    val prices = Array.tabulate[FT](N){i => i % 256 } // Standard array

    val result = tpchq6(dates, quants, discts, prices)

    // --- software version
    val conds = Array.tabulate(N){i => dates(i) > MIN_DATE && dates(i) < MAX_DATE  &&
                                       quants(i) < 24 && discts(i) >= MIN_DISC  && discts(i) <= MAX_DISC}
    // printArr(conds, "conds: ")

    val gold = Array.tabulate(N){i => if (conds(i)) prices(i) * discts(i) else 0.to[FT] }.reduce{_+_}

    println("expected " + gold)
    println("result " + result)

    val cksum = (gold < result + margin && gold > result - margin)
    println("PASS: " + cksum + " (TPCHQ6)")
  }
}

object TPCHQ62 extends SpatialApp { // Regression (Dense) // Args: 3840
  override val target = Zynq
/*


*/

  type FT = Int

  val MIN_DATE = 0
  val MAX_DATE = 9999
  val MIN_DISC = 0
  val MAX_DISC = 9999
  val margin = 1


  @virtualize
  def tpchq6[T:Type:Num](datesIn: Array[Int], quantsIn: Array[Int], disctsIn: Array[T], pricesIn: Array[T]): T = {
    val dataSize = ArgIn[Int]
    setArg(dataSize, datesIn.length)

    val dates  = DRAM[Int](dataSize)
    val quants = DRAM[Int](dataSize)
    val discts = DRAM[T](dataSize)
    val prices = DRAM[T](dataSize)
    val minDateIn = MIN_DATE
    val maxDateIn = MAX_DATE
    val out = ArgOut[T]

    val ts = 384 (96 -> 96 -> 192000)
    val op = 2 (1 -> 2)
    val par_load = 16
    val par_store = 16
    val ip = 32 (1 -> 384)

    setMem(dates, datesIn)
    setMem(quants, quantsIn)
    setMem(discts, disctsIn)
    setMem(prices, pricesIn)

    Accel {
      val minDate = minDateIn
      val maxDate = maxDateIn

      val acc = Reg[T]
      Reduce(acc)(dataSize by ts par op){ i =>
        val datesTile  = SRAM[Int](ts)
        val quantsTile = SRAM[Int](ts)
        val disctsTile = SRAM[T](ts)
        val pricesTile = SRAM[T](ts)
        Parallel {
          datesTile  load dates(i::i+ts par par_load)
          quantsTile load quants(i::i+ts par par_load)
          disctsTile load discts(i::i+ts par par_load)
          pricesTile load prices(i::i+ts par par_load)
        }
        Reduce(Reg[T])(ts par ip){ j =>
          val date  = datesTile(j)
          val disct = disctsTile(j)
          val quant = quantsTile(j)
          val price = pricesTile(j)
          val valid = date > minDate && date < maxDate && disct >= MIN_DISC.to[T] && disct <= MAX_DISC.to[T] && quant < 24
          mux(valid, price * disct, 0.to[T])
        }{_+_}
      }{_+_}

      out := acc
    }
    getArg(out)
  }

  @virtualize
  def main() {
    val N = args(0).to[Int]

    // val dates  = Array.fill(N){random[Int](20) + 65}
    // val quants = Array.fill(N){random[Int](25) }
    // // val discts = Array.fill(N){random[FT] * 0.05f + 0.02f}
    // // val prices = Array.fill(N){random[FT] * 1000f}
    // val discts = Array.fill(N){random[FT] /*/ 100000*/}
    // val prices = Array.fill(N){random[FT] /*/ 100000*/}

    val dates  = Array.tabulate[Int](N){i => i % 256 } // Standard array
    val quants = Array.tabulate[Int](N){i => i % 256 } // Standard array
    val discts = Array.tabulate[FT](N){i => i % 256 } // Standard array
    val prices = Array.tabulate[FT](N){i => i % 256 } // Standard array

    val result = tpchq6(dates, quants, discts, prices)

    // --- software version
    val conds = Array.tabulate(N){i => dates(i) > MIN_DATE && dates(i) < MAX_DATE  &&
                                       quants(i) < 24 && discts(i) >= MIN_DISC  && discts(i) <= MAX_DISC}
    // printArr(conds, "conds: ")

    val gold = Array.tabulate(N){i => if (conds(i)) prices(i) * discts(i) else 0.to[FT] }.reduce{_+_}

    println("expected " + gold)
    println("result " + result)

    val cksum = (gold < result + margin && gold > result - margin)
    println("PASS: " + cksum + " (TPCHQ6)")
  }
}

object TPCHQ63 extends SpatialApp { // Regression (Dense) // Args: 3840
  override val target = Zynq
/*


*/

  type FT = Int

  val MIN_DATE = 0
  val MAX_DATE = 9999
  val MIN_DISC = 0
  val MAX_DISC = 9999
  val margin = 1


  @virtualize
  def tpchq6[T:Type:Num](datesIn: Array[Int], quantsIn: Array[Int], disctsIn: Array[T], pricesIn: Array[T]): T = {
    val dataSize = ArgIn[Int]
    setArg(dataSize, datesIn.length)

    val dates  = DRAM[Int](dataSize)
    val quants = DRAM[Int](dataSize)
    val discts = DRAM[T](dataSize)
    val prices = DRAM[T](dataSize)
    val minDateIn = MIN_DATE
    val maxDateIn = MAX_DATE
    val out = ArgOut[T]

    val ts = 384 (96 -> 96 -> 192000)
    val op = 1 (1 -> 2)
    val par_load = 16
    val par_store = 16
    val ip = 64 (1 -> 384)

    setMem(dates, datesIn)
    setMem(quants, quantsIn)
    setMem(discts, disctsIn)
    setMem(prices, pricesIn)

    Accel {
      val minDate = minDateIn
      val maxDate = maxDateIn

      val acc = Reg[T]
      Reduce(acc)(dataSize by ts par op){ i =>
        val datesTile  = SRAM[Int](ts)
        val quantsTile = SRAM[Int](ts)
        val disctsTile = SRAM[T](ts)
        val pricesTile = SRAM[T](ts)
        Parallel {
          datesTile  load dates(i::i+ts par par_load)
          quantsTile load quants(i::i+ts par par_load)
          disctsTile load discts(i::i+ts par par_load)
          pricesTile load prices(i::i+ts par par_load)
        }
        Reduce(Reg[T])(ts par ip){ j =>
          val date  = datesTile(j)
          val disct = disctsTile(j)
          val quant = quantsTile(j)
          val price = pricesTile(j)
          val valid = date > minDate && date < maxDate && disct >= MIN_DISC.to[T] && disct <= MAX_DISC.to[T] && quant < 24
          mux(valid, price * disct, 0.to[T])
        }{_+_}
      }{_+_}

      out := acc
    }
    getArg(out)
  }

  @virtualize
  def main() {
    val N = args(0).to[Int]

    // val dates  = Array.fill(N){random[Int](20) + 65}
    // val quants = Array.fill(N){random[Int](25) }
    // // val discts = Array.fill(N){random[FT] * 0.05f + 0.02f}
    // // val prices = Array.fill(N){random[FT] * 1000f}
    // val discts = Array.fill(N){random[FT] /*/ 100000*/}
    // val prices = Array.fill(N){random[FT] /*/ 100000*/}

    val dates  = Array.tabulate[Int](N){i => i % 256 } // Standard array
    val quants = Array.tabulate[Int](N){i => i % 256 } // Standard array
    val discts = Array.tabulate[FT](N){i => i % 256 } // Standard array
    val prices = Array.tabulate[FT](N){i => i % 256 } // Standard array

    val result = tpchq6(dates, quants, discts, prices)

    // --- software version
    val conds = Array.tabulate(N){i => dates(i) > MIN_DATE && dates(i) < MAX_DATE  &&
                                       quants(i) < 24 && discts(i) >= MIN_DISC  && discts(i) <= MAX_DISC}
    // printArr(conds, "conds: ")

    val gold = Array.tabulate(N){i => if (conds(i)) prices(i) * discts(i) else 0.to[FT] }.reduce{_+_}

    println("expected " + gold)
    println("result " + result)

    val cksum = (gold < result + margin && gold > result - margin)
    println("PASS: " + cksum + " (TPCHQ6)")
  }
}


// good, but pipelining vs area
object AES1 extends SpatialApp { // Regression (Dense) // Args: 50
  override val target = Zynq

  /*
  TODO: Optimize/parallelize many of the memory accesses here and pipeline as much as possible
  
  MachSuite Concerns: 
    - Mix rows math seemed wrong in their implementation
    - Not exactly sure what was going on with their expand_key step
  */
  type UInt8 = FixPt[FALSE,_8,_0]
  @virtualize
  def main() = {
    // Setup off-chip data
    // val text_in = "the sharkmoster"
    // val plaintext = argon.lang.String.string2num(text_in)
    val plaintext = Array[UInt8](0,17,34,51,68,85,102,119,136,153,170,187,204,221,238,255)
    val key = Array.tabulate(32){i => i.to[UInt8]}
    val sbox = Array[UInt8]( // 256 elements
      0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5,
      0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76,
      0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0,
      0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0,
      0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc,
      0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15,
      0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a,
      0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75,
      0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0,
      0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84,
      0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b,
      0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf,
      0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85,
      0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8,
      0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5,
      0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2,
      0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17,
      0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73,
      0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88,
      0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb,
      0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c,
      0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79,
      0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9,
      0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08,
      0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6,
      0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a,
      0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e,
      0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e,
      0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94,
      0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
      0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68,
      0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16
    )
    
    val par_load = 16
    val par_store = 16
    val outer_par = 2 (1 -> 1 -> 4) // This may crash GC

    // Setup
    val num_bytes = ArgIn[Int]
    setArg(num_bytes, 16*args(0).to[Int])

    // Create DRAMs
    val plaintext_dram = DRAM[UInt8](16)
    val key_dram = DRAM[UInt8](32)
    val sbox_dram = DRAM[UInt8](256)
    val ciphertext_dram = DRAM[Int](num_bytes)

    // Transfer data to DRAMs
    setMem(plaintext_dram, plaintext)
    setMem(key_dram, key)
    setMem(sbox_dram, sbox)

    // Debugging support
    val niter = 15
    // val niter = ArgIn[Int]
    // setArg(niter, args(0).to[Int])
    // val key_debug = DRAM[UInt8](32)

    Accel{


      val sbox_sram = SRAM[UInt8](256)
      // Load structures
      sbox_sram load sbox_dram(0::256 par par_load)

      Foreach(num_bytes by 16 par outer_par){block_id => 
        // Setup data structures
        val plaintext_flat = SRAM.buffer[UInt8](16)
        val plaintext_sram = RegFile.buffer[UInt8](4,4)
        val key_sram = SRAM.buffer[UInt8](32)
        // val mix_lut = LUT[Int](4,4)(
        //    2, 3, 1, 1,
        //    1, 2, 3, 1,
        //    1, 1, 2, 3,
        //    3, 1, 1, 2
        //  )
        val rcon = Reg.buffer[UInt8](1)

        // Specify methods
        def expand_key(): Unit = {
          val addr_lut = LUT[Int](4)(29, 30, 31, 28)
          Foreach(4 by 1) { i => 
            key_sram(i) = key_sram(i) ^ sbox_sram(key_sram(addr_lut(i)).to[Int]) ^ mux(i.to[Index] == 0, rcon.value, 0)
          }
          // Pipe{key_sram(0) = key_sram(0) ^ sbox_sram(key_sram(29).as[UInt16].as[Int]) ^ rcon}
          // Pipe{key_sram(1) = key_sram(1) ^ sbox_sram(key_sram(30).as[UInt16].as[Int])}
          // Pipe{key_sram(2) = key_sram(2) ^ sbox_sram(key_sram(31).as[UInt16].as[Int])}
          // Pipe{key_sram(3) = key_sram(3) ^ sbox_sram(key_sram(28).as[UInt16].as[Int])}
          rcon := (((rcon)<<1) ^ ((((rcon)>>7) & 1) * 0x1b))

          Sequential.Foreach(4 until 16 by 4) {i =>
            Sequential.Foreach(4 by 1) {j => 
              key_sram(i.to[Index]+j.to[Index]) = key_sram(i.to[Index]+j.to[Index]) ^ key_sram(i.to[Index] - 4 + j.to[Index])
            }
            // Pipe{key_sram(i) = key_sram(i) ^ key_sram(i-4)}
            // Pipe{key_sram(i+1) = key_sram(i+1) ^ key_sram(i-3)}
            // Pipe{key_sram(i+2) = key_sram(i+2) ^ key_sram(i-2)}
            // Pipe{key_sram(i+3) = key_sram(i+3) ^ key_sram(i-1)}
          }
        
          Sequential.Foreach(16 until 20 by 1){i => 
            key_sram(i) = key_sram(i) ^ sbox_sram(key_sram(i.to[Index]-4).to[Int])
          }
          // Pipe{key_sram(16) = key_sram(16) ^ sbox_sram(key_sram(12).as[UInt16].as[Int])}
          // Pipe{key_sram(17) = key_sram(17) ^ sbox_sram(key_sram(13).as[UInt16].as[Int])}
          // Pipe{key_sram(18) = key_sram(18) ^ sbox_sram(key_sram(14).as[UInt16].as[Int])}
          // Pipe{key_sram(19) = key_sram(19) ^ sbox_sram(key_sram(15).as[UInt16].as[Int])}

          Sequential.Foreach(20 until 32 by 4) {i => 
            Sequential.Foreach(4 by 1) { j => 
              key_sram(i.to[Index]+j.to[Index]) = key_sram(i.to[Index]+j.to[Index]) ^ key_sram(i.to[Index] - 4 + j.to[Index])
            }
            // Pipe{key_sram(i) = key_sram(i) ^ key_sram(i-4)}
            // Pipe{key_sram(i+1) = key_sram(i+1) ^ key_sram(i-3)}
            // Pipe{key_sram(i+2) = key_sram(i+2) ^ key_sram(i-2)}
            // Pipe{key_sram(i+3) = key_sram(i+3) ^ key_sram(i-1)}
          }
        }

        def shift_rows(): Unit = {
          Sequential.Foreach(4 by 1){ i => 
            val row = RegFile[UInt8](4) 
            Foreach(4 by 1){ j => 
              val col_addr = (j.to[Index] - i.to[Index]) % 4
              row(col_addr) = plaintext_sram(i,j)
            }
            Foreach(4 by 1){ j => 
              plaintext_sram(i,j) = row(j)
            }
          }
        }

        def substitute_bytes(): Unit = {
          Sequential.Foreach(4 by 1, 4 by 1){(i,j) => 
            val addr = plaintext_sram(i,j).as[UInt16].as[Int] // Upcast without sign-extend
            val subst = sbox_sram(addr)
            plaintext_sram(i,j) = subst
          }
        }

        def rj_xtime(x: UInt8): UInt8 = {
          mux(((x & 0x80.to[UInt8]) > 0.to[UInt8]), ((x << 1) ^ 0x1b.to[UInt8]), x << 1)
        }

        def mix_columns(): Unit = {
          Sequential.Foreach(4 by 1){j => 
            val col = RegFile[UInt8](4)
            Foreach(4 by 1 par 4) { i => col(i) = plaintext_sram(i,j) }
            val e = Reduce(Reg[UInt8](0))(4 by 1 par 4) { i => col(i) }{_^_}
            // val e = col(0) ^ col(1) ^ col(2) ^ col(3)
            Foreach(4 by 1) { i => 
              val id1 = (i.to[Index]+1)%4
              plaintext_sram(i,j) = col(i) ^ e ^ rj_xtime(col(i) ^ col(id1))
            }
          }
        }

        def add_round_key(round: Index): Unit = {
          Foreach(4 by 1, 4 by 1) { (i,j) => 
            val key = mux(round % 2 == 1, key_sram(i.to[Index]+j.to[Index]*4+16), key_sram(i.to[Index]+j.to[Index]*4))
            plaintext_sram(i,j) = plaintext_sram(i,j) ^ key
          }
        }
        plaintext_flat load plaintext_dram(0::16 par par_load) // TODO: Allow dram loads to reshape (gh issue #83)
        key_sram load key_dram(0::32 par par_load)
        rcon := 1

        // gh issue #83
        Sequential.Foreach(4 by 1 par 1){i => 
          Sequential.Foreach(4 by 1 par 1){j => 
            plaintext_sram(i,j) = plaintext_flat(j.to[Index]*4+i.to[Index]) // MachSuite flattens columnwise... Why????
          }
        }

        /* Loopy version */
        Sequential.Foreach(niter by 1) { round => 
          // SubBytes
          if (round > 0) {
            Pipe{substitute_bytes()}
          }

          // ShiftRows
          if (round > 0) {
            Pipe{shift_rows()}
          }

          // MixColumns
          if (round > 0 && round < 14 ) {
            Pipe{mix_columns()}
          }

          // Expand key
          if (round > 0 && ((round % 2) == 0)) {
            Pipe{expand_key()}
          }

          // AddRoundKey
          add_round_key(round)

        }

        // /* Partially pipelined version */
        // // Round 0
        // add_round_key(0)

        // // Rounds 1 - 7
        // Sequential.Foreach(1 until 8 by 1) { round => 
        //   substitute_bytes()
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   if ((round % 2) == 0) {
        //     Pipe{expand_key()}
        //   }
        //   add_round_key(round)
        // }
        // // Rounds 8 - 14
        // Sequential.Foreach(8 until 14 by 1) { round => 
        //   substitute_bytes()
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   if ((round % 2) == 0) {
        //     Pipe{expand_key()}
        //   }
        //   add_round_key(round)
        // }
        // // Round 14
        // Pipe {
        //   substitute_bytes()
        //   Pipe{shift_rows()}
        //   Pipe{expand_key()}
        //   add_round_key(14)
        // }


        // /* Totally pipelined version */
        // // Round 0
        // add_round_key(0)
        
        // // Round 1
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   add_round_key(1)
        // }

        // // Round 2
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   Pipe{expand_key()}
        //   add_round_key(2)
        // }

        // // Round 3
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   add_round_key(3)
        // }

        // // Round 4
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   Pipe{expand_key()}
        //   add_round_key(4)
        // }

        // // Round 5
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   add_round_key(5)
        // }

        // // Round 6
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   Pipe{expand_key()}
        //   add_round_key(6)
        // }

        // // Round 7
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   add_round_key(7)
        // }

        // // Round 8
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   Pipe{expand_key()}
        //   add_round_key(8)
        // }

        // // Round 9
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   add_round_key(9)
        // }

        // // Round 10
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   Pipe{expand_key()}
        //   add_round_key(10)
        // }

        // // Round 11
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   add_round_key(11)
        // }

        // // Round 12
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   Pipe{expand_key()}
        //   add_round_key(12)
        // }

        // // Round 13
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   add_round_key(13)
        // }

        // // Round 14
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{expand_key()}
        //   add_round_key(14)
        // }

        // Reshape plaintext_sram (gh issue # 83)
        val ciphertext_flat = SRAM[Int](16)
        Sequential.Foreach(4 by 1, 4 by 1) {(i,j) => 
          ciphertext_flat(j.to[Index]*4+i.to[Index]) = plaintext_sram(i,j).as[Int]
        }

        ciphertext_dram(block_id::block_id+16 par par_store) store ciphertext_flat
      }

      // // Debugging
      // key_debug store key_sram

    }

    val ciphertext = getMem(ciphertext_dram)
    val ciphertext_gold = Array.fill(args(0).to[Int])(Array[Int](142,162,183,202,81,103,69,191,234,252,73,144,75,73,96,137)).flatten

    printArray(ciphertext_gold, "Expected: ")
    printArray(ciphertext, "Got: ")

    // // Debugging
    // val key_dbg = getMem(key_debug)
    // printArray(key_dbg, "Key: ")

    val cksum = ciphertext_gold.zip(ciphertext){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (AES) * For retiming, need to fix ^ reduction if not parallelized")

  }
}

object AES2 extends SpatialApp { // Regression (Dense) // Args: 50
  override val target = Zynq

  /*
  TODO: Optimize/parallelize many of the memory accesses here and pipeline as much as possible
  
  MachSuite Concerns: 
    - Mix rows math seemed wrong in their implementation
    - Not exactly sure what was going on with their expand_key step
  */
  type UInt8 = FixPt[FALSE,_8,_0]
  @virtualize
  def main() = {
    // Setup off-chip data
    // val text_in = "the sharkmoster"
    // val plaintext = argon.lang.String.string2num(text_in)
    val plaintext = Array[UInt8](0,17,34,51,68,85,102,119,136,153,170,187,204,221,238,255)
    val key = Array.tabulate(32){i => i.to[UInt8]}
    val sbox = Array[UInt8]( // 256 elements
      0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5,
      0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76,
      0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0,
      0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0,
      0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc,
      0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15,
      0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a,
      0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75,
      0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0,
      0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84,
      0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b,
      0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf,
      0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85,
      0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8,
      0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5,
      0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2,
      0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17,
      0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73,
      0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88,
      0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb,
      0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c,
      0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79,
      0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9,
      0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08,
      0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6,
      0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a,
      0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e,
      0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e,
      0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94,
      0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
      0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68,
      0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16
    )
    
    val par_load = 16
    val par_store = 16
    val outer_par = 1 (1 -> 1 -> 4) // This may crash GC

    // Setup
    val num_bytes = ArgIn[Int]
    setArg(num_bytes, 16*args(0).to[Int])

    // Create DRAMs
    val plaintext_dram = DRAM[UInt8](16)
    val key_dram = DRAM[UInt8](32)
    val sbox_dram = DRAM[UInt8](256)
    val ciphertext_dram = DRAM[Int](num_bytes)

    // Transfer data to DRAMs
    setMem(plaintext_dram, plaintext)
    setMem(key_dram, key)
    setMem(sbox_dram, sbox)

    // Debugging support
    val niter = 15
    // val niter = ArgIn[Int]
    // setArg(niter, args(0).to[Int])
    // val key_debug = DRAM[UInt8](32)

    Accel{


      val sbox_sram = SRAM[UInt8](256)
      // Load structures
      sbox_sram load sbox_dram(0::256 par par_load)

      Foreach(num_bytes by 16 par outer_par){block_id => 
        // Setup data structures
        val plaintext_flat = SRAM.buffer[UInt8](16)
        val plaintext_sram = RegFile.buffer[UInt8](4,4)
        val key_sram = SRAM.buffer[UInt8](32)
        // val mix_lut = LUT[Int](4,4)(
        //    2, 3, 1, 1,
        //    1, 2, 3, 1,
        //    1, 1, 2, 3,
        //    3, 1, 1, 2
        //  )
        val rcon = Reg.buffer[UInt8](1)

        // Specify methods
        def expand_key(): Unit = {
          val addr_lut = LUT[Int](4)(29, 30, 31, 28)
          Foreach(4 by 1) { i => 
            key_sram(i) = key_sram(i) ^ sbox_sram(key_sram(addr_lut(i)).to[Int]) ^ mux(i.to[Index] == 0, rcon.value, 0)
          }
          // Pipe{key_sram(0) = key_sram(0) ^ sbox_sram(key_sram(29).as[UInt16].as[Int]) ^ rcon}
          // Pipe{key_sram(1) = key_sram(1) ^ sbox_sram(key_sram(30).as[UInt16].as[Int])}
          // Pipe{key_sram(2) = key_sram(2) ^ sbox_sram(key_sram(31).as[UInt16].as[Int])}
          // Pipe{key_sram(3) = key_sram(3) ^ sbox_sram(key_sram(28).as[UInt16].as[Int])}
          rcon := (((rcon)<<1) ^ ((((rcon)>>7) & 1) * 0x1b))

          Sequential.Foreach(4 until 16 by 4) {i =>
            Sequential.Foreach(4 by 1) {j => 
              key_sram(i.to[Index]+j.to[Index]) = key_sram(i.to[Index]+j.to[Index]) ^ key_sram(i.to[Index] - 4 + j.to[Index])
            }
            // Pipe{key_sram(i) = key_sram(i) ^ key_sram(i-4)}
            // Pipe{key_sram(i+1) = key_sram(i+1) ^ key_sram(i-3)}
            // Pipe{key_sram(i+2) = key_sram(i+2) ^ key_sram(i-2)}
            // Pipe{key_sram(i+3) = key_sram(i+3) ^ key_sram(i-1)}
          }
        
          Sequential.Foreach(16 until 20 by 1){i => 
            key_sram(i) = key_sram(i) ^ sbox_sram(key_sram(i.to[Index]-4).to[Int])
          }
          // Pipe{key_sram(16) = key_sram(16) ^ sbox_sram(key_sram(12).as[UInt16].as[Int])}
          // Pipe{key_sram(17) = key_sram(17) ^ sbox_sram(key_sram(13).as[UInt16].as[Int])}
          // Pipe{key_sram(18) = key_sram(18) ^ sbox_sram(key_sram(14).as[UInt16].as[Int])}
          // Pipe{key_sram(19) = key_sram(19) ^ sbox_sram(key_sram(15).as[UInt16].as[Int])}

          Sequential.Foreach(20 until 32 by 4) {i => 
            Sequential.Foreach(4 by 1) { j => 
              key_sram(i.to[Index]+j.to[Index]) = key_sram(i.to[Index]+j.to[Index]) ^ key_sram(i.to[Index] - 4 + j.to[Index])
            }
            // Pipe{key_sram(i) = key_sram(i) ^ key_sram(i-4)}
            // Pipe{key_sram(i+1) = key_sram(i+1) ^ key_sram(i-3)}
            // Pipe{key_sram(i+2) = key_sram(i+2) ^ key_sram(i-2)}
            // Pipe{key_sram(i+3) = key_sram(i+3) ^ key_sram(i-1)}
          }
        }

        def shift_rows(): Unit = {
          Sequential.Foreach(4 by 1){ i => 
            val row = RegFile[UInt8](4) 
            Foreach(4 by 1){ j => 
              val col_addr = (j.to[Index] - i.to[Index]) % 4
              row(col_addr) = plaintext_sram(i,j)
            }
            Foreach(4 by 1){ j => 
              plaintext_sram(i,j) = row(j)
            }
          }
        }

        def substitute_bytes(): Unit = {
          Sequential.Foreach(4 by 1, 4 by 1){(i,j) => 
            val addr = plaintext_sram(i,j).as[UInt16].as[Int] // Upcast without sign-extend
            val subst = sbox_sram(addr)
            plaintext_sram(i,j) = subst
          }
        }

        def rj_xtime(x: UInt8): UInt8 = {
          mux(((x & 0x80.to[UInt8]) > 0.to[UInt8]), ((x << 1) ^ 0x1b.to[UInt8]), x << 1)
        }

        def mix_columns(): Unit = {
          Sequential.Foreach(4 by 1){j => 
            val col = RegFile[UInt8](4)
            Foreach(4 by 1 par 4) { i => col(i) = plaintext_sram(i,j) }
            val e = Reduce(Reg[UInt8](0))(4 by 1 par 4) { i => col(i) }{_^_}
            // val e = col(0) ^ col(1) ^ col(2) ^ col(3)
            Foreach(4 by 1) { i => 
              val id1 = (i.to[Index]+1)%4
              plaintext_sram(i,j) = col(i) ^ e ^ rj_xtime(col(i) ^ col(id1))
            }
          }
        }

        def add_round_key(round: Index): Unit = {
          Foreach(4 by 1, 4 by 1) { (i,j) => 
            val key = mux(round % 2 == 1, key_sram(i.to[Index]+j.to[Index]*4+16), key_sram(i.to[Index]+j.to[Index]*4))
            plaintext_sram(i,j) = plaintext_sram(i,j) ^ key
          }
        }
        plaintext_flat load plaintext_dram(0::16 par par_load) // TODO: Allow dram loads to reshape (gh issue #83)
        key_sram load key_dram(0::32 par par_load)
        rcon := 1

        // gh issue #83
        Sequential.Foreach(4 by 1 par 1){i => 
          Sequential.Foreach(4 by 1 par 1){j => 
            plaintext_sram(i,j) = plaintext_flat(j.to[Index]*4+i.to[Index]) // MachSuite flattens columnwise... Why????
          }
        }

        /* Loopy version */
        Sequential.Foreach(niter by 1) { round => 
          // SubBytes
          if (round > 0) {
            Pipe{substitute_bytes()}
          }

          // ShiftRows
          if (round > 0) {
            Pipe{shift_rows()}
          }

          // MixColumns
          if (round > 0 && round < 14 ) {
            Pipe{mix_columns()}
          }

          // Expand key
          if (round > 0 && ((round % 2) == 0)) {
            Pipe{expand_key()}
          }

          // AddRoundKey
          add_round_key(round)

        }

        // /* Partially pipelined version */
        // // Round 0
        // add_round_key(0)

        // // Rounds 1 - 7
        // Sequential.Foreach(1 until 8 by 1) { round => 
        //   substitute_bytes()
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   if ((round % 2) == 0) {
        //     Pipe{expand_key()}
        //   }
        //   add_round_key(round)
        // }
        // // Rounds 8 - 14
        // Sequential.Foreach(8 until 14 by 1) { round => 
        //   substitute_bytes()
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   if ((round % 2) == 0) {
        //     Pipe{expand_key()}
        //   }
        //   add_round_key(round)
        // }
        // // Round 14
        // Pipe {
        //   substitute_bytes()
        //   Pipe{shift_rows()}
        //   Pipe{expand_key()}
        //   add_round_key(14)
        // }


        // /* Totally pipelined version */
        // // Round 0
        // add_round_key(0)
        
        // // Round 1
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   add_round_key(1)
        // }

        // // Round 2
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   Pipe{expand_key()}
        //   add_round_key(2)
        // }

        // // Round 3
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   add_round_key(3)
        // }

        // // Round 4
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   Pipe{expand_key()}
        //   add_round_key(4)
        // }

        // // Round 5
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   add_round_key(5)
        // }

        // // Round 6
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   Pipe{expand_key()}
        //   add_round_key(6)
        // }

        // // Round 7
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   add_round_key(7)
        // }

        // // Round 8
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   Pipe{expand_key()}
        //   add_round_key(8)
        // }

        // // Round 9
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   add_round_key(9)
        // }

        // // Round 10
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   Pipe{expand_key()}
        //   add_round_key(10)
        // }

        // // Round 11
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   add_round_key(11)
        // }

        // // Round 12
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   Pipe{expand_key()}
        //   add_round_key(12)
        // }

        // // Round 13
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{mix_columns()}
        //   add_round_key(13)
        // }

        // // Round 14
        // Pipe{
        //   Pipe{substitute_bytes()}
        //   Pipe{shift_rows()}
        //   Pipe{expand_key()}
        //   add_round_key(14)
        // }

        // Reshape plaintext_sram (gh issue # 83)
        val ciphertext_flat = SRAM[Int](16)
        Sequential.Foreach(4 by 1, 4 by 1) {(i,j) => 
          ciphertext_flat(j.to[Index]*4+i.to[Index]) = plaintext_sram(i,j).as[Int]
        }

        ciphertext_dram(block_id::block_id+16 par par_store) store ciphertext_flat
      }

      // // Debugging
      // key_debug store key_sram

    }

    val ciphertext = getMem(ciphertext_dram)
    val ciphertext_gold = Array.fill(args(0).to[Int])(Array[Int](142,162,183,202,81,103,69,191,234,252,73,144,75,73,96,137)).flatten

    printArray(ciphertext_gold, "Expected: ")
    printArray(ciphertext, "Got: ")

    // // Debugging
    // val key_dbg = getMem(key_debug)
    // printArray(key_dbg, "Key: ")

    val cksum = ciphertext_gold.zip(ciphertext){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (AES) * For retiming, need to fix ^ reduction if not parallelized")

  }
}

// good
object Kmeans1 extends SpatialApp { // Regression (Dense) // Args: 3 64
  override val target = Zynq

  type X = Int

  val numcents = 16
  val dim = 32
  val pts_per_ld = 1 // ???

  val element_max = 10
  val margin = (element_max * 0.2).to[X]

  val MAXK = numcents
  val MAXD = dim

  @virtualize
  def kmeans[T:Type:Num](points_in: Array[T], cent_inits: Array[T], numPoints: Int, numCents: Int, numDims: Int, it: Int) = {
    bound(numPoints) = 960000
    bound(numCents) = MAXK
    bound(numDims) = MAXD

    val BN = pts_per_ld (96 -> 96 -> 9600)
    val BD = MAXD
    val par_load = 16
    val par_store = 16
    val PX = 1 (1 -> 1)
    val P0 = 2 (1 -> 2 -> dim)
    val P1 = 4 (1 -> 2 -> dim)
    val P2 = 4 (1 -> 2 -> dim)
    val P3 = 16 (1 -> 2 -> numcents)

    val iters = ArgIn[Int]
    val N     = ArgIn[Int]
    val K     = numCents //ArgIn[Int]
    val D     = numDims //ArgIn[Int]

    setArg(iters, it)
    setArg(N, numPoints)
    // setArg(K, numCents)
    // setArg(D, numDims)

    val points = DRAM[T](N, D)    // Input points
    val centroids = DRAM[T](numcents*dim) // Output centroids
    // val init_cents = DRAM[T](K,D) // Output centroids
    setMem(points, points_in)
    // setMem(init_cents, cent_inits)


    Accel {
      val cts = SRAM[T](MAXK, MAXD)
      val newCents = SRAM[T](MAXK,MAXD)

      // Load initial centroids (from points)
      cts load points(0::K, 0::D par par_load)

      // Initialize newCents
      // FPGA:
      Foreach(K by 1, D by 1 par P0) {(i,j) => newCents(i,j) = cts(i,j)} 

      val DM1 = D - 1

      Sequential.Foreach(iters by 1){epoch =>
        // Flush centroid accumulator
        // FPGA:
        Foreach(K by 1, D par P0){(ct,d) =>
          newCents(ct,d) = 0.to[T]
        }

        // For each set of points
        Foreach(N by BN par PX){i =>
          val pts = SRAM[T](BN, BD)
          pts load points(i::i+BN, 0::BD par par_load)

          // For each point in this set
          MemFold(newCents par P0)(BN par PX){pt =>
            // Find the index of the closest centroid
            val accum = Reg[Tup2[Int,T]]( pack(0.to[Int], 100000.to[T]) )
            val minCent = Reduce(accum)(K par PX){ct =>
              val dist = Reg[T](0.to[T])
              Reduce(dist)(D par P2){d => (pts(pt,d) - cts(ct,d)) ** 2 }{_+_}
              pack(ct, dist.value)
            }{(a,b) =>
              mux(a._2 < b._2, a, b)
            }

            // Store this point to the set of accumulators
            val localCent = SRAM[T](MAXK,MAXD)
            Foreach(K by 1, D par P2){(ct,d) =>
              //val elem = mux(d == DM1, 1.to[T], pts(pt, d)) // fix for vanishing mux
              val elem = pts(pt,d)
              localCent(ct, d) = mux(ct == minCent.value._1, elem, 0.to[T])
            }
            localCent
          }{_+_} // Add the current point to the accumulators for this centroid
        }

        val centCount = SRAM[T](MAXK)
        Foreach(K by 1 par P3){ct => centCount(ct) = max(newCents(ct,DM1), 1.to[T]) } 

        // Average each new centroid
        // val centsOut = SRAM[T](MAXK, MAXD)
        Foreach(K by 1, D par P0){(ct,d) =>
          cts(ct, d) = mux(centCount(ct) == 0.to[T], 0.to[T], newCents(ct,d) / centCount(ct)) //updateMux
        }
      }

      val flatCts = SRAM[T](MAXK * MAXD)
      Foreach(K by 1, D by 1 par PX) {(i,j) => // Parallelize when issue #159 is fixed
        flatCts(i.to[Index]*D+j.to[Index]) = cts(i,j)
      }
      // Store the centroids out
      centroids(0::K*D par par_store) store flatCts
    }

    getMem(centroids)
  }

  @virtualize
  def main() {
    val iters = args(0).to[Int]
    val N = args(1).to[Int]
    val K = numcents //args(2).to[SInt];
    val D = dim //args(3).to[SInt];

    val pts = Array.tabulate(N){i => Array.tabulate(D){d => if (d == D-1) 1.to[X] else random[X](element_max) + i }}
    val cnts = Array.tabulate(K){i => Array.tabulate(D){d => if (d == D-1) 1.to[X] else random[X](element_max) + (i*N/K) }}

    val result = kmeans(pts.flatten, cnts.flatten, N, K, D, iters)

    val cts = Array.empty[Array[X]](K)
    for (k <- 0 until K) {
      cts(k) = Array.tabulate(D){i => pts(k).apply(i) }
    }
    val ii = Array.tabulate(K){i => i}

    for(epoch <- 0 until iters) {
      def dist[T:Type:Num](p1: Array[T], p2: Array[T]) = p1.zip(p2){(a,b) => (a - b)**2 }.reduce(_+_)

      // Make weighted points
      val map = pts.groupByReduce{pt =>
        val dists = cts.map{ct => dist(ct, pt) }
        dists.zip(ii){(a,b) => pack(a,b) }.reduce{(a,b) => if (a._1 < b._1) a else b}._2  // minIndex
      }{pt => pt}{(x,y) => x.zip(y){_+_} }

      // Average
      for (k <- 0 until K) {
        if (!map.contains(k)) {
          cts(k) = Array.tabulate(D){d => 0.to[X]}
        } else {
          val wp = map(k)
          val n  = wp(D - 1)
          cts(k) = Array.tabulate(D){d => if (d == D-1) 1.to[X] else wp(d)/n }
        }
      }
    }

    val gold = cts.flatten

    println("\n\nOriginal Centers:")
    (0 until K).foreach{ i => printArray(cnts(i))}
    println("\n\nOriginal Points:")
    (0 until N).foreach{ i => printArray(pts(i))}
    println("\n\nCorrect Centers:")
    (0 until K).foreach{ i => printArray(cts(i))}
    println("\n\nFPGA Centers:")
    (0 until K).foreach { i => 
      val resrow = Array.tabulate(D){j => result(i*D + j)}
      printArray(resrow)
    }

    val cksum = result.zip(gold){ case (o, g) => (g < (o + margin)) && g > (o - margin)}.reduce{_&&_}

    println("PASS: " + cksum + " (Kmeans)")
  }
}

object Kmeans2 extends SpatialApp { // Regression (Dense) // Args: 3 64
  override val target = Zynq

  type X = Int

  val numcents = 16
  val dim = 32
  val pts_per_ld = 1 // ???

  val element_max = 10
  val margin = (element_max * 0.2).to[X]

  val MAXK = numcents
  val MAXD = dim

  @virtualize
  def kmeans[T:Type:Num](points_in: Array[T], cent_inits: Array[T], numPoints: Int, numCents: Int, numDims: Int, it: Int) = {
    bound(numPoints) = 960000
    bound(numCents) = MAXK
    bound(numDims) = MAXD

    val BN = pts_per_ld (96 -> 96 -> 9600)
    val BD = MAXD
    val par_load = 16
    val par_store = 16
    val PX = 1 (1 -> 1)
    val P0 = 1 (1 -> 2 -> dim)
    val P1 = 8 (1 -> 2 -> dim)
    val P2 = 4 (1 -> 2 -> dim)
    val P3 = 16 (1 -> 2 -> numcents)

    val iters = ArgIn[Int]
    val N     = ArgIn[Int]
    val K     = numCents //ArgIn[Int]
    val D     = numDims //ArgIn[Int]

    setArg(iters, it)
    setArg(N, numPoints)
    // setArg(K, numCents)
    // setArg(D, numDims)

    val points = DRAM[T](N, D)    // Input points
    val centroids = DRAM[T](numcents*dim) // Output centroids
    // val init_cents = DRAM[T](K,D) // Output centroids
    setMem(points, points_in)
    // setMem(init_cents, cent_inits)


    Accel {
      val cts = SRAM[T](MAXK, MAXD)
      val newCents = SRAM[T](MAXK,MAXD)

      // Load initial centroids (from points)
      cts load points(0::K, 0::D par par_load)

      // Initialize newCents
      // FPGA:
      Foreach(K by 1, D by 1 par P0) {(i,j) => newCents(i,j) = cts(i,j)} 

      val DM1 = D - 1

      Sequential.Foreach(iters by 1){epoch =>
        // Flush centroid accumulator
        // FPGA:
        Foreach(K by 1, D par P0){(ct,d) =>
          newCents(ct,d) = 0.to[T]
        }

        // For each set of points
        Foreach(N by BN par PX){i =>
          val pts = SRAM[T](BN, BD)
          pts load points(i::i+BN, 0::BD par par_load)

          // For each point in this set
          MemFold(newCents par P0)(BN par PX){pt =>
            // Find the index of the closest centroid
            val accum = Reg[Tup2[Int,T]]( pack(0.to[Int], 100000.to[T]) )
            val minCent = Reduce(accum)(K par PX){ct =>
              val dist = Reg[T](0.to[T])
              Reduce(dist)(D par P2){d => (pts(pt,d) - cts(ct,d)) ** 2 }{_+_}
              pack(ct, dist.value)
            }{(a,b) =>
              mux(a._2 < b._2, a, b)
            }

            // Store this point to the set of accumulators
            val localCent = SRAM[T](MAXK,MAXD)
            Foreach(K by 1, D par P2){(ct,d) =>
              //val elem = mux(d == DM1, 1.to[T], pts(pt, d)) // fix for vanishing mux
              val elem = pts(pt,d)
              localCent(ct, d) = mux(ct == minCent.value._1, elem, 0.to[T])
            }
            localCent
          }{_+_} // Add the current point to the accumulators for this centroid
        }

        val centCount = SRAM[T](MAXK)
        Foreach(K by 1 par P3){ct => centCount(ct) = max(newCents(ct,DM1), 1.to[T]) } 

        // Average each new centroid
        // val centsOut = SRAM[T](MAXK, MAXD)
        Foreach(K by 1, D par P0){(ct,d) =>
          cts(ct, d) = mux(centCount(ct) == 0.to[T], 0.to[T], newCents(ct,d) / centCount(ct)) //updateMux
        }
      }

      val flatCts = SRAM[T](MAXK * MAXD)
      Foreach(K by 1, D by 1 par PX) {(i,j) => // Parallelize when issue #159 is fixed
        flatCts(i.to[Index]*D+j.to[Index]) = cts(i,j)
      }
      // Store the centroids out
      centroids(0::K*D par par_store) store flatCts
    }

    getMem(centroids)
  }

  @virtualize
  def main() {
    val iters = args(0).to[Int]
    val N = args(1).to[Int]
    val K = numcents //args(2).to[SInt];
    val D = dim //args(3).to[SInt];

    val pts = Array.tabulate(N){i => Array.tabulate(D){d => if (d == D-1) 1.to[X] else random[X](element_max) + i }}
    val cnts = Array.tabulate(K){i => Array.tabulate(D){d => if (d == D-1) 1.to[X] else random[X](element_max) + (i*N/K) }}

    val result = kmeans(pts.flatten, cnts.flatten, N, K, D, iters)

    val cts = Array.empty[Array[X]](K)
    for (k <- 0 until K) {
      cts(k) = Array.tabulate(D){i => pts(k).apply(i) }
    }
    val ii = Array.tabulate(K){i => i}

    for(epoch <- 0 until iters) {
      def dist[T:Type:Num](p1: Array[T], p2: Array[T]) = p1.zip(p2){(a,b) => (a - b)**2 }.reduce(_+_)

      // Make weighted points
      val map = pts.groupByReduce{pt =>
        val dists = cts.map{ct => dist(ct, pt) }
        dists.zip(ii){(a,b) => pack(a,b) }.reduce{(a,b) => if (a._1 < b._1) a else b}._2  // minIndex
      }{pt => pt}{(x,y) => x.zip(y){_+_} }

      // Average
      for (k <- 0 until K) {
        if (!map.contains(k)) {
          cts(k) = Array.tabulate(D){d => 0.to[X]}
        } else {
          val wp = map(k)
          val n  = wp(D - 1)
          cts(k) = Array.tabulate(D){d => if (d == D-1) 1.to[X] else wp(d)/n }
        }
      }
    }

    val gold = cts.flatten

    println("\n\nOriginal Centers:")
    (0 until K).foreach{ i => printArray(cnts(i))}
    println("\n\nOriginal Points:")
    (0 until N).foreach{ i => printArray(pts(i))}
    println("\n\nCorrect Centers:")
    (0 until K).foreach{ i => printArray(cts(i))}
    println("\n\nFPGA Centers:")
    (0 until K).foreach { i => 
      val resrow = Array.tabulate(D){j => result(i*D + j)}
      printArray(resrow)
    }

    val cksum = result.zip(gold){ case (o, g) => (g < (o + margin)) && g > (o - margin)}.reduce{_&&_}

    println("PASS: " + cksum + " (Kmeans)")
  }
}

object Kmeans3 extends SpatialApp { // Regression (Dense) // Args: 3 64
  override val target = Zynq

  type X = Int

  val numcents = 16
  val dim = 32
  val pts_per_ld = 1 // ???

  val element_max = 10
  val margin = (element_max * 0.2).to[X]

  val MAXK = numcents
  val MAXD = dim

  @virtualize
  def kmeans[T:Type:Num](points_in: Array[T], cent_inits: Array[T], numPoints: Int, numCents: Int, numDims: Int, it: Int) = {
    bound(numPoints) = 960000
    bound(numCents) = MAXK
    bound(numDims) = MAXD

    val BN = pts_per_ld (96 -> 96 -> 9600)
    val BD = MAXD
    val par_load = 16
    val par_store = 16
    val PX = 1 (1 -> 1)
    val P0 = 1 (1 -> 2 -> dim)
    val P1 = 6 (1 -> 2 -> dim)
    val P2 = 6 (1 -> 2 -> dim)
    val P3 = 4 (1 -> 2 -> numcents)

    val iters = ArgIn[Int]
    val N     = ArgIn[Int]
    val K     = numCents //ArgIn[Int]
    val D     = numDims //ArgIn[Int]

    setArg(iters, it)
    setArg(N, numPoints)
    // setArg(K, numCents)
    // setArg(D, numDims)

    val points = DRAM[T](N, D)    // Input points
    val centroids = DRAM[T](numcents*dim) // Output centroids
    // val init_cents = DRAM[T](K,D) // Output centroids
    setMem(points, points_in)
    // setMem(init_cents, cent_inits)


    Accel {
      val cts = SRAM[T](MAXK, MAXD)
      val newCents = SRAM[T](MAXK,MAXD)

      // Load initial centroids (from points)
      cts load points(0::K, 0::D par par_load)

      // Initialize newCents
      // FPGA:
      Foreach(K by 1, D by 1 par P0) {(i,j) => newCents(i,j) = cts(i,j)} 

      val DM1 = D - 1

      Sequential.Foreach(iters by 1){epoch =>
        // Flush centroid accumulator
        // FPGA:
        Foreach(K by 1, D par P0){(ct,d) =>
          newCents(ct,d) = 0.to[T]
        }

        // For each set of points
        Foreach(N by BN par PX){i =>
          val pts = SRAM[T](BN, BD)
          pts load points(i::i+BN, 0::BD par par_load)

          // For each point in this set
          MemFold(newCents par P0)(BN par PX){pt =>
            // Find the index of the closest centroid
            val accum = Reg[Tup2[Int,T]]( pack(0.to[Int], 100000.to[T]) )
            val minCent = Reduce(accum)(K par PX){ct =>
              val dist = Reg[T](0.to[T])
              Reduce(dist)(D par P2){d => (pts(pt,d) - cts(ct,d)) ** 2 }{_+_}
              pack(ct, dist.value)
            }{(a,b) =>
              mux(a._2 < b._2, a, b)
            }

            // Store this point to the set of accumulators
            val localCent = SRAM[T](MAXK,MAXD)
            Foreach(K by 1, D par P2){(ct,d) =>
              //val elem = mux(d == DM1, 1.to[T], pts(pt, d)) // fix for vanishing mux
              val elem = pts(pt,d)
              localCent(ct, d) = mux(ct == minCent.value._1, elem, 0.to[T])
            }
            localCent
          }{_+_} // Add the current point to the accumulators for this centroid
        }

        val centCount = SRAM[T](MAXK)
        Foreach(K by 1 par P3){ct => centCount(ct) = max(newCents(ct,DM1), 1.to[T]) } 

        // Average each new centroid
        // val centsOut = SRAM[T](MAXK, MAXD)
        Foreach(K by 1, D par P0){(ct,d) =>
          cts(ct, d) = mux(centCount(ct) == 0.to[T], 0.to[T], newCents(ct,d) / centCount(ct)) //updateMux
        }
      }

      val flatCts = SRAM[T](MAXK * MAXD)
      Foreach(K by 1, D by 1 par PX) {(i,j) => // Parallelize when issue #159 is fixed
        flatCts(i.to[Index]*D+j.to[Index]) = cts(i,j)
      }
      // Store the centroids out
      centroids(0::K*D par par_store) store flatCts
    }

    getMem(centroids)
  }

  @virtualize
  def main() {
    val iters = args(0).to[Int]
    val N = args(1).to[Int]
    val K = numcents //args(2).to[SInt];
    val D = dim //args(3).to[SInt];

    val pts = Array.tabulate(N){i => Array.tabulate(D){d => if (d == D-1) 1.to[X] else random[X](element_max) + i }}
    val cnts = Array.tabulate(K){i => Array.tabulate(D){d => if (d == D-1) 1.to[X] else random[X](element_max) + (i*N/K) }}

    val result = kmeans(pts.flatten, cnts.flatten, N, K, D, iters)

    val cts = Array.empty[Array[X]](K)
    for (k <- 0 until K) {
      cts(k) = Array.tabulate(D){i => pts(k).apply(i) }
    }
    val ii = Array.tabulate(K){i => i}

    for(epoch <- 0 until iters) {
      def dist[T:Type:Num](p1: Array[T], p2: Array[T]) = p1.zip(p2){(a,b) => (a - b)**2 }.reduce(_+_)

      // Make weighted points
      val map = pts.groupByReduce{pt =>
        val dists = cts.map{ct => dist(ct, pt) }
        dists.zip(ii){(a,b) => pack(a,b) }.reduce{(a,b) => if (a._1 < b._1) a else b}._2  // minIndex
      }{pt => pt}{(x,y) => x.zip(y){_+_} }

      // Average
      for (k <- 0 until K) {
        if (!map.contains(k)) {
          cts(k) = Array.tabulate(D){d => 0.to[X]}
        } else {
          val wp = map(k)
          val n  = wp(D - 1)
          cts(k) = Array.tabulate(D){d => if (d == D-1) 1.to[X] else wp(d)/n }
        }
      }
    }

    val gold = cts.flatten

    println("\n\nOriginal Centers:")
    (0 until K).foreach{ i => printArray(cnts(i))}
    println("\n\nOriginal Points:")
    (0 until N).foreach{ i => printArray(pts(i))}
    println("\n\nCorrect Centers:")
    (0 until K).foreach{ i => printArray(cts(i))}
    println("\n\nFPGA Centers:")
    (0 until K).foreach { i => 
      val resrow = Array.tabulate(D){j => result(i*D + j)}
      printArray(resrow)
    }

    val cksum = result.zip(gold){ case (o, g) => (g < (o + margin)) && g > (o - margin)}.reduce{_&&_}

    println("PASS: " + cksum + " (Kmeans)")
  }
}

object Sobel1 extends SpatialApp { // Regression (Dense) // Args: none


  val Kh = 3
  val Kw = 3
  val Cmax = 1024

  @virtualize
  def convolve[T:Type:Num](image: Matrix[T]): Matrix[T] = {
    val B = 16 (1 -> 1 -> 16)

    val R = ArgIn[Int]
    val C = ArgIn[Int]
    setArg(R, image.rows)
    setArg(C, image.cols)


    val lb_par = 16 (1 -> 1 -> 16)
    val par_store = 16
    val row_stride = 100 (100 -> 100 -> 500)
    val row_par = 8 (1 -> 1 -> 16)
    val par_Kh = 1 (1 -> 1 -> 3)
    val par_Kw = 1 (1 -> 1 -> 3)

    val img = DRAM[T](R, C)
    val imgOut = DRAM[T](R, C)

    setMem(img, image)

    Accel {
      Foreach(R by row_stride par row_par){ rr => 
        val lb = LineBuffer[T](Kh, Cmax)
        val sr = RegFile[T](Kh, Kw)
        val lineOut = SRAM[T](Cmax)
        val kh = LUT[T](3,3)(1.to[T], 0.to[T], -1.to[T],
                             2.to[T], 0.to[T], -2.to[T],
                             1.to[T], 0.to[T], -1.to[T])
        val kv = LUT[T](3,3)(1.to[T],  2.to[T],  1.to[T],
                             0.to[T],  0.to[T],  0.to[T],
                            -1.to[T], -2.to[T], -1.to[T])

        Foreach(0 until row_stride+2) { r =>
          val ldaddr = if (r.to[Index]+rr.to[Index] >= R.value) 0.to[Index] else {r.to[Index]+rr.to[Index]} 
          lb load img(ldaddr, 0::C par lb_par)

          /*println("Row " + r)
          Foreach(0 until Kh) { i =>
            Foreach(0 until C) { c => print("" + lb(i,c) + "\t") }
            println("")
          }*/

          Foreach(0 until C) { c =>
            Pipe{sr.reset(c == 0)}

            Foreach(0 until Kh par Kh){i => sr(i, *) <<= lb(i, c) }
            
            val horz = Reduce(Reg[T])(Kh by 1 par par_Kh){i =>
              Reduce(Reg[T])(Kw by 1 par par_Kw){j => 
              // val number = mux((r < 2) || (c < 2) , 0.to[T], sr(i,j))
              // number * kh(i,j) 
                sr(i,j) * kh(i,j)
              }{_+_}
            }{_+_}
            val vert = Reduce(Reg[T])(Kh by 1 par par_Kh){i => 
              Reduce(Reg[T])(Kw by 1 par par_Kw){j => 
              // val number = mux((r < 2) || (c < 2) , 0.to[T], sr(i,j))
              // number * kv(i,j) 
                sr(i,j) * kv(i,j)
              }{_+_}
            }{_+_}

            lineOut(c) = mux(r.to[Index] < 2.to[Index], 0.to[T], abs(horz.value) + abs(vert.value))// Technically should be sqrt(horz**2 + vert**2)
          }

          if (r.to[Index]+rr.to[Index] < R) {imgOut(r.to[Index]+rr.to[Index], 0::C par par_store) store lineOut}
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
      // Shift result down by 2 and over by 2 because of the way accel is written
      val px00 = if ((j-2) > border && (j-2) < C-border && (i-2) > border && (i-2) < C - border) (i-2)*16 else 0
      val px01 = if ((j-1) > border && (j-1) < C-border && (i-2) > border && (i-2) < C - border) (i-2)*16 else 0
      val px02 = if ((j+0) > border && (j+0) < C-border && (i-2) > border && (i-2) < C - border) (i-2)*16 else 0
      val px10 = if ((j-2) > border && (j-2) < C-border && (i-1) > border && (i-1) < C - border) (i-1)*16 else 0
      val px11 = if ((j-1) > border && (j-1) < C-border && (i-1) > border && (i-1) < C - border) (i-1)*16 else 0
      val px12 = if ((j+0) > border && (j+0) < C-border && (i-1) > border && (i-1) < C - border) (i-1)*16 else 0
      val px20 = if ((j-2) > border && (j-2) < C-border && (i+0) > border && (i+0) < C - border) (i+0)*16 else 0
      val px21 = if ((j-1) > border && (j-1) < C-border && (i+0) > border && (i+0) < C - border) (i+0)*16 else 0
      val px22 = if ((j+0) > border && (j+0) < C-border && (i+0) > border && (i+0) < C - border) (i+0)*16 else 0
      abs(px00 * 1 + px01 * 2 + px02 * 1 - px20 * 1 - px21 * 2 - px22 * 1) + abs(px00 * 1 - px02 * 1 + px10 * 2 - px12 * 2 + px20 * 1 - px22 * 1)
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
    println("PASS: " + cksum + " (Convolution_FPGA)")



  }
}

object Sobel2 extends SpatialApp { // Regression (Dense) // Args: none


  val Kh = 3
  val Kw = 3
  val Cmax = 1024

  @virtualize
  def convolve[T:Type:Num](image: Matrix[T]): Matrix[T] = {
    val B = 16 (1 -> 1 -> 16)

    val R = ArgIn[Int]
    val C = ArgIn[Int]
    setArg(R, image.rows)
    setArg(C, image.cols)


    val lb_par = 16 (1 -> 1 -> 16)
    val par_store = 16
    val row_stride = 100 (100 -> 100 -> 500)
    val row_par = 4 (1 -> 1 -> 16)
    val par_Kh = 3 (1 -> 1 -> 3)
    val par_Kw = 3 (1 -> 1 -> 3)

    val img = DRAM[T](R, C)
    val imgOut = DRAM[T](R, C)

    setMem(img, image)

    Accel {
      Foreach(R by row_stride par row_par){ rr => 
        val lb = LineBuffer[T](Kh, Cmax)
        val sr = RegFile[T](Kh, Kw)
        val lineOut = SRAM[T](Cmax)
        val kh = LUT[T](3,3)(1.to[T], 0.to[T], -1.to[T],
                             2.to[T], 0.to[T], -2.to[T],
                             1.to[T], 0.to[T], -1.to[T])
        val kv = LUT[T](3,3)(1.to[T],  2.to[T],  1.to[T],
                             0.to[T],  0.to[T],  0.to[T],
                            -1.to[T], -2.to[T], -1.to[T])

        Foreach(0 until row_stride+2) { r =>
          val ldaddr = if (r.to[Index]+rr.to[Index] >= R.value) 0.to[Index] else {r.to[Index]+rr.to[Index]} 
          lb load img(ldaddr, 0::C par lb_par)

          /*println("Row " + r)
          Foreach(0 until Kh) { i =>
            Foreach(0 until C) { c => print("" + lb(i,c) + "\t") }
            println("")
          }*/

          Foreach(0 until C) { c =>
            Pipe{sr.reset(c == 0)}

            Foreach(0 until Kh par Kh){i => sr(i, *) <<= lb(i, c) }
            
            val horz = Reduce(Reg[T])(Kh by 1 par par_Kh){i =>
              Reduce(Reg[T])(Kw by 1 par par_Kw){j => 
              // val number = mux((r < 2) || (c < 2) , 0.to[T], sr(i,j))
              // number * kh(i,j) 
                sr(i,j) * kh(i,j)
              }{_+_}
            }{_+_}
            val vert = Reduce(Reg[T])(Kh by 1 par par_Kh){i => 
              Reduce(Reg[T])(Kw by 1 par par_Kw){j => 
              // val number = mux((r < 2) || (c < 2) , 0.to[T], sr(i,j))
              // number * kv(i,j) 
                sr(i,j) * kv(i,j)
              }{_+_}
            }{_+_}

            lineOut(c) = mux(r.to[Index] < 2.to[Index], 0.to[T], abs(horz.value) + abs(vert.value))// Technically should be sqrt(horz**2 + vert**2)
          }

          if (r.to[Index]+rr.to[Index] < R) {imgOut(r.to[Index]+rr.to[Index], 0::C par par_store) store lineOut}
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
      // Shift result down by 2 and over by 2 because of the way accel is written
      val px00 = if ((j-2) > border && (j-2) < C-border && (i-2) > border && (i-2) < C - border) (i-2)*16 else 0
      val px01 = if ((j-1) > border && (j-1) < C-border && (i-2) > border && (i-2) < C - border) (i-2)*16 else 0
      val px02 = if ((j+0) > border && (j+0) < C-border && (i-2) > border && (i-2) < C - border) (i-2)*16 else 0
      val px10 = if ((j-2) > border && (j-2) < C-border && (i-1) > border && (i-1) < C - border) (i-1)*16 else 0
      val px11 = if ((j-1) > border && (j-1) < C-border && (i-1) > border && (i-1) < C - border) (i-1)*16 else 0
      val px12 = if ((j+0) > border && (j+0) < C-border && (i-1) > border && (i-1) < C - border) (i-1)*16 else 0
      val px20 = if ((j-2) > border && (j-2) < C-border && (i+0) > border && (i+0) < C - border) (i+0)*16 else 0
      val px21 = if ((j-1) > border && (j-1) < C-border && (i+0) > border && (i+0) < C - border) (i+0)*16 else 0
      val px22 = if ((j+0) > border && (j+0) < C-border && (i+0) > border && (i+0) < C - border) (i+0)*16 else 0
      abs(px00 * 1 + px01 * 2 + px02 * 1 - px20 * 1 - px21 * 2 - px22 * 1) + abs(px00 * 1 - px02 * 1 + px10 * 2 - px12 * 2 + px20 * 1 - px22 * 1)
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
    println("PASS: " + cksum + " (Convolution_FPGA)")



  }
}

object Sobel3 extends SpatialApp { // Regression (Dense) // Args: none


  val Kh = 3
  val Kw = 3
  val Cmax = 1024

  @virtualize
  def convolve[T:Type:Num](image: Matrix[T]): Matrix[T] = {
    val B = 16 (1 -> 1 -> 16)

    val R = ArgIn[Int]
    val C = ArgIn[Int]
    setArg(R, image.rows)
    setArg(C, image.cols)


    val lb_par = 8 (1 -> 1 -> 16)
    val par_store = 16
    val row_stride = 100 (100 -> 100 -> 500)
    val row_par = 4 (1 -> 1 -> 16)
    val par_Kh = 3 (1 -> 1 -> 3)
    val par_Kw = 3 (1 -> 1 -> 3)

    val img = DRAM[T](R, C)
    val imgOut = DRAM[T](R, C)

    setMem(img, image)

    Accel {
      Foreach(R by row_stride par row_par){ rr => 
        val lb = LineBuffer[T](Kh, Cmax)
        val sr = RegFile[T](Kh, Kw)
        val lineOut = SRAM[T](Cmax)
        val kh = LUT[T](3,3)(1.to[T], 0.to[T], -1.to[T],
                             2.to[T], 0.to[T], -2.to[T],
                             1.to[T], 0.to[T], -1.to[T])
        val kv = LUT[T](3,3)(1.to[T],  2.to[T],  1.to[T],
                             0.to[T],  0.to[T],  0.to[T],
                            -1.to[T], -2.to[T], -1.to[T])

        Foreach(0 until row_stride+2) { r =>
          val ldaddr = if (r.to[Index]+rr.to[Index] >= R.value) 0.to[Index] else {r.to[Index]+rr.to[Index]} 
          lb load img(ldaddr, 0::C par lb_par)

          /*println("Row " + r)
          Foreach(0 until Kh) { i =>
            Foreach(0 until C) { c => print("" + lb(i,c) + "\t") }
            println("")
          }*/

          Foreach(0 until C) { c =>
            Pipe{sr.reset(c == 0)}

            Foreach(0 until Kh par Kh){i => sr(i, *) <<= lb(i, c) }
            
            val horz = Reduce(Reg[T])(Kh by 1 par par_Kh){i =>
              Reduce(Reg[T])(Kw by 1 par par_Kw){j => 
              // val number = mux((r < 2) || (c < 2) , 0.to[T], sr(i,j))
              // number * kh(i,j) 
                sr(i,j) * kh(i,j)
              }{_+_}
            }{_+_}
            val vert = Reduce(Reg[T])(Kh by 1 par par_Kh){i => 
              Reduce(Reg[T])(Kw by 1 par par_Kw){j => 
              // val number = mux((r < 2) || (c < 2) , 0.to[T], sr(i,j))
              // number * kv(i,j) 
                sr(i,j) * kv(i,j)
              }{_+_}
            }{_+_}

            lineOut(c) = mux(r.to[Index] < 2.to[Index], 0.to[T], abs(horz.value) + abs(vert.value))// Technically should be sqrt(horz**2 + vert**2)
          }

          if (r.to[Index]+rr.to[Index] < R) {imgOut(r.to[Index]+rr.to[Index], 0::C par par_store) store lineOut}
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
      // Shift result down by 2 and over by 2 because of the way accel is written
      val px00 = if ((j-2) > border && (j-2) < C-border && (i-2) > border && (i-2) < C - border) (i-2)*16 else 0
      val px01 = if ((j-1) > border && (j-1) < C-border && (i-2) > border && (i-2) < C - border) (i-2)*16 else 0
      val px02 = if ((j+0) > border && (j+0) < C-border && (i-2) > border && (i-2) < C - border) (i-2)*16 else 0
      val px10 = if ((j-2) > border && (j-2) < C-border && (i-1) > border && (i-1) < C - border) (i-1)*16 else 0
      val px11 = if ((j-1) > border && (j-1) < C-border && (i-1) > border && (i-1) < C - border) (i-1)*16 else 0
      val px12 = if ((j+0) > border && (j+0) < C-border && (i-1) > border && (i-1) < C - border) (i-1)*16 else 0
      val px20 = if ((j-2) > border && (j-2) < C-border && (i+0) > border && (i+0) < C - border) (i+0)*16 else 0
      val px21 = if ((j-1) > border && (j-1) < C-border && (i+0) > border && (i+0) < C - border) (i+0)*16 else 0
      val px22 = if ((j+0) > border && (j+0) < C-border && (i+0) > border && (i+0) < C - border) (i+0)*16 else 0
      abs(px00 * 1 + px01 * 2 + px02 * 1 - px20 * 1 - px21 * 2 - px22 * 1) + abs(px00 * 1 - px02 * 1 + px10 * 2 - px12 * 2 + px20 * 1 - px22 * 1)
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
    println("PASS: " + cksum + " (Convolution_FPGA)")



  }
}


object GDA1 extends SpatialApp { // Regression (Dense) // Args: 64


  type X = Float

  val MAXC = 96
  val C = MAXC
  val margin = 1



  @virtualize
  def gda[T: Type : Num](xCPU: Array[T], yCPU: Array[Int], mu0CPU: Array[T], mu1CPU: Array[T]) = {
    val rTileSize = 32(96 -> 19200)
    val op = 2(1 -> 8)
    val ip = 2(1 -> 12)
    val subLoopPar = 2(1 -> 16)
    val prodLoopPar = 8(1 -> 96)
    val outerAccumPar = 4(1 -> 1)

    val rows = yCPU.length;
    bound(rows) = 360000
    val cols = mu0CPU.length;
    bound(cols) = MAXC

    val R = ArgIn[Int]
    // val C = ArgIn[SInt]
    // setArg(C, cols)
    setArg(R, rows)

    val x = DRAM[T](R, C)
    val y = DRAM[Int](R)
    val mu0 = DRAM[T](C)
    val mu1 = DRAM[T](C)
    val sigma = DRAM[T](C, C)

    setMem(x, xCPU)
    setMem(y, yCPU)
    setMem(mu0, mu0CPU)
    setMem(mu1, mu1CPU)

    Accel {
      val mu0Tile = SRAM[T](MAXC)
      val mu1Tile = SRAM[T](MAXC)
      Parallel {
        mu0Tile load mu0(0 :: C par 16) // Load mu0
        mu1Tile load mu1(0 :: C par 16) // Load mu1
      }

      val sigmaOut = SRAM[T](MAXC, MAXC)

      MemReduce(sigmaOut)(R by rTileSize par op){ r =>
        val gdaYtile = SRAM[Int](rTileSize)
        val gdaXtile = SRAM[T](rTileSize, MAXC)
        val blk = Reg[Int]
        Parallel {
          gdaYtile load y(r :: r + rTileSize par 16)
          gdaXtile load x(r :: r + rTileSize, 0 :: C par 16) // Load tile of x
          Pipe {
            blk := min(R.value - r, rTileSize)
          }
        }

        val sigmaBlk = SRAM[T](MAXC, MAXC)

        MemReduce(sigmaBlk)(blk par param(1)) { rr =>
          val subTile = SRAM[T](MAXC)
          val sigmaTile = SRAM[T](MAXC, MAXC)
          Foreach(C par subLoopPar) { cc =>
            subTile(cc) = gdaXtile(rr, cc) - mux(gdaYtile(rr) == 1, mu1Tile(cc), mu0Tile(cc))
          }
          Foreach(C by 1, C par ip) { (ii, jj) =>
            sigmaTile(ii, jj) = subTile(ii) * subTile(jj)
          }
          sigmaTile
        }{_+_}
      }{_+_}

      sigma(0 :: C, 0 :: C par 16) store sigmaOut
    }

    getMem(sigma)
  }


  def printArr(a: Array[Int], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  @virtualize
  def main() {
    val R = args(0).to[Int]
    // val C = args(0).to[SInt] // TODO: Should be selectable up to maximum

    // val x  = Array.fill(R){ Array.fill(C){ random[X](10) }}
    // val ys = Array.fill(R){ random[Int](1) }
    // val mu0 = Array.fill(C){ random[X](10) }
    // val mu1 = Array.fill(C){ random[X](10) }

    val x = Array.tabulate(R) { i => Array.tabulate(C) { j => (i * C + j) % 256 } }
    val ys = Array.tabulate(R) { i => i % 256 }
    val mu0 = Array.tabulate(C) { i => i % 2 }
    val mu1 = Array.tabulate(C) { i => i % 2 }

    val result = gda(x.flatten, ys, mu0, mu1)

    val gold = x.zip(ys) { (row, y) =>
      val sub = if (y == 1) row.zip(mu1){_-_} else row.zip(mu0) {_-_}
      Array.tabulate(C) { i => Array.tabulate(C) { j => sub(i) * sub(j) } }.flatten
    }.reduce { (a, b) => a.zip(b) {_+_} }

    printArr(gold, "gold: ")
    printArr(result, "result: ")

    val cksum = gold.zip(result){ case (a,b) => a < b + margin && a > b - margin }.reduce{_&&_}
    println("PASS: " + cksum  + " (GDA)")

    // // println("actual: " + gold.mkString(", "))
    // //println("result: " + result.mkString(", "))
    // // println("Sum of differences: " + gold.zip(result){_-_}.reduce{_+_})
    // printArr(gold, "gold: ")
    // printArr(result, "result: ")

    // val cksum = gold.zip(result){ case (a,b) => a < b + margin && a > b - margin }.reduce{_&&_}
    // println("PASS: " + cksum  + " (GDA)")

    // assert( result == gold )
  }

}

object GDA2 extends SpatialApp { // Regression (Dense) // Args: 64


  type X = Float

  val MAXC = 96
  val C = MAXC
  val margin = 1



  @virtualize
  def gda[T: Type : Num](xCPU: Array[T], yCPU: Array[Int], mu0CPU: Array[T], mu1CPU: Array[T]) = {
    val rTileSize = 32(96 -> 19200)
    val op = 1(1 -> 8)
    val ip = 8(1 -> 12)
    val subLoopPar = 16(1 -> 16)
    val prodLoopPar = 16(1 -> 96)
    val outerAccumPar = 4(1 -> 1)

    val rows = yCPU.length;
    bound(rows) = 360000
    val cols = mu0CPU.length;
    bound(cols) = MAXC

    val R = ArgIn[Int]
    // val C = ArgIn[SInt]
    // setArg(C, cols)
    setArg(R, rows)

    val x = DRAM[T](R, C)
    val y = DRAM[Int](R)
    val mu0 = DRAM[T](C)
    val mu1 = DRAM[T](C)
    val sigma = DRAM[T](C, C)

    setMem(x, xCPU)
    setMem(y, yCPU)
    setMem(mu0, mu0CPU)
    setMem(mu1, mu1CPU)

    Accel {
      val mu0Tile = SRAM[T](MAXC)
      val mu1Tile = SRAM[T](MAXC)
      Parallel {
        mu0Tile load mu0(0 :: C par 16) // Load mu0
        mu1Tile load mu1(0 :: C par 16) // Load mu1
      }

      val sigmaOut = SRAM[T](MAXC, MAXC)

      MemReduce(sigmaOut)(R by rTileSize par op){ r =>
        val gdaYtile = SRAM[Int](rTileSize)
        val gdaXtile = SRAM[T](rTileSize, MAXC)
        val blk = Reg[Int]
        Parallel {
          gdaYtile load y(r :: r + rTileSize par 16)
          gdaXtile load x(r :: r + rTileSize, 0 :: C par 16) // Load tile of x
          Pipe {
            blk := min(R.value - r, rTileSize)
          }
        }

        val sigmaBlk = SRAM[T](MAXC, MAXC)

        MemReduce(sigmaBlk)(blk par param(1)) { rr =>
          val subTile = SRAM[T](MAXC)
          val sigmaTile = SRAM[T](MAXC, MAXC)
          Foreach(C par subLoopPar) { cc =>
            subTile(cc) = gdaXtile(rr, cc) - mux(gdaYtile(rr) == 1, mu1Tile(cc), mu0Tile(cc))
          }
          Foreach(C by 1, C par ip) { (ii, jj) =>
            sigmaTile(ii, jj) = subTile(ii) * subTile(jj)
          }
          sigmaTile
        }{_+_}
      }{_+_}

      sigma(0 :: C, 0 :: C par 16) store sigmaOut
    }

    getMem(sigma)
  }


  def printArr(a: Array[Int], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  @virtualize
  def main() {
    val R = args(0).to[Int]
    // val C = args(0).to[SInt] // TODO: Should be selectable up to maximum

    // val x  = Array.fill(R){ Array.fill(C){ random[X](10) }}
    // val ys = Array.fill(R){ random[Int](1) }
    // val mu0 = Array.fill(C){ random[X](10) }
    // val mu1 = Array.fill(C){ random[X](10) }

    val x = Array.tabulate(R) { i => Array.tabulate(C) { j => (i * C + j) % 256 } }
    val ys = Array.tabulate(R) { i => i % 256 }
    val mu0 = Array.tabulate(C) { i => i % 2 }
    val mu1 = Array.tabulate(C) { i => i % 2 }

    val result = gda(x.flatten, ys, mu0, mu1)

    val gold = x.zip(ys) { (row, y) =>
      val sub = if (y == 1) row.zip(mu1){_-_} else row.zip(mu0) {_-_}
      Array.tabulate(C) { i => Array.tabulate(C) { j => sub(i) * sub(j) } }.flatten
    }.reduce { (a, b) => a.zip(b) {_+_} }

    printArr(gold, "gold: ")
    printArr(result, "result: ")

    val cksum = gold.zip(result){ case (a,b) => a < b + margin && a > b - margin }.reduce{_&&_}
    println("PASS: " + cksum  + " (GDA)")

    // // println("actual: " + gold.mkString(", "))
    // //println("result: " + result.mkString(", "))
    // // println("Sum of differences: " + gold.zip(result){_-_}.reduce{_+_})
    // printArr(gold, "gold: ")
    // printArr(result, "result: ")

    // val cksum = gold.zip(result){ case (a,b) => a < b + margin && a > b - margin }.reduce{_&&_}
    // println("PASS: " + cksum  + " (GDA)")

    // assert( result == gold )
  }

}

object GDA3 extends SpatialApp { // Regression (Dense) // Args: 64


  type X = Float

  val MAXC = 96
  val C = MAXC
  val margin = 1



  @virtualize
  def gda[T: Type : Num](xCPU: Array[T], yCPU: Array[Int], mu0CPU: Array[T], mu1CPU: Array[T]) = {
    val rTileSize = 32(96 -> 19200)
    val op = 1(1 -> 8)
    val ip = 2(1 -> 12)
    val subLoopPar = 8(1 -> 16)
    val prodLoopPar = 32(1 -> 96)
    val outerAccumPar = 4(1 -> 1)

    val rows = yCPU.length;
    bound(rows) = 360000
    val cols = mu0CPU.length;
    bound(cols) = MAXC

    val R = ArgIn[Int]
    // val C = ArgIn[SInt]
    // setArg(C, cols)
    setArg(R, rows)

    val x = DRAM[T](R, C)
    val y = DRAM[Int](R)
    val mu0 = DRAM[T](C)
    val mu1 = DRAM[T](C)
    val sigma = DRAM[T](C, C)

    setMem(x, xCPU)
    setMem(y, yCPU)
    setMem(mu0, mu0CPU)
    setMem(mu1, mu1CPU)

    Accel {
      val mu0Tile = SRAM[T](MAXC)
      val mu1Tile = SRAM[T](MAXC)
      Parallel {
        mu0Tile load mu0(0 :: C par 16) // Load mu0
        mu1Tile load mu1(0 :: C par 16) // Load mu1
      }

      val sigmaOut = SRAM[T](MAXC, MAXC)

      MemReduce(sigmaOut)(R by rTileSize par op){ r =>
        val gdaYtile = SRAM[Int](rTileSize)
        val gdaXtile = SRAM[T](rTileSize, MAXC)
        val blk = Reg[Int]
        Parallel {
          gdaYtile load y(r :: r + rTileSize par 16)
          gdaXtile load x(r :: r + rTileSize, 0 :: C par 16) // Load tile of x
          Pipe {
            blk := min(R.value - r, rTileSize)
          }
        }

        val sigmaBlk = SRAM[T](MAXC, MAXC)

        MemReduce(sigmaBlk)(blk par param(1)) { rr =>
          val subTile = SRAM[T](MAXC)
          val sigmaTile = SRAM[T](MAXC, MAXC)
          Foreach(C par subLoopPar) { cc =>
            subTile(cc) = gdaXtile(rr, cc) - mux(gdaYtile(rr) == 1, mu1Tile(cc), mu0Tile(cc))
          }
          Foreach(C by 1, C par ip) { (ii, jj) =>
            sigmaTile(ii, jj) = subTile(ii) * subTile(jj)
          }
          sigmaTile
        }{_+_}
      }{_+_}

      sigma(0 :: C, 0 :: C par 16) store sigmaOut
    }

    getMem(sigma)
  }


  def printArr(a: Array[Int], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  @virtualize
  def main() {
    val R = args(0).to[Int]
    // val C = args(0).to[SInt] // TODO: Should be selectable up to maximum

    // val x  = Array.fill(R){ Array.fill(C){ random[X](10) }}
    // val ys = Array.fill(R){ random[Int](1) }
    // val mu0 = Array.fill(C){ random[X](10) }
    // val mu1 = Array.fill(C){ random[X](10) }

    val x = Array.tabulate(R) { i => Array.tabulate(C) { j => (i * C + j) % 256 } }
    val ys = Array.tabulate(R) { i => i % 256 }
    val mu0 = Array.tabulate(C) { i => i % 2 }
    val mu1 = Array.tabulate(C) { i => i % 2 }

    val result = gda(x.flatten, ys, mu0, mu1)

    val gold = x.zip(ys) { (row, y) =>
      val sub = if (y == 1) row.zip(mu1){_-_} else row.zip(mu0) {_-_}
      Array.tabulate(C) { i => Array.tabulate(C) { j => sub(i) * sub(j) } }.flatten
    }.reduce { (a, b) => a.zip(b) {_+_} }

    printArr(gold, "gold: ")
    printArr(result, "result: ")

    val cksum = gold.zip(result){ case (a,b) => a < b + margin && a > b - margin }.reduce{_&&_}
    println("PASS: " + cksum  + " (GDA)")

    // // println("actual: " + gold.mkString(", "))
    // //println("result: " + result.mkString(", "))
    // // println("Sum of differences: " + gold.zip(result){_-_}.reduce{_+_})
    // printArr(gold, "gold: ")
    // printArr(result, "result: ")

    // val cksum = gold.zip(result){ case (a,b) => a < b + margin && a > b - margin }.reduce{_&&_}
    // println("PASS: " + cksum  + " (GDA)")

    // assert( result == gold )
  }

}
