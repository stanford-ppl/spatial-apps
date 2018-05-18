import spatial.dsl._
import spatial.targets._
import virtualized._

/*

optiQL
trait TPCHQ6Trait extends TPCHBaseTrait {
  val queryName = "Q6"

  def query() = {
    val lineItems = loadLineItems()
    tic(lineItems.size)

    //FIXME: infix_&& fails to resolve automatically
    val q = lineItems Where (l => infix_&&(l.l_shipdate >= Date("1994-01-01"), infix_&&(l.l_shipdate < Date("1995-01-01"), infix_&&(l.l_discount >= 0.05, infix_&&(l.l_discount <= 0.07, l.l_quantity < 24)))))
    val revenue = q.Sum(l => l.l_extendedprice * l.l_discount)

    toc(revenue)
    println(revenue)
  }
}


SQL:
SELECT
    sum(l_extendedprice * l_discount) as revenue
FROM
    lineitem
WHERE
    l_shipdate >= date '1994-01-01'
    AND l_shipdate < date '1994-01-01' + interval '1' year
    AND l_discount between 0.06 - 0.01 AND 0.06 + 0.01
    AND l_quantity < 24;

*/

object BTC extends SpatialApp { // Regression (Dense) // Args: 0100000081cd02ab7e569e8bcd9317e2fe99f2de44d49ab2b8851ba4a308000000000000e320b6c2fffc8d750423db8b1eb942ae710e951ed797f7affc8892b0f1fc122bc7f5d74df2b9441a42a14695
  /*
    According to https://en.bitcoin.it/wiki/Block_hashing_algorithm
    Proof of Work = SHA256(SHA256(HEADER))

  */

  type ULong = FixPt[FALSE, _32, _0]
  type UInt8 = FixPt[FALSE, _8, _0]
  @virtualize
  def main() = {
    // Setup off-chip data

    val raw_text = args(0).to[MString]// loadCSV1D[String]("/remote/regression/data/machsuite/sha_txt.csv", "\n").apply(0)
    val data_text_int = argon.lang.String.string2num(raw_text)
    val data_text = Array.tabulate(data_text_int.length){i => data_text_int(i).to[UInt8]}
    val len = HostIO[Int]
    setArg(len, data_text.length)
    val text_dram = DRAM[UInt8](1024)
    val hash_dram = DRAM[UInt8](32)//(5)

    println("Hashing: " + raw_text + " (len: " + data_text.length + ")")
    setMem(text_dram, data_text)

    Accel{

      // Init
      val datalen = Reg[Int](0)
      val bitlen = RegFile[ULong](2, List(0.to[ULong],0.to[ULong]))
      val state = RegFile[ULong](8, List(0x6a09e667L.to[ULong],0xbb67ae85L.to[ULong],0x3c6ef372L.to[ULong],0xa54ff53aL.to[ULong],
                                       0x510e527fL.to[ULong],0x9b05688cL.to[ULong],0x1f83d9abL.to[ULong],0x5be0cd19L.to[ULong])
                              )
      val hash = SRAM[UInt8](32)
      val K_LUT = LUT[ULong](64)(
            0x428a2f98L.to[ULong],0x71374491L.to[ULong],0xb5c0fbcfL.to[ULong],0xe9b5dba5L.to[ULong],0x3956c25bL.to[ULong],0x59f111f1L.to[ULong],0x923f82a4L.to[ULong],0xab1c5ed5L.to[ULong],
            0xd807aa98L.to[ULong],0x12835b01L.to[ULong],0x243185beL.to[ULong],0x550c7dc3L.to[ULong],0x72be5d74L.to[ULong],0x80deb1feL.to[ULong],0x9bdc06a7L.to[ULong],0xc19bf174L.to[ULong],
            0xe49b69c1L.to[ULong],0xefbe4786L.to[ULong],0x0fc19dc6L.to[ULong],0x240ca1ccL.to[ULong],0x2de92c6fL.to[ULong],0x4a7484aaL.to[ULong],0x5cb0a9dcL.to[ULong],0x76f988daL.to[ULong],
            0x983e5152L.to[ULong],0xa831c66dL.to[ULong],0xb00327c8L.to[ULong],0xbf597fc7L.to[ULong],0xc6e00bf3L.to[ULong],0xd5a79147L.to[ULong],0x06ca6351L.to[ULong],0x14292967L.to[ULong],
            0x27b70a85L.to[ULong],0x2e1b2138L.to[ULong],0x4d2c6dfcL.to[ULong],0x53380d13L.to[ULong],0x650a7354L.to[ULong],0x766a0abbL.to[ULong],0x81c2c92eL.to[ULong],0x92722c85L.to[ULong],
            0xa2bfe8a1L.to[ULong],0xa81a664bL.to[ULong],0xc24b8b70L.to[ULong],0xc76c51a3L.to[ULong],0xd192e819L.to[ULong],0xd6990624L.to[ULong],0xf40e3585L.to[ULong],0x106aa070L.to[ULong],
            0x19a4c116L.to[ULong],0x1e376c08L.to[ULong],0x2748774cL.to[ULong],0x34b0bcb5L.to[ULong],0x391c0cb3L.to[ULong],0x4ed8aa4aL.to[ULong],0x5b9cca4fL.to[ULong],0x682e6ff3L.to[ULong],
            0x748f82eeL.to[ULong],0x78a5636fL.to[ULong],0x84c87814L.to[ULong],0x8cc70208L.to[ULong],0x90befffaL.to[ULong],0xa4506cebL.to[ULong],0xbef9a3f7L.to[ULong],0xc67178f2L.to[ULong]
          )

      val data = SRAM[UInt8](64)
      
      def SHFR(x: ULong, y: Int): ULong = {
        val tmp = Reg[ULong](0)
        tmp := x
        Foreach(y by 1){_ => tmp := tmp >> 1}
        tmp.value
      }

      // DBL_INT_ADD treats two unsigned ints a and b as one 64-bit integer and adds c to it
      def DBL_INT_ADD(c:ULong): Unit = {
        if (bitlen(0) > 0xffffffffL.to[ULong] - c) {bitlen(1) = bitlen(1) + 1}
        bitlen(0) = bitlen(0) + c
      }

      def SIG0(x:ULong): ULong = {
        // (ROTRIGHT(x,7) ^ ROTRIGHT(x,18) ^ ((x) >> 3))
        ( x >> 7 | x << (32-7) ) ^ ( x >> 18 | x << (32-18) ) ^ x >> 3
      }

      def SIG1(x:ULong): ULong = {
        // (ROTRIGHT(x,17) ^ ROTRIGHT(x,19) ^ ((x) >> 10))
        ( x >> 17 | x << (32-17) ) ^ ( x >> 19 | x << (32-19) ) ^ x >> 10
      }

      def CH(x:ULong, y:ULong, z:ULong): ULong = {
        // (((x) & (y)) ^ (~(x) & (z)))
        (x & y) ^ (~x & z)
      }

      def MAJ(x:ULong, y:ULong, z:ULong): ULong = {
        // (((x) & (y)) ^ ((x) & (z)) ^ ((y) & (z)))
        (x & y) ^ (x & z) ^ (y & z)
      }

      def EP0(x: ULong): ULong = {
        // (ROTRIGHT(x,2) ^ ROTRIGHT(x,13) ^ ROTRIGHT(x,22))
        ( x >> 2 | x << (32-2) ) ^ ( x >> 13 | x << (32-13) ) ^ ( x >> 22 | x << (32-22) )
      }

      def EP1(x: ULong): ULong = {
        // (ROTRIGHT(x,6) ^ ROTRIGHT(x,11) ^ ROTRIGHT(x,25))
        ( x >> 6 | x << (32-6) ) ^ ( x >> 11 | x << (32-11) ) ^ ( x >> 25 | x << (32-25) )
      }

      def sha_transform(): Unit = {
        val m = SRAM[ULong](64)
        Foreach(0 until 64 by 1){i => 
          if ( i.to[Index] < 16 ) {
            val j = 4*i.to[Index]
            // println(" m(" + i + ") = " + {(data(j).as[ULong] << 24) | (data(j+1).as[ULong] << 16) | (data(j+2).as[ULong] << 8) | (data(j+3).as[ULong])})
            m(i) = (data(j).as[ULong] << 24) | (data(j+1).as[ULong] << 16) | (data(j+2).as[ULong] << 8) | (data(j+3).as[ULong])
          } else {
            // println(" m(" + i + ") = " + SIG1(m(i-2)) + " " + m(i-7) + " " + SIG0(m(i-15)) + " " + m(i-16))
            m(i) = SIG1(m(i-2)) + m(i-7) + SIG0(m(i-15)) + m(i-16)
          } 
          // val j = 4*i.to[Index]
          // m(i) = if (i.to[Index] < 16) {(data(j).as[ULong] << 24) | (data(j+1).as[ULong] << 16) | (data(j+2).as[ULong] << 8) | (data(j+3).as[ULong])}
          //        else {SIG1(m(i-2)) + m(i-7) + SIG0(m(i-15)) + m(i-16)}
        }
        val A = Reg[ULong] 
        val B = Reg[ULong] 
        val C = Reg[ULong] 
        val D = Reg[ULong] 
        val E = Reg[ULong] 
        val F = Reg[ULong] 
        val G = Reg[ULong] 
        val H = Reg[ULong] 

        A := state(0)
        B := state(1)
        C := state(2)
        D := state(3)
        E := state(4)
        F := state(5)
        G := state(6)
        H := state(7)

        Foreach(64 by 1){ i => 
          val tmp1 = H + EP1(E) + CH(E,F,G) + K_LUT(i) + m(i)
          val tmp2 = EP0(A) + MAJ(A,B,C)
          // println(" " + i + " : " + A.value + " " + B.value + " " + 
          //   C.value + " " + D.value + " " + E.value + " " + F.value + " " + G.value + " " + H.value)
          // println("    " + H.value + " " + EP1(E) + " " + CH(E,F,G) + " " + K_LUT(i) + " " + m(i))
          H := G; G := F; F := E; E := D + tmp1; D := C; C := B; B := A; A := tmp1 + tmp2
        }

        Foreach(8 by 1 par 8){i => 
          state(i) = state(i) + mux(i.to[Index] == 0, A, mux(i.to[Index] == 1, B, mux(i.to[Index] == 2, C, mux(i.to[Index] == 3, D, 
            mux(i.to[Index] == 4, E, mux(i.to[Index] == 5, F, mux(i.to[Index] == 6, G, H)))))))
        }

      }

      def SHA256(): Unit = {
        // Init 
        Pipe{
          bitlen.reset
          state.reset
        }

        // Update
        Sequential.Foreach(0 until len.value by 64 par 1) { i => 
          datalen := min(len.value - i, 64)
          // println(" datalen " + datalen.value + " and i " + i + " and len " + len.value)
          data load text_dram(i::i+datalen.value)
          if (datalen.value == 64.to[Int]) {
            // println("doing this " + datalen.value)
            sha_transform()
            DBL_INT_ADD(512); 
          }
        }

        // Final
        val pad_stop = if (datalen.value < 56) 56 else 64
        Foreach(datalen until pad_stop by 1){i => data(i) = if (i.to[Index] == datalen) 0x80.to[UInt8] else 0.to[UInt8]}
        if (datalen.value >= 56) {
          sha_transform()
          Foreach(56 by 1){i => data(i) = 0}
        }

        DBL_INT_ADD(datalen.value.to[ULong] * 8.to[ULong])
        Pipe{data(63) = (bitlen(0)).to[UInt8]}
        Pipe{data(62) = (bitlen(0) >> 8).to[UInt8]}
        Pipe{data(61) = (bitlen(0) >> 16).to[UInt8]}
        Pipe{data(60) = (bitlen(0) >> 24).to[UInt8]}
        Pipe{data(59) = (bitlen(1)).to[UInt8]}
        Pipe{data(58) = (bitlen(1) >> 8).to[UInt8]}
        Pipe{data(57) = (bitlen(1) >> 16).to[UInt8]}
        Pipe{data(56) = (bitlen(1) >> 24).to[UInt8]}
        sha_transform()

        // Foreach(8 by 1){i => println(" " + state(i))}

        Sequential.Foreach(4 by 1){ i => 
          hash(i)    = (SHFR(state(0), (24-i.to[Index]*8))).apply(7::0).as[UInt8]
          hash(i+4)  = (SHFR(state(1), (24-i.to[Index]*8))).apply(7::0).as[UInt8]
          hash(i+8)  = (SHFR(state(2), (24-i.to[Index]*8))).apply(7::0).as[UInt8]
          hash(i+12) = (SHFR(state(3), (24-i.to[Index]*8))).apply(7::0).as[UInt8]
          hash(i+16) = (SHFR(state(4), (24-i.to[Index]*8))).apply(7::0).as[UInt8]
          hash(i+20) = (SHFR(state(5), (24-i.to[Index]*8))).apply(7::0).as[UInt8]
          hash(i+24) = (SHFR(state(6), (24-i.to[Index]*8))).apply(7::0).as[UInt8]
          hash(i+28) = (SHFR(state(7), (24-i.to[Index]*8))).apply(7::0).as[UInt8]
        }

      }

      Sequential.Foreach(2 by 1){i => 
        Pipe{SHA256()}
        if (i.to[Index] == 0) {
          text_dram(0::32) store hash
          len := 32
        }
      }
      
      hash_dram store hash
    }  

    val hashed_result = getMem(hash_dram)
    val hashed_gold = Array[UInt8](101.to[UInt8],0.to[UInt8],241.to[UInt8],59.to[UInt8],194.to[UInt8],84.to[UInt8],197.to[UInt8],158.to[UInt8],159.to[UInt8],61.to[UInt8],119.to[UInt8],189.to[UInt8],11.to[UInt8],25.to[UInt8],153.to[UInt8],230.to[UInt8],134.to[UInt8],250.to[UInt8],223.to[UInt8],119.to[UInt8],101.to[UInt8],174.to[UInt8],43.to[UInt8],89.to[UInt8],38.to[UInt8],109.to[UInt8],29.to[UInt8],131.to[UInt8],91.to[UInt8],134.to[UInt8],144.to[UInt8],131.to[UInt8])
    printArray(hashed_gold, "Expected: ")
    printArray(hashed_result, "Got: ")

    val cksum = hashed_gold.zip(hashed_result){_==_}.reduce{_&&_}
    println("PASS: " + cksum + " (BTC)")

  }
}

object SW extends SpatialApp { // Regression (Dense) // Args: tcgacgaaataggatgacagcacgttctcgtattagagggccgcggtacaaaccaaatgctgcggcgtacagggcacggggcgctgttcgggagatcgggggaatcgtggcgtgggtgattcgccggc ttcgagggcgcgtgtcgcggtccatcgacatgcccggtcggtgggacgtgggcgcctgatatagaggaatgcgattggaaggtcggacgggtcggcgagttgggcccggtgaatctgccatggtcgat
  override val target = AWS_F1


 /*
  
  Smith-Waterman Genetic Alignment algorithm                                                  
  
  This is just like SW algorithm, except negative scores are capped at 0, backwards traversal starts at highest score from any 
     element on the perimeter, and end when score is 0


    [SIC] SW diagram
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
    val row_par = 2 (1 -> 1 -> 8)

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


    val lb_par = 16 (1 -> 1 -> 16)
    val par_store = 16
    val row_stride = 10 (100 -> 100 -> 500)
    val row_par = 2 (1 -> 1 -> 16)
    val par_Kh = 3 (1 -> 1 -> 3)
    val par_Kw = 3 (1 -> 1 -> 3)

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

            lineOut(c) = mux(r.to[Index] + rr.to[Index] < 2.to[Index] || r.to[Index] + rr.to[Index] >= R-2, 0.to[T], abs(horz.value) + abs(vert.value))// Technically should be sqrt(horz**2 + vert**2)
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

