import spatial.dsl._
import org.virtualized._
import spatial.targets._

object Stencil3D extends SpatialApp { // Regression (Dense) // Args: none
  override val target = AWS_F1


 /*
                                                                                                                             
 H   ↗        ___________________                  ___________________                                                                  
  E         /                   /|               /000000000000000000/ |                                                                
   I       / ←    ROW      →   / |              /0  x  x  x  x    0/ 0|                        
 ↙  G     /__________________ /  |             /0________________0/  0|                                                                 
     H   |                   |   |            |0  X  X  X  X  X  0| x0|      
      T  |     ___           |   |            |0                 0|  0|      
         |    /__/|          |   |            |0   VALID DATA    0|  0|    *This app frames all borders with original value  
   ↑     |  ↑|   ||          |   |            |0  X  X  X  X  X  0| x0|      
         |  3|   || ----->   |   |   --->     |0                 0|  0|        
  COL    |  ↓|___|/          |   |            |0  X  X  X  X  X  0| x0|      
         |                   |   |            |0                 0|  0|      
         |                   |   |            |0  X  X  X  X  X  0| x0|      
         |                   |  /             |0                 0| 0/      
   ↓     |                   | /              |0                 0|0/ 
         |                   |/               |0000000000000000000|/        
          ```````````````````                  ```````````````````      
                                                
                                                
 */


  @virtualize
  def main() = {

   	// Problem properties
   	val ROWS = 16 // Leading dim
   	val COLS = 32
    val HEIGHT = 32
    val loop_height = 1 (1 -> 1 -> 8)
    val loop_row = 1 (1 -> 1 -> 8)
    val loop_col = 2 (1 -> 1 -> 8)
    // val num_slices = ArgIn[Int]
    // setArg(num_slices, args(0).to[Int])
    val num_slices = HEIGHT
   	val filter_size = 3*3*3

   	// Setup data
   	val raw_data = loadCSV1D[Int]("/remote/regression/data/machsuite/stencil3d_data.csv", "\n")
   	val data = raw_data.reshape(HEIGHT, COLS, ROWS)

   	// Setup DRAMs
   	val data_dram = DRAM[Int](HEIGHT, COLS, ROWS)
   	val result_dram = DRAM[Int](HEIGHT, COLS, ROWS)

   	setMem(data_dram, data)

   	Accel {
      val filter = LUT[Int](3,3,3)(   0,  0,  0,
                                      0, -1,  0,
                                      0,  0,  0,

                                      0, -1,  0,
                                     -1,  6, -1,
                                      0, -1,  0,

                                      0,  0,  0,
                                      0, -1,  0,
                                      0,  0,  0)

      val result_sram = SRAM[Int](HEIGHT,COLS,ROWS)

      Foreach(num_slices by 1 par loop_height) { p => 
        val temp_slice = SRAM[Int](COLS,ROWS)
        MemReduce(temp_slice)(-1 until 2 by 1) { slice => 
          val local_slice = SRAM[Int](COLS,ROWS)
          val lb = LineBuffer[Int](3,ROWS)
          Foreach(COLS+1 by 1 par loop_col){ i => 
            lb load data_dram((p+slice)%HEIGHT, i, 0::ROWS)
            Foreach(ROWS+1 by 1 par loop_row) {j => 
              val sr = RegFile[Int](3,3)
              Foreach(3 by 1 par 3) {k => sr(k,*) <<= lb(k,j%ROWS)}
              val temp = Reduce(Reg[Int](0))(3 by 1, 3 by 1){(r,c) => sr(r,c) * filter(slice+1,r,c)}{_+_}
              // For final version, make wr_value a Mux1H instead of a unique writer per val
              if (i == 0 || j == 0) {Pipe{}/*do nothing*/}
              else if (i == 1 || i == COLS || j == 1 || j == ROWS) {
                Pipe{
                  if (slice == 0) {local_slice(i-1, j-1) = sr(1,1)} // If on boundary of page, use meat only
                  else {local_slice(i-1, j-1) = 0} // If on boundary of page, ignore bread
                }
              }
              else if (slice == 0 && (p == 0 || p == HEIGHT-1)) {local_slice(i-1,j-1) = sr(1,1)} // First and last page, use meat only
              else if ((p == 0 || p == HEIGHT-1)) {local_slice(i-1,j-1) = 0} // First and last page, ignore bread
              else {local_slice(i-1, j-1) = temp} // Otherwise write convolution result
            }       
          }
          local_slice
        }{_+_}

        Foreach(COLS by 1, ROWS by 1){(i,j) => result_sram(p, i, j) = temp_slice(i,j)}

      }

      result_dram store result_sram


   	}

   	// Get results
   	val result_data = getTensor3(result_dram)
   	val raw_gold = loadCSV1D[Int]("/remote/regression/data/machsuite/stencil3d_gold.csv", "\n")
   	val gold = raw_gold.reshape(HEIGHT,COLS,ROWS)

   	// Printers
   	printTensor3(gold, "gold") // Least significant dimension is horizontal, second-least is vertical, third least is ---- separated blocks
   	printTensor3(result_data, "results")

   	val cksum = gold.zip(result_data){_==_}.reduce{_&&_}
   	println("PASS: " + cksum + " (Stencil3D)")

 }
}

object NW extends SpatialApp { // Regression (Dense) // Args: none
  override val target = AWS_F1


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
    val dash = argon.lang.String.char2num("-")
    val underscore = argon.lang.String.char2num("_")

    val SKIPB = 0
    val SKIPA = 1
    val ALIGN = 2
    val MATCH_SCORE = 1
    val MISMATCH_SCORE = -1
    val GAP_SCORE = -1 
    // val seqa_string = "tcgacgaaataggatgacagcacgttctcgtattagagggccgcggtacaaaccaaatgctgcggcgtacagggcacggggcgctgttcgggagatcgggggaatcgtggcgtgggtgattcgccggc".toText
    // val seqb_string = "ttcgagggcgcgtgtcgcggtccatcgacatgcccggtcggtgggacgtgggcgcctgatatagaggaatgcgattggaaggtcggacgggtcggcgagttgggcccggtgaatctgccatggtcgat".toText
    val seqa_string = "tcgacgaaataggatgacagcacgttctcgtattagagggccgcggtacaaaccaaatgctgcggcgtacagggcacggggcgctgttcgggagatcgggggaatcgtggcgtgggtgattcgccggc"
    val seqb_string = "ttcgagggcgcgtgtcgcggtccatcgacatgcccggtcggtgggacgtgggcgcctgatatagaggaatgcgattggaaggtcggacgggtcggcgagttgggcccggtgaatctgccatggtcgat"
    val length = 128

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
    val seqa_dram_aligned = DRAM[Int8](length*2)
    val seqb_dram_aligned = DRAM[Int8](length*2)
    setMem(seqa_dram_raw, seqa_bin)
    setMem(seqb_dram_raw, seqb_bin)

    Accel{
      val seqa_sram_raw = SRAM[Int8](length)
      val seqb_sram_raw = SRAM[Int8](length)
      val seqa_fifo_aligned = FIFO[Int8](length*2)
      val seqb_fifo_aligned = FIFO[Int8](length*2)

      seqa_sram_raw load seqa_dram_raw
      seqb_sram_raw load seqb_dram_raw

      val score_matrix = SRAM[nw_tuple](length+1,length+1)

      // Build score matrix
      Foreach(length+1 by 1){ r =>
        Sequential.Foreach(length+1 by 1) { c => // Bug #151, should be able to remove previous_result reg when fixed
          val previous_result = Reg[nw_tuple]
          val update = if (r == 0) (nw_tuple(-c.as[Int16], 0)) else if (c == 0) (nw_tuple(-r.as[Int16], 1)) else {
            val match_score = mux(seqa_sram_raw(c-1) == seqb_sram_raw(r-1), MATCH_SCORE.to[Int16], MISMATCH_SCORE.to[Int16])
            val from_top = score_matrix(r-1, c).score + GAP_SCORE
            val from_left = previous_result.score + GAP_SCORE
            val from_diag = score_matrix(r-1, c-1).score + match_score
            mux(from_left >= from_top && from_left >= from_diag, nw_tuple(from_left, SKIPB), mux(from_top >= from_diag, nw_tuple(from_top,SKIPA), nw_tuple(from_diag, ALIGN)))
          }
          previous_result := update
          score_matrix(r,c) = update
        }
      }

      // Read score matrix
      val b_addr = Reg[Int](length)
      val a_addr = Reg[Int](length)
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
        seqa_dram_aligned store seqa_fifo_aligned
        seqb_dram_aligned store seqb_fifo_aligned
      }

    }

    val seqa_aligned_result = getMem(seqa_dram_aligned)
    val seqb_aligned_result = getMem(seqb_dram_aligned)
    val seqa_aligned_string = argon.lang.String.num2string(seqa_aligned_result)
    val seqb_aligned_string = argon.lang.String.num2string(seqb_aligned_result)

    val seqa_gold_string = "cggccgcttag-tgggtgcggtgctaagggggctagagggcttg-tc-gcggggcacgggacatgcg--gcg-t--cgtaaaccaaacat-g-gcgccgggag-attatgctcttgcacg-acag-ta----g-gat-aaagc---agc-t_________________________________________________________________________________________________________".toText
    val seqb_gold_string = "--------tagct-ggtaccgt-ctaa-gtggc--ccggg-ttgagcggctgggca--gg-c-tg-gaag-gttagcgt-aaggagatatagtccg-cgggtgcagggtg-gctggcccgtacagctacctggcgctgtgcgcgggagctt_________________________________________________________________________________________________________".toText

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

    println("Result A: " + seqa_aligned_string)
    println("Gold A:   " + seqa_gold_string)
    println("Result B: " + seqb_aligned_string)
    println("Gold B:   " + seqb_gold_string)

    val cksumA = seqa_aligned_string == seqa_gold_string //seqa_aligned_result.zip(seqa_gold_bin){_==_}.reduce{_&&_}
    val cksumB = seqb_aligned_string == seqb_gold_string //seqb_aligned_result.zip(seqb_gold_bin){_==_}.reduce{_&&_}
    val cksum = cksumA && cksumB
    println("PASS: " + cksum + " (NW)")



  }
}      

object EdgeDetector extends SpatialApp { // Regression (Dense) // Args: none

  type T = FixPt[TRUE,_16,_16]

  @virtualize
  def main() {
    type T = FixPt[TRUE,_16,_16]
    val rowtile = 16
    val coltile = 64
    val data = loadCSV2D[T]("/remote/regression/data/slacsample2d.csv", ",", "\n")
    val memrows = ArgIn[Int]
    val memcols = ArgIn[Int]
    setArg(memrows, data.rows.to[Int])
    setArg(memcols, data.cols.to[Int])
    val srcmem = DRAM[T](memrows, memcols)
    setMem(srcmem, data)
    val risingEdges = DRAM[Int](memrows)
    // val fallingEdges = DRAM[Int](memrows)

    val window = 16

    Accel {
      val sr = RegFile[T](1,window)
      val rawdata = SRAM[T](coltile)
      val results = SRAM[Int](rowtile)
      // Work on each row
      Sequential.Foreach(memrows by rowtile) { r => 
        Sequential.Foreach(rowtile by 1) { rr => 
          // Work on each tile of a row
          val globalMax = Reduce(Reg[Tuple2[Int,T]](pack(0.to[Int], -1000.to[T])))(memcols by coltile) { c =>
            // Load tile from row
            rawdata load srcmem(r + rr, c::c+coltile)
            // Scan through tile to get deriv
            val localMax = Reduce(Reg[Tuple2[Int,T]](pack(0.to[Int], -1000.to[T])))(coltile by 1) { j =>
              sr(0,*) <<= rawdata(j)
              val mean_right = Reduce(Reg[T](0.to[T]))(window/2 by 1) { k => sr(0,k) }{_+_} / window.to[T]
              val mean_left = Reduce(Reg[T](0.to[T]))(window/2 by 1) { k => sr(0,k+window/2) }{_+_} / window.to[T]
              val slope = (mean_right - mean_left) / (window/2).to[T]
              val idx = j + c
              mux(idx < window, pack(idx, 0.to[T]), pack(idx,slope))
            }{(a,b) => mux(a._2 > b._2, a, b)}
            localMax
          }{(a,b) => mux(a._2 > b._2, a, b)}
          results(rr) = globalMax._1
        }
        risingEdges(r::r+rowtile) store results
      }
    }


    // Extract results from accelerator
    val results = getMem(risingEdges)
    val gold = loadCSV1D[Int]("/remote/regression/data/edge_gold.csv", ",")
    val margin = 2.to[Int]

    // Create validation checks and debug code
    printArray(results, "Results:")

    val cksum = results.zip(gold) {case (a,b) => a < b + margin && a > b - margin}.reduce{_&&_}
    println("PASS: " + cksum + " (EdgeDetector)")
  }
}

object MD_Grid extends SpatialApp { // Regression (Dense) // Args: none
  override val target = AWS_F1


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
  type T = FixPt[TRUE, _12, _52]
  @struct case class XYZ(x: T, y: T, z: T) 

  @virtualize
  def main() = {

    val N_ATOMS = 256
    val DOMAIN_EDGE = 20
    val BLOCK_SIDE = 4
    val density = 10
    val lj1 = 1.5.to[T]
    val lj2 = 2.to[T]

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
    val force_x_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
    val force_y_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
    val force_z_dram = DRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
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
      val force_x_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
      val force_y_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)
      val force_z_sram = SRAM[T](BLOCK_SIDE,BLOCK_SIDE,BLOCK_SIDE,density)

      dvec_x_sram load dvec_x_dram
      dvec_y_sram load dvec_y_dram
      dvec_z_sram load dvec_z_dram
      npoints_sram load npoints_dram

      // Iterate over each block
      Foreach(BLOCK_SIDE by 1, BLOCK_SIDE by 1, BLOCK_SIDE by 1) { (b0x, b0y, b0z) => 
        // Iterate over each point in this block, considering boundaries
        val b0_cube_forces = SRAM[XYZ](density)
        val b1x_start = max(0.to[Int],b0x-1.to[Int])
        val b1x_end = min(BLOCK_SIDE.to[Int], b0x+2.to[Int])
        val b1y_start = max(0.to[Int],b0y-1.to[Int])
        val b1y_end = min(BLOCK_SIDE.to[Int], b0y+2.to[Int])
        val b1z_start = max(0.to[Int],b0z-1.to[Int])
        val b1z_end = min(BLOCK_SIDE.to[Int], b0z+2.to[Int])
        MemReduce(b0_cube_forces)(b1x_start until b1x_end by 1, b1y_start until b1y_end by 1, b1z_start until b1z_end by 1) { (b1x, b1y, b1z) => 
          val b1_cube_contributions = SRAM[XYZ](density)
          // Iterate over points in b0
          val p_range = npoints_sram(b0x, b0y, b0z)
          val q_range = npoints_sram(b1x, b1y, b1z)
          Foreach(0 until p_range) { p_idx =>
            val px = dvec_x_sram(b0x, b0y, b0z, p_idx)
            val py = dvec_y_sram(b0x, b0y, b0z, p_idx)
            val pz = dvec_z_sram(b0x, b0y, b0z, p_idx)
            val q_sum = Reg[XYZ](XYZ(0.to[T], 0.to[T], 0.to[T]))
            Reduce(q_sum)(0 until q_range) { q_idx => 
              val qx = dvec_x_sram(b1x, b1y, b1z, q_idx)
              val qy = dvec_y_sram(b1x, b1y, b1z, q_idx)
              val qz = dvec_z_sram(b1x, b1y, b1z, q_idx)
              if ( !(b0x == b1x && b0y == b1y && b0z == b1z && p_idx == q_idx) ) { // Skip self
                val delta = XYZ(px - qx, py - qy, pz - qz)
                val r2inv = 1.0.to[T]/( delta.x*delta.x + delta.y*delta.y + delta.z*delta.z );
                // Assume no cutoff and aways account for all nodes in area
                val r6inv = r2inv * r2inv * r2inv;
                val potential = r6inv*(lj1*r6inv - lj2);
                val force = r2inv*potential;
                XYZ(delta.x*force, delta.y*force, delta.z*force)
              } else {
                XYZ(0.to[T], 0.to[T], 0.to[T])
              }
            }{(a,b) => XYZ(a.x + b.x, a.y + b.y, a.z + b.z)}
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
      }
      force_x_dram store force_x_sram
      force_y_dram store force_y_sram
      force_z_dram store force_z_sram

    }

    val force_x_received = getTensor4(force_x_dram)
    val force_y_received = getTensor4(force_y_dram)
    val force_z_received = getTensor4(force_z_dram)
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
    val cksum = cksumx && cksumy && cksumz
    println("PASS: " + cksum + " (MD_Grid)")
  }
}      

object FFT_Strided extends SpatialApp { // Regression (Dense) // Args: none
  override val target = AWS_F1


 /*                                                                                                  

   NOTES: This version uses one extra loop than the machsuite implementation because they mutate their counter that holds "odd" inside of the loop,
          so we can either use an FSM or use strict loops that take this mutation into account and I chose the latter
 */

  type T = FixPt[TRUE,_32,_32]
  @virtualize
  def main() = {

    val FFT_SIZE = 1024
    val numiter = (scala.math.log(FFT_SIZE) / scala.math.log(2)).to[Int]

    val data_real = loadCSV1D[T]("/remote/regression/data/machsuite/fft_strided_real.csv", "\n")
    val data_img = loadCSV1D[T]("/remote/regression/data/machsuite/fft_strided_img.csv", "\n")
    val data_twid_real = loadCSV1D[T]("/remote/regression/data/machsuite/fft_strided_twidreal.csv", "\n")
    val data_twid_img = loadCSV1D[T]("/remote/regression/data/machsuite/fft_strided_twidimg.csv", "\n")

    val data_real_dram = DRAM[T](FFT_SIZE)
    val data_img_dram = DRAM[T](FFT_SIZE)
    val data_twid_real_dram = DRAM[T](FFT_SIZE/2)
    val data_twid_img_dram = DRAM[T](FFT_SIZE/2)
    val result_real_dram = DRAM[T](FFT_SIZE)
    val result_img_dram = DRAM[T](FFT_SIZE)

    setMem(data_real_dram, data_real)
    setMem(data_img_dram, data_img)
    setMem(data_twid_real_dram, data_twid_real)
    setMem(data_twid_img_dram, data_twid_img)

    Accel{
      val data_real_sram = SRAM[T](FFT_SIZE)
      val data_img_sram = SRAM[T](FFT_SIZE)
      val data_twid_real_sram = SRAM[T](FFT_SIZE/2)
      val data_twid_img_sram = SRAM[T](FFT_SIZE/2)

      data_real_sram load data_real_dram
      data_img_sram load data_img_dram
      data_twid_real_sram load data_twid_real_dram
      data_twid_img_sram load data_twid_img_dram

      val span = Reg[Int](FFT_SIZE)
      Foreach(0 until numiter) { log => 
        span := span >> 1
        val num_sections = Reduce(Reg[Int](1))(0 until log){i => 2}{_*_}
        Foreach(0 until num_sections) { section => 
          val base = span*(2*section+1)
          Sequential.Foreach(0 until span by 1) { offset => 
            val odd = base + offset
            val even = odd ^ span

            val rtemp = data_real_sram(even) + data_real_sram(odd)
            Pipe{data_real_sram(odd) = data_real_sram(even) - data_real_sram(odd)}
            Pipe{data_real_sram(even) = rtemp}

            val itemp = data_img_sram(even) + data_img_sram(odd)
            Pipe{data_img_sram(odd) = data_img_sram(even) - data_img_sram(odd)}
            Pipe{data_img_sram(even) = itemp}
            
            val rootindex = (Reduce(Reg[Int](1))(0 until log){i => 2.to[Int]}{_*_} * even) & (FFT_SIZE - 1).to[Int]
            if (rootindex > 0.to[Int]) {
              // println("Accessing " + rootindex + " at " + even )
              val temp = data_twid_real_sram(rootindex) * data_real_sram(odd) - data_twid_img_sram(rootindex) * data_img_sram(odd)
              data_img_sram(odd) = data_twid_real_sram(rootindex) * data_img_sram(odd) + data_twid_img_sram(rootindex) * data_real_sram(odd)
              data_real_sram(odd) = temp
            }
          }
        }
      }
      result_real_dram store data_real_sram
      result_img_dram store data_img_sram
    }

    val result_real = getMem(result_real_dram)
    val result_img = getMem(result_img_dram)
    val gold_real = loadCSV1D[T]("/remote/regression/data/machsuite/fft_strided_real_gold.csv", "\n")
    val gold_img = loadCSV1D[T]("/remote/regression/data/machsuite/fft_strided_img_gold.csv", "\n")

    printArray(gold_real, "Gold real: ")
    printArray(result_real, "Result real: ")
    printArray(gold_img, "Gold img: ")
    printArray(result_img, "Result img: ")

    val margin = 0.01.to[T]
    val cksumR = gold_real.zip(result_real){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    val cksumI = gold_img.zip(result_img){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    val cksum = cksumR && cksumI
    println("PASS: " + cksum + " (FFT_Strided)")

  }
}

object Viterbi extends SpatialApp { // Regression (Dense) // Args: none
  override val target = AWS_F1


  /*

                    ←       N_OBS            →

          State 63 ----- State 63 ----- State 63                
        /  .        \ /            \ /                     P(obs | state) = emission_probs   
       /   .         X              X                                         (N_STATES x N_TOKENS)
      /    .        / \            / \                        
     O----State k  ----- State k  ----- State k  ...            
      \    .        \ /            \ /                          
       \   .         X              X                          shortest path to (state, obs) = llike
        \  .        / \            / \                                                          (N_OBS x N_STATES)
          State 0  ----- State 0  ----- State 0                  
      ↑               ↑                                                           
    init_states     transition_probs                        
     (N_STATES)       (N_STATES x N_STATES)

            obs_vec
             (N_OBS, max value = N_TOKENS)


  TODO: Eliminate backprop step and do everything feed-forward
  MachSuite Concerns:
    - Constructing path step by step seems to give the wrong result because they do extra math in the backprop step. 
           Why do you need to do math when going backwards? I thought you just read off the result

  */

  type T = FixPt[TRUE,_16,_16]

  @virtualize
  def main() = {
    // Setup dimensions of problem
    val N_STATES = 64
    val N_TOKENS = 64
    val N_OBS = 140

    // debugging
    val steps_to_take = N_OBS //ArgIn[Int] //
    // setArg(steps_to_take, args(0).to[Int])

    // Setup data
    val init_states = Array[T](4.6977033615112305.to[T],3.6915655136108398.to[T],4.8652229309082031.to[T],4.7658410072326660.to[T],
      4.0006790161132812.to[T],3.9517300128936768.to[T],3.4640796184539795.to[T],3.4600069522857666.to[T],4.2856273651123047.to[T],
      3.6522088050842285.to[T],4.8189344406127930.to[T],3.8075556755065918.to[T],3.8743767738342285.to[T],5.4135279655456543.to[T],
      4.9173111915588379.to[T],3.6458325386047363.to[T],5.8528852462768555.to[T],11.3210048675537109.to[T],4.9971127510070801.to[T],
      5.1006979942321777.to[T],3.5980830192565918.to[T],5.3161897659301758.to[T],3.4544019699096680.to[T],3.7314746379852295.to[T],
      4.9998908042907715.to[T],3.4898567199707031.to[T],4.2091164588928223.to[T],3.5122559070587158.to[T],3.9326364994049072.to[T],
      7.2767667770385742.to[T],3.6539671421051025.to[T],4.0916681289672852.to[T],3.5044839382171631.to[T],4.5234117507934570.to[T],
      3.7673256397247314.to[T],4.0265331268310547.to[T],3.7147023677825928.to[T],6.7589721679687500.to[T],3.5749390125274658.to[T],
      3.7701597213745117.to[T],3.5728175640106201.to[T],5.0258340835571289.to[T],4.9390106201171875.to[T],5.7208223342895508.to[T],
      6.3652114868164062.to[T],3.5838112831115723.to[T],5.0102572441101074.to[T],4.0017414093017578.to[T],4.2373661994934082.to[T],
      3.8841004371643066.to[T],5.3679313659667969.to[T],3.9980680942535400.to[T],3.5181968212127686.to[T],4.7306714057922363.to[T],
      5.5075111389160156.to[T],5.1880970001220703.to[T],4.8259010314941406.to[T],4.2589011192321777.to[T],5.6381106376647949.to[T],
      3.4522385597229004.to[T],3.5920252799987793.to[T],4.2071061134338379.to[T],5.0856294631958008.to[T],6.0637059211730957.to[T])

    val obs_vec = Array[Int](0,27,49,52,20,31,63,63,29,0,47,4,38,38,38,38,4,43,7,28,31,
                         7,7,7,57,2,2,43,52,52,43,3,43,13,54,44,51,32,9,9,15,45,21,
                         33,61,45,62,0,55,15,55,30,13,13,53,13,13,50,57,57,34,26,21,
                         43,7,12,41,41,41,17,17,30,41,8,58,58,58,31,52,54,54,54,54,
                         54,54,15,54,54,54,54,52,56,52,21,21,21,28,18,18,15,40,1,62,
                         40,6,46,24,47,2,2,53,41,0,55,38,5,57,57,57,57,14,57,34,37,
                         57,30,30,5,1,5,62,25,59,5,2,43,30,26,38,38)

    val raw_transitions = loadCSV1D[T]("/remote/regression/data/machsuite/viterbi_transition.csv", "\n")
    val raw_emissions = loadCSV1D[T]("/remote/regression/data/machsuite/viterbi_emission.csv", "\n")
    val transitions = raw_transitions.reshape(N_STATES, N_STATES)
    val emissions = raw_emissions.reshape(N_STATES, N_TOKENS)

    val correct_path = Array[Int](27,27,27,27,27,31,63,63,63,63,47,4,38,38,38,38,7,7,7,
                                  7,7,7,7,7,2,2,2,43,52,52,43,43,43,43,43,44,44,32,9,9,
                                  15,45,45,45,45,45,45,0,55,55,55,30,13,13,13,13,13,13,
                                  57,57,21,21,21,21,7,41,41,41,41,17,17,30,41,41,58,58,
                                  58,31,54,54,54,54,54,54,54,54,54,54,54,54,52,52,52,21,
                                  21,21,28,18,18,40,40,40,40,40,40,46,46,2,2,2,53,53,53,
                                  55,38,57,57,57,57,57,57,57,57,57,57,30,30,5,5,5,5,5,5,
                                  5,5,30,30,26,38,38)
    // Handle DRAMs
    val init_dram = DRAM[T](N_STATES)
    val obs_dram = DRAM[Int](N_OBS)
    val transitions_dram = DRAM[T](N_STATES,N_STATES)
    val emissions_dram = DRAM[T](N_STATES,N_TOKENS)
    // val llike_dram = DRAM[T](N_OBS,N_STATES)
    val path_dram = DRAM[Int](N_OBS)
    setMem(init_dram,init_states)
    setMem(obs_dram, obs_vec)
    setMem(transitions_dram,transitions)
    setMem(emissions_dram,emissions)


    Accel{
      // Load data structures
      val obs_sram = SRAM[Int](N_OBS)
      val init_sram = SRAM[T](N_STATES)
      val transitions_sram = SRAM[T](N_STATES,N_STATES)
      val emissions_sram = SRAM[T](N_STATES,N_TOKENS)
      val llike_sram = SRAM[T](N_OBS, N_STATES)
      val path_sram = SRAM[Int](N_OBS)

      Parallel{
        obs_sram load obs_dram
        init_sram load init_dram
        transitions_sram load transitions_dram
        emissions_sram load emissions_dram
      }

      // from --> to
      Sequential.Foreach(0 until steps_to_take) { step => 
        val obs = obs_sram(step)
        Sequential.Foreach(0 until N_STATES) { to => 
          val emission = emissions_sram(to, obs)
          val best_hop = Reg[T](0x4000)
          best_hop.reset
          Reduce(best_hop)(0 until N_STATES) { from => 
            val base = llike_sram((step-1) % N_OBS, from) + transitions_sram(from,to)
            base + emission
          } { (a,b) => mux(a < b, a, b)}
          llike_sram(step,to) = mux(step == 0, emission + init_sram(to), best_hop)
        }
      }

      // to <-- from
      Sequential.Foreach(steps_to_take-1 until -1 by -1) { step => 
        val from = path_sram(step+1)
        val min_pack = Reg[Tup2[Int, T]](pack(-1.to[Int], (0x4000).to[T]))
        min_pack.reset
        Reduce(min_pack)(0 until N_STATES){ to => 
          val jump_cost = mux(step == steps_to_take-1, 0.to[T], transitions_sram(to, from))
          val p = llike_sram(step,to) + jump_cost
          pack(to,p)
        }{(a,b) => mux(a._2 < b._2, a, b)}
        path_sram(step) = min_pack._1
      }

      // Store results
      // llike_dram store llike_sram
      path_dram store path_sram
    }

    // Get data structures
    // val llike = getMatrix(llike_dram)
    val path = getMem(path_dram)

    // Print data structures
    // printMatrix(llike, "log-likelihood")
    printArray(path, "path taken")
    printArray(correct_path, "correct path")

    // Check results
    val cksum = correct_path.zip(path){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (Viterbi)")

  }
}

object Gibbs_Ising2D extends SpatialApp { // DISABLED Regression (Dense) // Args: 200 0.3 2
  type T = FixPt[TRUE,_32,_32]
  type PROB = FixPt[FALSE, _0, _16]
  @virtualize
  def main() = {

    val COLS = 64
    val ROWS = 32 
    val lut_size = 9
    val border = -1

    val I = args(0).to[Int] // Number of iterations to run
    val J = args(1).to[T] // Energy scalar for edge
    val J_b = args(2).to[T] // Energy scalar for external field

    // Args
    val iters = ArgIn[Int]
    val exp_negbias = ArgIn[T]
    val exp_posbias = ArgIn[T]

    // Set up lut for edge energy ratio
    // 𝚺 x_j * x_i can be from -4 to +4
    val exp_data = Array.tabulate[T](lut_size){i => 
      val x = i - 4
      exp(x.to[Float]*J.to[Float] * -2.to[Float]).to[T]
    }
    // Set up args for bias energy ratio
    val exp_neg = exp(-J_b.to[Float]*2.to[Float]).to[T]
    val exp_pos = exp(J_b.to[Float]*2.to[Float]).to[T]

    // Debugging
    printArray(exp_data, "exp data")
    println("neg: " + exp_neg)
    println("pos: " + exp_pos)

    // Set initial and bias patterns:
    // Checkerboard
    val grid_init = (0::ROWS, 0::COLS){(i,j) => if ((i+j)%2 == 0) -1.to[Int] else 1.to[Int]}
    // // Square
    // val grid_init = (0::ROWS, 0::COLS){(i,j) => if (i > ROWS/4 && i < 3*ROWS/4 && j > COLS/4 && j < 3*COLS/4) -1.to[Int] else 1.to[Int]}

    // Square
    val bias_matrix = (0::ROWS, 0::COLS){(i,j) => if (i > ROWS/4 && i < 3*ROWS/4 && j > COLS/4 && j < 3*COLS/4) -1.to[Int] else 1.to[Int]}

    val exp_lut = DRAM[T](lut_size)
    val grid_dram = DRAM[Int](ROWS,COLS)
    val bias_dram = DRAM[Int](ROWS,COLS)

    setMem(grid_dram, grid_init)
    setMem(bias_dram, bias_matrix)
    setMem(exp_lut, exp_data)
    setArg(exp_negbias, exp_neg)
    setArg(exp_posbias, exp_pos)
    setArg(iters, I)

    // Accel{
    //   val exp_sram = SRAM[T](lut_size)
    //   val grid_lb = LineBuffer[Int](2,COLS)
    //   val prev_line = SRAM[Int](COLS)
    //   val bias_line = SRAM[Int](COLS)
    //   val local_prev_line = SRAM[Int](COLS)
    //   exp_sram load exp_lut
    //   prev_line load grid_dram(ROWS-1, 0::COLS)
    //   local_prev_line load grid_dram(ROWS-1, 0::COLS)

    //   Foreach(iters by 1) { iter =>
    //     Foreach(ROWS+1 by 1) { i => 
    //       val active_row = (i-1)%ROWS
    //       // Fetch next row
    //       grid_lb load grid_dram(i%ROWS, 0::COLS)
    //       bias_line load bias_dram(active_row, 0::COLS)
    //       val window_sr = RegFile[Int](2,2)

    //       // Update each point in active row
    //       Sequential.Foreach(0 until COLS+1 by 1) { j => 
    //         // Shift wavefront into regfile for parallel access
    //         Sequential.Foreach(2 by 1 par 2) { k => window_sr(k, *) <<= grid_lb(k, j%COLS) }
    //         // Keep local copy of previous line that does not get rotated through the buffers
    //         // Collect neighbors, wrapping around edges
    //         val N = mux(i == 0, border.to[Int], local_prev_line((j-1)%COLS))
    //         val E = mux(j == COLS, border.to[Int], window_sr(1,0))
    //         val S = mux(i == ROWS, border.to[Int], window_sr(0,1))
    //         val W = mux(j == 0, border.to[Int], local_prev_line(j-2))
    //         val self = window_sr(1,1)
    //         val sum = (N+E+S+W)*self
    //         val p_flip = exp_sram(-sum+lut_size/2)
    //         val pi_x = exp_sram(sum+4) * mux((bias_line((j-1)%COLS) * self) < 0, exp_negbias, exp_posbias)
    //         val threshold = min(1.to[T], pi_x)
    //         val rng = unif[_16]()
    //         val flip = mux(pi_x > 1, true.to[Bool], mux(rng < threshold.as[PROB], true.to[Bool], false.to[Bool]))
    //         if (j > 0 && j < COLS) {
    //           prev_line(j) = mux(flip, -self, self)
    //         }
    //         if (j > 0) {
    //           local_prev_line(j) = mux(flip, -self, self)  
    //         }
    //       }
    //       // Write back line
    //       if (iter > 0 || i > 0) {grid_dram(active_row, 0::COLS) store prev_line}
    //     }
    //   }
    // }

    Accel{
      val exp_sram = SRAM[T](lut_size)
      val grid_sram = SRAM[Int](ROWS,COLS)
      val bias_sram = SRAM[Int](ROWS,COLS)
      exp_sram load exp_lut
      grid_sram load grid_dram
      bias_sram load bias_dram

      Foreach(iters by 1) { iter =>
        Sequential.Foreach(ROWS by 1) { i => 
          // Update each point in active row
          Sequential.Foreach(0 until COLS by 1) { j => 
            val N = grid_sram((i+1)%ROWS, j)
            val E = grid_sram(i, (j+1)%COLS)
            val S = grid_sram((i-1)%ROWS, j)
            val W = grid_sram(i, (j-1)%COLS)
            val self = grid_sram(i,j)
            val sum = (N+E+S+W)*self
            val p_flip = exp_sram(-sum+lut_size/2)
            val pi_x = exp_sram(sum+4) * mux((bias_sram(i,j) * self) < 0, exp_posbias, exp_negbias)
            val threshold = min(1.to[T], pi_x)
            val rng = unif[_16]()
            val flip = mux(pi_x > 1, 1.to[T], mux(rng < threshold(31::16).as[PROB], 1.to[T], 0.to[T]))
            grid_sram(i,j) = mux(flip == 1.to[T], -self, self)
          }
        }
      }
      grid_dram store grid_sram
    }

    val result = getMatrix(grid_dram)
    println("Ran for " + I + " iters.")
    // printMatrix(result, "Result matrix")

    print(" ")
    for( j <- 0 until COLS) { print("-")}
    for( i <- 0 until ROWS) {
      println("")
      print("|")
      for( j <- 0 until COLS) {
        if (result(i,j) == -1) {print("X")} else {print(" ")}
      }
      print("|")
    }
    println(""); print(" ")
    for( j <- 0 until COLS) { print("-")}
    println("")

    val blips_inside = (0::ROWS, 0::COLS){(i,j) => 
      if (i > ROWS/4 && i < 3*ROWS/4 && j > COLS/4 && j < 3*COLS/4) {
        if (result(i,j) != -1) 1 else 0
      } else { 0 }
    }.reduce{_+_}
    val blips_outside = (0::ROWS, 0::COLS){(i,j) => 
      if (i > ROWS/4 && i < 3*ROWS/4 && j > COLS/4 && j < 3*COLS/4) {
        0
      } else { 
        if (result(i,j) != 1) 1 else 0
      }
    }.reduce{_+_}
    println("Found " + blips_inside + " blips inside the bias region and " + blips_outside + " blips outside the bias region")
    val cksum = (blips_inside + blips_outside) < (ROWS*COLS/8)
    println("PASS: " + cksum + " (Gibbs_Ising2D)")

  }
}

object GEMM_Blocked extends SpatialApp { // Regression (Dense) // Args: none
  override val target = AWS_F1


 /*
                                                             
    CONCERNS: We need to figure out how HLS is actually managing the srams, or make our management better  
              We cannot do unaligned stores yet, so tilesize of 8 won't work unless we keep ts 16 of c_sram onchip                                                                                          
 */
  type T = FixPt[TRUE,_16,_16]

  @virtualize
  def main() = {

    val dim = 64
    val tileSize = 8

    val a_data = loadCSV1D[T]("/remote/regression/data/machsuite/gemm_a.csv", "\n").reshape(dim,dim)
    val b_data = loadCSV1D[T]("/remote/regression/data/machsuite/gemm_b.csv", "\n").reshape(dim,dim)
    val c_init = (0::dim, 0::dim){(i,j) => 0.to[T]}
    val a_dram = DRAM[T](dim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](dim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)
    setMem(c_dram, c_init)

    Accel{
      val a_sram = SRAM[T](tileSize)
      val b_sram = SRAM[T](tileSize,tileSize)
      val c_sram = SRAM[T](dim,dim) // No tiling along rows dim in machsuite??
      c_sram load c_dram

      Foreach(dim by tileSize) { jj => 
        Foreach(dim by tileSize) { kk =>
          b_sram load b_dram(kk::kk+tileSize, jj::jj+tileSize)
          Foreach(dim by 1) { i => 
            a_sram load a_dram(i, kk::kk+tileSize)
            Foreach(tileSize by 1) { k => 
              val temp_a = a_sram(k)
              Foreach(tileSize by 1) { j => 
                c_sram(i,j+jj) = c_sram(i,j+jj) + b_sram(k, j) * temp_a
              }
            }
          } 
        }
      }
      c_dram store c_sram
    }

    val c_gold = loadCSV1D[T]("/remote/regression/data/machsuite/gemm_gold.csv", "\n").reshape(dim,dim)
    val c_result = getMatrix(c_dram)

    printMatrix(c_gold, "C Gold: ")
    printMatrix(c_result, "C Result: ")

    val margin = 0.5.to[T]
    val cksum = c_gold.zip(c_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (GEMM_Blocked)")
  }
}

object SPMV_CRS extends SpatialApp { // Regression (Sparse) // Args: none
  override val target = AWS_F1


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

    val NNZ = 1666
    val N = 494
    val tileSize = 494

    val raw_values = loadCSV1D[T]("/remote/regression/data/machsuite/crs_values.csv", "\n")
    val raw_cols = loadCSV1D[Int]("/remote/regression/data/machsuite/crs_cols.csv", "\n")
    val raw_rowid = loadCSV1D[Int]("/remote/regression/data/machsuite/crs_rowid.csv", "\n")
    val raw_vec = loadCSV1D[T]("/remote/regression/data/machsuite/crs_vec.csv", "\n")

    val values_dram = DRAM[T](NNZ) 
    val cols_dram = DRAM[Int](NNZ) 
    val rowid_dram = DRAM[Int](N+1) 
    val vec_dram = DRAM[T](N) 
    val result_dram = DRAM[T](N)

    setMem(values_dram, raw_values)
    setMem(cols_dram, raw_cols)
    setMem(rowid_dram, raw_rowid)
    setMem(vec_dram, raw_vec)

    Accel {
      val rowid_sram = SRAM[Int](tileSize+1)
      val cols_sram = SRAM[Int](tileSize)
      val values_sram = SRAM[T](tileSize)
      val vec_sram = SRAM[T](tileSize)
      val result_sram = SRAM[T](tileSize)

      Foreach(N/tileSize by 1) { tile =>
        rowid_sram load rowid_dram(tile*(tileSize+1) :: (tile+1)*(tileSize+1))
        Foreach(tileSize by 1) { i => 
          val start_id = rowid_sram(i)
          val stop_id = rowid_sram(i+1)
          Parallel{
            cols_sram load cols_dram(start_id :: stop_id)
            values_sram load values_dram(start_id :: stop_id)
          }
          vec_sram gather vec_dram(cols_sram, stop_id - start_id)
          val element = Reduce(Reg[T](0))(stop_id - start_id by 1) { j => 
            values_sram(j) * vec_sram(j)
          }{_+_}
          result_sram(i) = element
        }
        result_dram(tile*tileSize :: (tile+1)*tileSize) store result_sram
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

object BFS_Queue extends SpatialApp { // Regression (Sparse) // Args: none
  override val target = AWS_F1


 /*                                                                                                  
          ________________
    Q:   |          x x x |
          `````````↑``````
                   * Grab numel before starting next horizon so we know how many to deq before we hit next frontier
             ___________________
    levels: |     0             |  starts as -1
             `````|`````````````
                  |
             _____🡓_____________
    nodes:  |                   |  contains start and end indices into edges
             `````|`````````````
                  |______
                       /  \
             _________↙_____🡖__________________________________________
    edges:  |                                                          |
             ``````````````````````````````````````````````````````````
        * Index into levels push them onto queue if they are unvisited

  CONCERNS: This isn't really sparse...

 */

  @virtualize
  def main() = {

    val SCALE = 8
    val EDGE_FACTOR = 16
    val N_NODES = 1 << SCALE
    val N_EDGES = N_NODES*EDGE_FACTOR
    val N_LEVELS = 10
    val unvisited = -1
    val start_id = 38

    val nodes_raw = loadCSV1D[Int]("/remote/regression/data/machsuite/bfs_nodes.csv", "\n")
    val edges_data = loadCSV1D[Int]("/remote/regression/data/machsuite/bfs_edges.csv", "\n")

    val node_starts_data = Array.tabulate[Int](N_NODES){i => nodes_raw(2*i)}
    val node_ends_data = Array.tabulate[Int](N_NODES){i => nodes_raw(2*i+1)}
    val node_starts_dram = DRAM[Int](N_NODES)
    val node_ends_dram = DRAM[Int](N_NODES)
    val edges_dram = DRAM[Int](N_EDGES)
    val widths_dram = DRAM[Int](16)

    setMem(node_starts_dram, node_starts_data)
    setMem(node_ends_dram, node_ends_data)
    setMem(edges_dram, edges_data)

    Accel{
      val node_starts_sram = SRAM[Int](N_NODES)
      val node_ends_sram = SRAM[Int](N_NODES)
      val levels_sram = SRAM[Int](N_NODES)
      val edges_sram = SRAM[Int](N_NODES) // bigger than necessary
      val Q = FIFO[Int](N_NODES)
      val widths_sram = SRAM[Int](16)

      node_starts_sram load node_starts_dram
      node_ends_sram load node_ends_dram

      Foreach(N_NODES by 1){ i => levels_sram(i) = unvisited }
      Pipe{levels_sram(start_id) = 0}
      Foreach(16 by 1) {i => widths_sram(i) = if ( i == 0) 1 else 0}
      Q.enq(start_id)

      FSM[Int,Int](0)( horizon => horizon < N_LEVELS ) { horizon => 
        val level_size = Q.numel
        Sequential.Foreach(level_size by 1) { i => 
          val n = Q.deq()
          val start = node_starts_sram(n)
          val end = node_ends_sram(n)
          val length = end - start
          edges_sram load edges_dram(start::end)
          Sequential.Foreach(length by 1) { e =>
            val tmp_dst = edges_sram(e)
            val dst_level = levels_sram(tmp_dst)
            if (dst_level == unvisited) { Q.enq(tmp_dst) }
            if (dst_level == unvisited) { levels_sram(tmp_dst) = horizon+1 }
          }
        }
        widths_sram(horizon+1) = Q.numel
      }{ horizon => mux(Q.numel == 0.to[Int], N_LEVELS+1, horizon+1) }

      widths_dram store widths_sram

    }

    val widths_gold = Array[Int](1,26,184,22,0,0,0,0,0,0,0,0,0,0,0,0)
    val widths_result = getMem(widths_dram)

    printArray(widths_gold, "Gold: ")
    printArray(widths_result, "Received: ")

    val cksum = widths_gold.zip(widths_result){_==_}.reduce{_&&_}
    println("PASS: " + cksum + " (BFS_Queue)")

  }
}

object PageRank extends SpatialApp { // DISABLED Regression (Sparse) // Args: 1 768 0.125

  type Elem = FixPt[TRUE,_16,_16] // Float
  type X = FixPt[TRUE,_16,_16] // Float

  /*
                                          0
         _________________________________|__________________________________________________________
        |                   |                 |                  |                 |                 |
        1                   3                 5                  7                 9                 11
     ___|______        _____|____         ____|__          ______|______           |        _________|____________
    |     |    |      |     |    |       |       |        |      |      |          |       |       |        |     |
    2     50   55     4     92   49      150     6        8      10     12        42      110     210      310   311
   _|_    _|_   |    _|_    |   _|_      |     __|_       |      |      |         |        |       |      _|_     |
  |   |  |   |  |   |   |   |  |   |     |    |    |      |      |      |         |        |       |     |   |    |
  57 100 58 101 140 60 102  99 120 115   13  103  104    105    106    108        43      111     211   300  301  290
                    |
              ______|________
             |   |   |   |   |
             80 131 132 181 235



  */
  val edges_per_page = 6 // Will make this random later
  val margin = 1

  @virtualize
  def pagerank[T:Type:Num](
    pagesIN:  Array[T],
    edgesIN:  Array[Int],
    countsIN: Array[Int],
    edgeIdIN: Array[Int],
    edgeLenIN: Array[Int],
    itersIN: Int,
    dampIN: T,
    np: Int
  ) = {

    val NE = 9216
    val tileSize = 16 // For now
    val iters = ArgIn[Int]
    val NP    = ArgIn[Int]
    val damp  = ArgIn[T]
    setArg(iters, itersIN)
    setArg(NP, np)
    setArg(damp, dampIN)

    val OCpages    = DRAM[T](NP)
    val OCedges    = DRAM[Int](NE)    // srcs of edges
    val OCcounts   = DRAM[Int](NE)    // counts for each edge
    val OCedgeId   = DRAM[Int](NP) // Start index of edges
    val OCedgeLen  = DRAM[Int](NP) // Number of edges for each page
    // val OCresult   = DRAM[T](np)

    setMem(OCpages, pagesIN)
    setMem(OCedges, edgesIN)
    setMem(OCcounts, countsIN)
    setMem(OCedgeId, edgeIdIN)
    setMem(OCedgeLen, edgeLenIN)

    Accel {
      val frontierOff = SRAM[Int](tileSize)
      val currentPR = SRAM[T](tileSize)
      // Flush frontierOff so we don't cause gather segfault. Flush currentPR because mux isn't do what I thought it would
      Foreach(tileSize by 1) { i => frontierOff(i) = 0.to[Int]}

      Sequential.Foreach(iters by 1){ iter =>
        // val oldPrIdx = iter % 2.as[SInt]
        // val newPrIdx = mux(oldPrIdx == 1, 0.as[SInt], 1.as[SInt])
        Sequential.Foreach(NP by tileSize) { tid =>
          val edgesId = SRAM[Int](tileSize)
          val edgesLen = SRAM[Int](tileSize)
          Parallel {
            currentPR load OCpages(tid::tid+tileSize)
            edgesId load OCedgeId(tid :: tid+tileSize)
            edgesLen load OCedgeLen(tid :: tid+tileSize)
          }

          Sequential.Foreach(tileSize by 1) { pid =>
            val startId = edgesId(pid)
            val numEdges = Reg[Int](0)
            Pipe{ numEdges := edgesLen(pid) }


            // Gather edges indices and counts
            val edges = SRAM[Int](tileSize)
            val counts = SRAM[Int](tileSize)
            Parallel {
              edges load OCedges(startId :: startId + numEdges.value)
              counts load OCcounts(startId :: startId + numEdges.value)
            }

            // Triage edges based on if they are in current tile or offchip
            val offLoc = SRAM[Int](tileSize)
            val onChipMask = SRAM[Int](tileSize) // Really bitmask
            val offAddr = Reg[Int](-1)
            Sequential.Foreach(numEdges.value by 1){ i =>
              val addr = edges(i) // Write addr to both tiles, but only inc one addr
              val onchip = addr >= tid && addr < tid+tileSize
              offAddr := offAddr.value + mux(onchip, 0, 1)
              offLoc(i) = mux(onchip, offAddr.value, (tileSize-1).to[Int]) // Probably no need to mux here
              onChipMask(i) = mux(onchip, 1.to[Int], 0.to[Int])
            }

            // Set up gather addresses
            Sequential.Foreach(numEdges.value by 1){i =>
              frontierOff(offLoc(i)) = edges(i)
            }

            // Gather offchip ranks
            val gatheredPR = SRAM[T](tileSize)
            val num2gather = max(offAddr.value + 1.to[Int], 0.to[Int]) // Probably no need for mux
            gatheredPR gather OCpages(frontierOff, num2gather)

            // Compute new PR
            val pr = Reduce(Reg[T])(numEdges.value by 1){ i =>
              val addr = edges(i)
              val off  = offLoc(i)
              val mask = onChipMask(i)
              val onchipRank = currentPR(addr - tid)

              val offchipRank = gatheredPR(off)

              val rank = mux(mask == 1.to[Int], onchipRank, offchipRank)

              rank / (counts(i).to[T])
            }{_+_}
            //val pr = Reduce(numEdges.value by 1)(0.as[T]){ i => frontier(i) / counts(i).to[T] }{_+_}

            // Update PR
            currentPR(pid) = pr.value * damp + (1.to[T] - damp)

            // Reset counts (Plasticine: assume this is done by CUs)
            Pipe{offAddr := -1}

          }
          OCpages(tid::tid+tileSize) store currentPR
        }
      }
    }
    getMem(OCpages)
  }

  @virtualize
  def main() {
    val iters = args(0).to[Int]
    val NP = args(1).to[Int]
    val damp = args(2).to[X]
    val NE = 18432

    val pages = Array.tabulate(NP){i => 4.to[X]}
    val edges = Array.tabulate(NP){i => Array.tabulate(edges_per_page) {j => if (i < edges_per_page) j else i - j}}.flatten
    val counts = Array.tabulate(NP){i => Array.tabulate(edges_per_page) { j => edges_per_page.to[Int] }}.flatten
    val edgeId = Array.tabulate(NP){i => i*edges_per_page }
    val edgeLen = Array.tabulate(NP){i => edges_per_page.to[Int] }

    val result = pagerank(pages, edges, counts, edgeId, edgeLen, iters, damp, NP)


    val gold = Array.empty[X](NP)
    // Init
    for (i <- 0 until NP) {
      gold(i) = pages(i)
    }

    // Really bad imperative version
    for (ep <- 0 until iters) {
      for (i <- 0 until NP) {
        val numEdges = edgeLen(i)
        val startId = edgeId(i)
        val iterator = Array.tabulate(numEdges){kk => startId + kk}
        val these_edges = iterator.map{j => edges(j)}
        val these_pages = these_edges.map{j => gold(j)}
        val these_counts = these_edges.map{j => counts(j)}
        val pr = these_pages.zip(these_counts){ (p,c) =>
          // println("page " + i + " doing " + p + " / " + c)
          p/c.to[X]
        }.reduce{_+_}
        // println("new pr for " + i + " is " + pr)
        gold(i) = pr*damp + (1.to[X]-damp)
      }
    }

    printArray(gold, "gold: ")
    printArray(result, "result: ")
    val cksum = result.zip(gold){ case (o, g) => (g < (o + margin)) && g > (o - margin)}.reduce{_&&_}
    println("PASS: " + cksum + " (PageRank)")
  }
}

object BlackScholes extends SpatialApp {


  val margin = 0.5f // Validates true if within +/- margin
  val innerPar = 16
  val outerPar = 1
  val tileSize = 2000

  final val inv_sqrt_2xPI = 0.39894228040143270286f

  @virtualize
  def CNDF(x: Float) = {
    val ax = abs(x)

    val xNPrimeofX = exp((ax ** 2) * -0.05f) * inv_sqrt_2xPI
    val xK2 = 1.to[Float] / ((ax * 0.2316419f) + 1.0f)

    val xK2_2 = xK2 ** 2
    val xK2_3 = xK2_2 * xK2
    val xK2_4 = xK2_3 * xK2
    val xK2_5 = xK2_4 * xK2

    val xLocal_10 = xK2 * 0.319381530f
    val xLocal_20 = xK2_2 * -0.356563782f
    val xLocal_30 = xK2_3 * 1.781477937f
    val xLocal_31 = xK2_4 * -1.821255978f
    val xLocal_32 = xK2_5 * 1.330274429f

    val xLocal_21 = xLocal_20 + xLocal_30
    val xLocal_22 = xLocal_21 + xLocal_31
    val xLocal_23 = xLocal_22 + xLocal_32
    val xLocal_1 = xLocal_23 + xLocal_10

    val xLocal0 = xLocal_1 * xNPrimeofX
    val xLocal  = -xLocal0 + 1.0f

    mux(x < 0.0f, xLocal0, xLocal)
  }

  @virtualize
  def BlkSchlsEqEuroNoDiv(sptprice: Float, strike: Float, rate: Float,
    volatility: Float, time: Float, otype: Int) = {

    val xLogTerm = log( sptprice / strike )
    val xPowerTerm = (volatility ** 2) * 0.5f
    val xNum = (rate + xPowerTerm) * time + xLogTerm
    val xDen = volatility * sqrt(time)

    val xDiv = xNum / (xDen ** 2)
    val nofXd1 = CNDF(xDiv)
    val nofXd2 = CNDF(xDiv - xDen)

    val futureValueX = strike * exp(-rate * time)

    val negNofXd1 = -nofXd1 + 1.0f
    val negNofXd2 = -nofXd2 + 1.0f

    val optionPrice1 = (sptprice * nofXd1) - (futureValueX * nofXd2)
    val optionPrice2 = (futureValueX * negNofXd2) - (sptprice * negNofXd1)
    mux(otype == 0, optionPrice2, optionPrice1)
  }

  @virtualize
  def blackscholes(
    stypes:      Array[Int],
    sprices:     Array[Float],
    sstrike:     Array[Float],
    srate:       Array[Float],
    svolatility: Array[Float],
    stimes:      Array[Float]
  ): Array[Float] = {
    val B  = tileSize (96 -> 96 -> 19200)
    val OP = outerPar (1 -> 2)
    val IP = innerPar (1 -> 96)

    val size = stypes.length; bound(size) = 9995328

    val N = ArgIn[Int]
    setArg(N, size)

    val types    = DRAM[Int](N)
    val prices   = DRAM[Float](N)
    val strike   = DRAM[Float](N)
    val rate     = DRAM[Float](N)
    val vol      = DRAM[Float](N)
    val times    = DRAM[Float](N)
    val optprice = DRAM[Float](N)
    setMem(types, stypes)
    setMem(prices, sprices)
    setMem(strike, sstrike)
    setMem(rate, srate)
    setMem(vol, svolatility)
    setMem(times, stimes)

    Accel {
      Foreach(N by B par OP) { i =>
        val typeBlk   = SRAM[Int](B)
        val priceBlk  = SRAM[Float](B)
        val strikeBlk = SRAM[Float](B)
        val rateBlk   = SRAM[Float](B)
        val volBlk    = SRAM[Float](B)
        val timeBlk   = SRAM[Float](B)
        val optpriceBlk = SRAM[Float](B)

        Parallel {
          typeBlk   load types(i::i+B par IP)
          priceBlk  load prices(i::i+B par IP)
          strikeBlk load strike(i::i+B par IP)
          rateBlk   load rate(i::i+B par IP)
          volBlk    load vol(i::i+B par IP)
          timeBlk   load times(i::i+B par IP)
        }

        Foreach(B par IP){ j =>
          val price = BlkSchlsEqEuroNoDiv(priceBlk(j), strikeBlk(j), rateBlk(j), volBlk(j), timeBlk(j), typeBlk(j))
          optpriceBlk(j) = price
        }
        optprice(i::i+B par IP) store optpriceBlk
      }
    }
    getMem(optprice)
  }

  @virtualize
  def main(): Unit = {
    val N = args(0).to[Int]

    val types  = Array.fill(N)(random[Int](2))
    val prices = Array.fill(N)(random[Float])
    val strike = Array.fill(N)(random[Float])
    val rate   = Array.fill(N)(random[Float])
    val vol    = Array.fill(N)(random[Float])
    val time   = Array.fill(N)(random[Float])

    val out = blackscholes(types, prices, strike, rate, vol, time)

    printArray(out, "result: ")

    //val cksum = out.zip(gold){ case (o, g) => (g < (o + margin)) && g > (o - margin)}.reduce{_&&_}
    //println("PASS: " + cksum + " (BlackSholes)")


  }
}

object Sort_Merge extends SpatialApp { // Regression (Dense) // Args: none
  override val target = AWS_F1


 /*                                                                                                  
                              |     |                                                                                                                                                                                        
                     |        |     |                                      |     |                                                                                                                                                                                     
                     |  |     |     |  |                             |     |     |                                                                                                                                                                           
                     |  |  |  |     |  |                          |  |     |     |     |                                                                                                                                                                     
                     |  |  |  |  |  |  |                          |  |  |  |     |     |                                                                                                                                                                     
                     |  |  |  |  |  |  |  |                       |  |  |  |  |  |     |                                                                                                                                                                      
                     |  |  |  |  |  |  |  |                       |  |  |  |  |  |  |  |                                                                                                                                                                        
                     |  |  |  |  |  |  |  |                       |  |  |  |  |  |  |  |                                                                                                                                                                      
                                                                  |  |  |  |  |  |  |  |                      
   Outer FSM iter 1:  ↖↗    ↖↗    ↖↗    ↖↗      Outer FSM iter 2:  ↖.....↖     ↖.....↖                                                                                                                      
                     fifos numel = 1                               fifos numel = 2                                                            
                                                                  
                                                                                                     
                            |           |                                           |  |                                                                                                   
                         |  |           |                                        |  |  |                                                                                                   
                      |  |  |        |  |                                  |  |  |  |  |                                                                                                  
                   |  |  |  |        |  |                               |  |  |  |  |  |                                                                                                  
                   |  |  |  |     |  |  |                            |  |  |  |  |  |  |                                                                                                  
                   |  |  |  |  |  |  |  |                         |  |  |  |  |  |  |  |                                                                                                  
                   |  |  |  |  |  |  |  |                         |  |  |  |  |  |  |  |                                                                                                  
                   |  |  |  |  |  |  |  |                         |  |  |  |  |  |  |  |                                                                                                  
 Outer FSM iter 3:  ↖...........↖               Outer FSM iter 4:                                                                                                     
                   fifos numel = 4                                 Done
                                                                                                                                                                                                                   
                                                                                                                                                                           
 */

  @virtualize
  def main() = {

    val numel = 2048
    val START = 0
    val STOP = numel
    val levels = STOP-START //ArgIn[Int]
    // setArg(levels, args(0).to[Int])

    val raw_data = loadCSV1D[Int]("/remote/regression/data/machsuite/sort_data.csv", "\n")

    val data_dram = DRAM[Int](numel)
    // val sorted_dram = DRAM[Int](numel)

    setMem(data_dram, raw_data)

    Accel{
      val data_sram = SRAM[Int](numel)
      val lower_fifo = FIFO[Int](numel/2)
      val upper_fifo = FIFO[Int](numel/2)

      data_sram load data_dram


      FSM[Int,Int](1){m => m < levels} { m =>
        FSM[Int,Int](START)(i => i < STOP) { i =>
          val from = i
          val mid = i+m-1
          val to = min(i+m+m-1, STOP.to[Int])
          val lower_tmp = Reg[Int](0)
          val upper_tmp = Reg[Int](0)
          Foreach(from until mid+1 by 1){ i => if (i == from) {lower_tmp := data_sram(i)} else {lower_fifo.enq(data_sram(i))} }
          Foreach(mid+1 until to+1 by 1){ j => if (j == mid+1) {upper_tmp := data_sram(j)} else {upper_fifo.enq(data_sram(j))} }
          Sequential.Foreach(from until to+1 by 1) { k => 
            if (lower_tmp < upper_tmp) {
              Pipe{
                data_sram(k) = lower_tmp
                val next_lower = if (lower_fifo.empty) {0x7FFFFFFF.to[Int]} else {lower_fifo.deq()}
                lower_tmp := next_lower
              }
            } else {
              Pipe {
                data_sram(k) = upper_tmp
                val next_upper = if (upper_fifo.empty) {0x7FFFFFFF.to[Int]} else {upper_fifo.deq()}
                upper_tmp := next_upper
              }
            }
          }
        }{ i => i + m + m }
      }{ m => m + m}

      // sorted_dram store data_sram
      data_dram store data_sram
    }

    val sorted_gold = loadCSV1D[Int]("/remote/regression/data/machsuite/sort_gold.csv", "\n")
    val sorted_result = getMem(data_dram)

    printArray(sorted_gold, "Sorted Gold: ")
    printArray(sorted_result, "Sorted Result: ")

    val cksum = sorted_gold.zip(sorted_result){_==_}.reduce{_&&_}
    // // Use the real way to check if list is sorted instead of using machsuite gold
    // // This way says I've done goofed, issue #
    // val cksum = Array.tabulate(STOP-1){ i => pack(sorted_result(i), sorted_result(i+1)) }.map{a => a._1 <= a._2}.reduce{_&&_}
    println("PASS: " + cksum + " (Sort_Merge)")
  }
}

object KMP extends SpatialApp { // Regression (Dense) // Args: none
  override val target = AWS_F1


 /*
  
  Knuth-Morris-Pratt

  Used https://www.browserling.com/tools/text-to-hex to convert string to hex, and then converted hex to dec                                                               
                                                                                             
                                                                                                           
 */

  @virtualize
  def main() = {
    val raw_string_data = loadCSV1D[MString]("/remote/regression/data/machsuite/kmp_string.csv", "\n")
    val raw_string_pattern = "bull"//Array[Int](98,117,108,108)
    val raw_string = argon.lang.String.string2num(raw_string_data(0))
    val raw_pattern = argon.lang.String.string2num(raw_string_pattern)
    val STRING_SIZE_NUM = raw_string.length.to[Int]
    val PATTERN_SIZE_NUM = raw_pattern.length.to[Int]
    val STRING_SIZE = ArgIn[Int]
    val PATTERN_SIZE = ArgIn[Int]
    setArg(STRING_SIZE, STRING_SIZE_NUM)
    setArg(PATTERN_SIZE, PATTERN_SIZE_NUM)
    val string_dram = DRAM[Int8](STRING_SIZE)
    val pattern_dram = DRAM[Int8](PATTERN_SIZE)
    val nmatches = ArgOut[Int]

    setMem(string_dram, raw_string)
    setMem(pattern_dram, raw_pattern)

    Accel{
      val string_sram = SRAM[Int8](32411) // Conveniently sized
      val pattern_sram = SRAM[Int8](4) // Conveniently sized
      val kmp_next = SRAM[Int](4) // Conveniently sized
      val num_matches = Reg[Int](0)

      string_sram load string_dram
      pattern_sram load pattern_dram

      // Init kmp_next
      val k = Reg[Int](0)
      kmp_next(0) = 0
      Sequential.Foreach(1 until PATTERN_SIZE by 1) { q => 
        // val whileCond = Reg[Bit](false)
        FSM[Int](state => state != 1) { state => 
          // whileCond := (k > 0) && (pattern_sram(k) != pattern_sram(q))
          if ((k > 0) && (pattern_sram(k) != pattern_sram(q))) k := 0 // TODO: Will it always bump back to 0 in this step or should it really be kmp_next(q)?
        }{state => mux((k > 0) && (pattern_sram(k) != pattern_sram(q)), 0, 1)}
        if (pattern_sram(k) == pattern_sram(q)) {k :+= 1.to[Int]}
        kmp_next(q) = k
      }

      // Scan string
      val q = Reg[Int](0)
      Sequential.Foreach(0 until STRING_SIZE) { i => 
        // val whileCond = Reg[Bit](false)
        FSM[Int](state => state != 1) { state => 
          // whileCond := (q > 0) && (pattern_sram(i) != pattern_sram(q))
          if ((q > 0) && (string_sram(i) != pattern_sram(q))) q := kmp_next(q)
        }{state => mux((q > 0) && (string_sram(i) != pattern_sram(q)), 0, 1)}
        if (pattern_sram(q) == string_sram(i)) { q :+= 1 }
        if (q >= PATTERN_SIZE) {
          Pipe{
            num_matches :+= 1
            val bump = kmp_next(q - 1)
            q := bump
          }
        }
      }

      nmatches := num_matches
    }

    val gold_nmatches = 12
    val computed_nmatches = getArg(nmatches)

    println("Expected " + gold_nmatches + " matches")
    println("Found " + computed_nmatches)

    val cksum = gold_nmatches == computed_nmatches
    println("PASS: " + cksum + " (KMP) * Implement string find, string file parser, and string <-> hex <-> dec features once argon refactor is done so we can test any strings")
  }
}      

object TPCHQ6 extends SpatialApp { // Regression (Dense) // Args: 3840
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

  type FT = Int

  val MIN_DATE = 0
  val MAX_DATE = 9999
  val MIN_DISC = 0
  val MAX_DISC = 9999
  val margin = 1

  val innerPar = 16
  val outerPar = 2

  val tileSize = 384

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

    val ts = tileSize (96 -> 96 -> 192000)
    val op = outerPar (1 -> 6)
    val ip = innerPar (1 -> 384)
    val lp = 16 (1 -> 384)

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
          datesTile  load dates(i::i+ts par lp)
          quantsTile load quants(i::i+ts par lp)
          disctsTile load discts(i::i+ts par lp)
          pricesTile load prices(i::i+ts par lp)
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

object AES extends SpatialApp { // Regression (Dense) // Args: none
  override val target = AWS_F1

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
    
    // Create DRAMs
    val plaintext_dram = DRAM[UInt8](16)
    val key_dram = DRAM[UInt8](32)
    val sbox_dram = DRAM[UInt8](256)
    val ciphertext_dram = DRAM[UInt8](16)

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
      // Setup data structures
      val plaintext_flat = SRAM[UInt8](16)
      val plaintext_sram = SRAM[UInt8](4,4)
      val sbox_sram = SRAM[UInt8](256)
      val key_sram = SRAM[UInt8](32)
      // val mix_lut = LUT[Int](4,4)(
      //    2, 3, 1, 1,
      //    1, 2, 3, 1,
      //    1, 1, 2, 3,
      //    3, 1, 1, 2
      //  )
      val rcon = Reg[UInt8](1)

      // Specify methods
      def expand_key(): Unit = {
        Pipe{key_sram(0) = key_sram(0) ^ sbox_sram(key_sram(29).as[UInt16].as[Int]) ^ rcon}
        Pipe{key_sram(1) = key_sram(1) ^ sbox_sram(key_sram(30).as[UInt16].as[Int])}
        Pipe{key_sram(2) = key_sram(2) ^ sbox_sram(key_sram(31).as[UInt16].as[Int])}
        Pipe{key_sram(3) = key_sram(3) ^ sbox_sram(key_sram(28).as[UInt16].as[Int])}
        rcon := (((rcon)<<1) ^ ((((rcon)>>7) & 1) * 0x1b))

        Sequential.Foreach(4 until 16 by 4) {i =>
          Pipe{key_sram(i) = key_sram(i) ^ key_sram(i-4)}
          Pipe{key_sram(i+1) = key_sram(i+1) ^ key_sram(i-3)}
          Pipe{key_sram(i+2) = key_sram(i+2) ^ key_sram(i-2)}
          Pipe{key_sram(i+3) = key_sram(i+3) ^ key_sram(i-1)}
        }
      
        Pipe{key_sram(16) = key_sram(16) ^ sbox_sram(key_sram(12).as[UInt16].as[Int])}
        Pipe{key_sram(17) = key_sram(17) ^ sbox_sram(key_sram(13).as[UInt16].as[Int])}
        Pipe{key_sram(18) = key_sram(18) ^ sbox_sram(key_sram(14).as[UInt16].as[Int])}
        Pipe{key_sram(19) = key_sram(19) ^ sbox_sram(key_sram(15).as[UInt16].as[Int])}

        Sequential.Foreach(20 until 32 by 4) {i => 
          Pipe{key_sram(i) = key_sram(i) ^ key_sram(i-4)}
          Pipe{key_sram(i+1) = key_sram(i+1) ^ key_sram(i-3)}
          Pipe{key_sram(i+2) = key_sram(i+2) ^ key_sram(i-2)}
          Pipe{key_sram(i+3) = key_sram(i+3) ^ key_sram(i-1)}
        }
      }

      def shift_rows(): Unit = {
        Sequential.Foreach(4 by 1){ i => 
          val row = RegFile[UInt8](4)
          Foreach(4 by 1){ j => 
            val col_addr = (j - i) % 4
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
          Sequential.Foreach(4 by 1) { i => col(i) = plaintext_sram(i,j) }
          val e = Reduce(Reg[UInt8](0))(4 by 1 par 4) { i => col(i) }{_^_}
          // val e = col(0) ^ col(1) ^ col(2) ^ col(3)
          Pipe{plaintext_sram(0,j) = col(0) ^ e ^ rj_xtime(col(0) ^ col(1))}
          Pipe{plaintext_sram(1,j) = col(1) ^ e ^ rj_xtime(col(1) ^ col(2))}
          Pipe{plaintext_sram(2,j) = col(2) ^ e ^ rj_xtime(col(2) ^ col(3))}
          Pipe{plaintext_sram(3,j) = col(3) ^ e ^ rj_xtime(col(3) ^ col(0))}
        }
      }

      def add_round_key(round: Index): Unit = {
        Foreach(4 by 1, 4 by 1) { (i,j) => 
          val key = mux(round % 2 == 1, key_sram(i+j*4+16), key_sram(i+j*4))
          plaintext_sram(i,j) = plaintext_sram(i,j) ^ key
        }
      }

      // Load structures
      Parallel {
        plaintext_flat load plaintext_dram // TODO: Allow dram loads to reshape (gh issue #83)
        sbox_sram load sbox_dram
        key_sram load key_dram
      }

      // gh issue #83
      Foreach(4 by 1, 4 by 1){(i,j) => 
        plaintext_sram(i,j) = plaintext_flat(j*4+i) // MachSuite flattens columnwise... Why????
      } 

      // Do AES
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

      // Reshape plaintext_sram (gh issue # 83)
      Foreach(4 by 1, 4 by 1) {(i,j) => 
        plaintext_flat(j*4+i) = plaintext_sram(i,j)
      }

      ciphertext_dram store plaintext_flat

      // // Debugging
      // key_debug store key_sram

    }

    val ciphertext = getMem(ciphertext_dram)
    val ciphertext_gold = Array[UInt8](142,162,183,202,81,103,69,191,234,252,73,144,75,73,96,137)

    printArray(ciphertext_gold, "Expected: ")
    printArray(ciphertext, "Got: ")

    // // Debugging
    // val key_dbg = getMem(key_debug)
    // printArray(key_dbg, "Key: ")

    val cksum = ciphertext_gold.zip(ciphertext){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (AES) * For retiming, need to fix ^ reduction if not parallelized")

  }
}

object SHA extends SpatialApp { // Regression (Dense) // Args: none
  override val target = AWS_F1

  type ULong = FixPt[FALSE, _32, _0]
  @struct case class byte_pack(a: Int8, b: Int8, c: Int8, d: Int8)

  @virtualize
  def main() = {
    // Setup off-chip data
    val BLOCK_SIZE = 8192
    val SHA_BLOCKSIZE = 64

    val CONST1 = 0x5a827999L
    val CONST2 = 0x6ed9eba1L
    val CONST3 = 0x8f1bbcdcL
    val CONST4 = 0xca62c1d6L

    val raw_text = loadCSV1D[String]("/remote/regression/data/machsuite/sha_txt.csv", "\n").apply(0)
    // val raw_text = loadCSV1D[String]("/home/mattfel/txt", "\n").apply(0)
    val data_text = argon.lang.String.string2num(raw_text)
    // val data_text = loadCSV1D[Int8]("/home/mattfel/txt",",")
    val len = ArgIn[Int]
    setArg(len, data_text.length)
    val text_dram = DRAM[Int8](len)
    val hash_dram = DRAM[ULong](16)//(5)

    // println("Hashing: " + argon.lang.String.num2string(data_text) + " (len: " + data_text.length + ")")
    println("Hashing: " + raw_text + " (len: " + data_text.length + ")")
    // printArray(data_text, "text as data: ")
    setMem(text_dram, data_text)

    Accel{
      val buffer = SRAM[Int8](BLOCK_SIZE)
      val sha_digest = RegFile[ULong](5)
      val sha_data = SRAM[ULong](16)
      val count_lo = Reg[Int](0)
      val count_hi = Reg[Int](0)

      def asLong(r: Reg[ULong]): ULong = {r.value.apply(31::0).as[ULong]}

      def sha_transform(): Unit = {
        val W = SRAM[ULong](80)
        val A = Reg[ULong]
        val B = Reg[ULong]
        val C = Reg[ULong]
        val D = Reg[ULong]
        val E = Reg[ULong]

        Foreach(80 by 1) { i =>
          W(i) = if (i < 16) {sha_data(i)} else {W(i-3) ^ W(i-8) ^ W(i-14) ^ W(i-16)}
        }

        A := sha_digest(0)
        B := sha_digest(1)
        C := sha_digest(2)
        D := sha_digest(3)
        E := sha_digest(4)

        Sequential.Foreach(20 by 1) { i => 
          val temp = ((A << 5) | (A >> (32 - 5))) + E + W(i) + CONST1 + ((B & C) | (~B & D))
          E := D; D := C; C := ((B << 30) | (B >> (32 - 30))); B := A; A := temp
        }
        Sequential.Foreach(20 until 40 by 1) { i => 
          val temp = ((A << 5) | (A >> (32 - 5))) + E + W(i) + CONST2 + (B ^ C ^ D)
          E := D; D := C; C := ((B << 30) | (B >> (32 - 30))); B := A; A := temp
        }
        Sequential.Foreach(40 until 60 by 1) { i => 
          val temp = ((A << 5) | (A >> (32 - 5))) + E + W(i) + CONST3 + ((B & C) | (B & D) | (C & D))
          E := D; D := C; C := ((B << 30) | (B >> (32 - 30))); B := A; A := temp
        }
        Sequential.Foreach(60 until 80 by 1) { i => 
          val temp = ((A << 5) | (A >> (32 - 5))) + E + W(i) + CONST4 + (B ^ C ^ D)
          E := D; D := C; C := ((B << 30) | (B >> (32 - 30))); B := A; A := temp
        }

        Pipe{sha_digest(0) = sha_digest(0) + A}
        // Pipe{println("sha_digest 0 is " + sha_digest(0))}
        Pipe{sha_digest(1) = sha_digest(1) + B}
        Pipe{sha_digest(2) = sha_digest(2) + C}
        Pipe{sha_digest(3) = sha_digest(3) + D}
        Pipe{sha_digest(4) = sha_digest(4) + E}
      }
      def sha_update(count: Index): Unit = {
        if (count_lo + (count << 3) < count_lo) {count_hi :+= 1}
        count_lo :+= count << 3
        count_hi :+= count >> 29
        Sequential.Foreach(0 until count by SHA_BLOCKSIZE) { base => 
          val numel = min(count - base, SHA_BLOCKSIZE.to[Index])
          // TODO: Can make this one writer only
          if (numel == SHA_BLOCKSIZE) {Pipe{
            Sequential.Foreach(SHA_BLOCKSIZE/4 by 1){ i => 
              sha_data(i) = (buffer(base + i*4).as[ULong]) | (buffer(base + i*4+1).as[ULong] << 8) | (buffer(base + i*4 + 2).as[ULong] << 16) | (buffer(base + i*4+3).as[ULong] << 24)
            }
            sha_transform()
          }} else {
            Sequential(0 until numel by 1) { i => 
              sha_data(i) = (buffer(base + i*4).as[ULong]) | (buffer(base + i*4+1).as[ULong] << 8) | (buffer(base + i*4 + 2).as[ULong] << 16) | (buffer(base + i*4+3).as[ULong] << 24)
            }         
          }
        }

      }

      Pipe{sha_digest(0) = 0x67452301L.to[ULong]}
      Pipe{sha_digest(1) = 0xefcdab89L.to[ULong]}
      Pipe{sha_digest(2) = 0x98badcfeL.to[ULong]}
      Pipe{sha_digest(3) = 0x10325476L.to[ULong]}
      Pipe{sha_digest(4) = 0xc3d2e1f0L.to[ULong]}

      Sequential.Foreach(len by BLOCK_SIZE) { chunk => 
        val count = min(BLOCK_SIZE.to[Int], (len - chunk))
        buffer load text_dram(chunk::chunk+count)
        sha_update(count)

        // def byte_reverse(x: ULong): ULong = {
        //  byte_pack(x(31::24).as[Int8],x(23::16).as[Int8],x(15::8).as[Int8],x(7::0).as[Int8]).as[ULong]
        // }

        // Final sha
        // TODO: This last bit is probably wrong for any input that is not size 8192
        val lo_bit_count = count_lo.value.to[ULong]
        val hi_bit_count = count_hi.value.to[ULong]
        val count_final = ((lo_bit_count.to[Int8] >> 3) & 0x3f.to[Int8]).to[Int]
        sha_data(count_final) = 0x80
        if (count_final > 56) {
          Foreach(count_final+1 until 16 by 1) { i => sha_data(i) = 0 }
          Sequential(sha_transform())
          sha_data(14) = 0
        } else {
          Foreach(count_final+1 until 16 by 1) { i => sha_data(i) = 0 }
        }
        Pipe{sha_data(14) = hi_bit_count}
        Pipe{sha_data(15) = lo_bit_count}
        sha_transform()
      }


      hash_dram store sha_digest
    }

    val hashed_result = getMem(hash_dram)
    val hashed_gold = Array[ULong](1754467640L,1633762310L,3755791939L,3062269980L,2187536409L,0,0,0,0,0,0,0,0,0,0,0)
    printArray(hashed_gold, "Expected: ")
    printArray(hashed_result, "Got: ")

    val cksum = hashed_gold == hashed_result
    println("PASS: " + cksum + " (SHA)")

  }
}

object Kmeans extends SpatialApp { // Regression (Dense) // Args: 3 64

  type X = Int

  val numcents = 16
  val dim = 32
  val pts_per_ld = 1 // ???

  val ip = 16
  val op = 1

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
    val PX = 1 (1 -> 1)
    val P0 = ip (32 -> 96 -> 192) // Dimensions loaded in parallel
    val P1 = op (1 -> 12)         // Sets of points calculated in parallel
    val P2 = ip (1 -> 4 -> 96)    // Dimensions accumulated in parallel (outer)
    val P3 = ip (1 -> 4 -> 16)    // Points calculated in parallel
    val PR = ip (1 -> 4 -> 96)
    val P4 = ip (1 -> 4 -> 96)

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
      cts load points(0::K, 0::D par 16)

      // Initialize newCents
      // FPGA:
      Foreach(K by 1, D by 1) {(i,j) => newCents(i,j) = cts(i,j)} 

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
          pts load points(i::i+BN, 0::BD par 16)

          // For each point in this set
          MemFold(newCents)(BN par PX){pt =>
            // Find the index of the closest centroid
            val accum = Reg[Tup2[Int,T]]( pack(0.to[Int], 100000.to[T]) )
            val minCent = Reduce(accum)(K par PX){ct =>
              val dist = Reg[T](0.to[T])
              Reduce(dist)(D par P2){d => (pts(pt,d) - cts(ct,d)) ** 2 }{_+_}
              pack(ct, dist.value)
            }{(a,b) =>
              mux(a._2 < b._2, a, b)
            }
            /*// PIR Version
            val minCent = Reg[Int]
            val minDist = Reg[T](100000.to[T])
            Foreach(K par PX){ct =>
              val dist = Reduce(Reg[T])(D par P2){d =>
                val cent = mux(epoch == 0, origCts(ct,d), cts(ct,d))
                (pts(pt,d) - cent) ** 2
              }{_+_}

              Pipe {
                val d = dist.value
                minDist := min(minDist.value, d)
                minCent := mux(minDist.value == d, ct, minCent.value)
              }
            }*/

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
        Foreach(K by 1 par PX){ct => centCount(ct) = max(newCents(ct,DM1), 1.to[T]) } // Until diagonal banking is allowed

        // Average each new centroid
        // val centsOut = SRAM[T](MAXK, MAXD)
        Foreach(K by 1, D par P0){(ct,d) =>
//          val updateMux = mux(centCount(ct) == 0.to[T], 0.to[T], newCents(ct,d) / centCount(ct))
          cts(ct, d) = mux(centCount(ct) == 0.to[T], 0.to[T], newCents(ct,d) / centCount(ct)) //updateMux
        }
      }

      val flatCts = SRAM[T](MAXK * MAXD)
      Foreach(K by 1, D by 1) {(i,j) =>
        flatCts(i*D+j) = cts(i,j)
      }
      // Store the centroids out
      centroids(0::K*D par 16) store flatCts
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
    // val pts = Array.tabulate(N){i => Array.tabulate(D){d => if (d == D-1) 1.to[X] else if (d == 0) random[X](element_max) + i else 0.to[X]}}
    // val cnts = Array.tabulate(K){i => Array.tabulate(D){d => if (d == D-1) 1.to[X] else if (d == 0) random[X](element_max) + i else 0.to[X]}}
    // val pts = Array.tabulate(N){i => Array.tabulate(D){d => if (d == D-1) 1.to[X] else 5*i }}
    // val cnts = Array.tabulate(K){i => Array.tabulate(D){d => if (d == D-1) 1.to[X] else 5*i+1 }}

    // println("points: ")
    // for (i <- 0 until N) { println(i.mkString + ": " + pts(i).mkString(", ")) }

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

    // for (i <- 0 until result.length) {
    //   val diff = result(i) - gold(i)
    //   if (abs(diff) > margin)
    //     println("[" + i + "] gold: " + gold(i) + ", result: " + result(i) + ", diff: " + diff)
    // }

    val cksum = result.zip(gold){ case (o, g) => (g < (o + margin)) && g > (o - margin)}.reduce{_&&_}

    println("PASS: " + cksum + " (Kmeans)")
  }

}
