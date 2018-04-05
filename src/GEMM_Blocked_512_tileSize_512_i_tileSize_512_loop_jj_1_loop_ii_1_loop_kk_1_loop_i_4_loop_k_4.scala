import spatial.dsl._
import virtualized._
import spatial.targets._

object GEMM_Blocked_512_tileSize_512_i_tileSize_512_loop_jj_1_loop_ii_1_loop_kk_1_loop_i_4_loop_k_4 extends SpatialApp { // Regression (Dense) // Args: 128
  override val target = AWS_F1
                                                                                                  
                                                                                                  
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
    val innerPar = 16
val tileSize = 512
val i_tileSize = 512
val loop_jj = 1
val loop_ii = 1
val loop_kk = 1
val loop_i = 4
val loop_k = 4
    val par_load = innerPar
    val par_store = innerPar
    val loop_j     = innerPar (1 -> 1 -> 16)
    val reduce_col = innerPar (1 -> 1 -> 16)
    val reduce_tmp = innerPar (1 -> 1 -> 16)

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

