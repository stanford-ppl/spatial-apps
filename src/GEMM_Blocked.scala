import spatial.dsl._
import org.virtualized._
import spatial.targets._

object GEMM_Blocked extends SpatialApp { // Regression (Dense) // Args: none
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
                                                                                           
                                                                
>>>>>>> origin/asplos2018

        
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
     ↳ .................__________...................           .................__________....................   
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
  type T = FixPt[TRUE,_32,_32] // Fatter type so that tileSize is burst aligned

  @virtualize
  def main() = {

    val dim = 64
    val innerPar = 16
    val tileSize = innerPar

    val par_load = tileSize
    val par_store = tileSize
    val loop_jj    = 3 (1 -> 1 -> dim/tileSize)
    val loop_kk    = 2 (1 -> 1 -> dim/tileSize)
    val loop_i     = 2 (1 -> 1 -> dim)
    val loop_k     = 2 (1 -> 1 -> tileSize)
    val loop_j     = innerPar (1 -> 1 -> tileSize)
    val reduce_col = innerPar (1 -> 1 -> tileSize)
    val reduce_tmp = innerPar (1 -> 1 -> tileSize)

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

      Foreach(dim by tileSize par loop_jj) { jj => 
        val c_col = SRAM[T](dim,tileSize)
        MemReduce(c_col par reduce_col)(dim by tileSize par loop_kk) { kk => 
          val c_col_partial = SRAM[T](dim,tileSize)
          val b_sram = SRAM[T](tileSize,tileSize)
          b_sram load b_dram(kk::kk+tileSize, jj::jj+tileSize par par_load)
          Foreach(dim by 1 par loop_i) { i => 
            val a_sram = SRAM[T](tileSize)
            a_sram load a_dram(i, kk::kk+tileSize)
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
        c_dram(0::dim, jj::jj+tileSize par par_store) store c_col
      }
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

