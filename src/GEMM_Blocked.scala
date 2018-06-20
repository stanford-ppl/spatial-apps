import spatial.dsl._
import virtualized._
import spatial.targets._

object GEMM_Blocked extends SpatialApp { // Regression (Dense) // Args: 128
  override val target = AWS_F1
                                                                                                  
  val DIM = 512 // param

  val ts = 32 // param
  val its = 64 // param
  val loop_ii    = 1 // param (1, <DIM> / <its>, 4)
  val loop_jj    = 1 // param (1, <DIM> / <ts>, 4)
  val loop_kk    = 1 // param (1, <DIM> / <ts>, 4)
  val loop_i     = 1 // param (1, 5, 1)
  val loop_k     = 1 // param (1, 5, 1)

  val ip = 16
  val loop_j     = ip
  val par_load = ip
  val par_store = ip
  val reduce_col = ip
  val reduce_tmp = ip
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
  type T = FixPt[TRUE,_16,_16] // Fatter type so that ts is burst aligned

  def gemm(a_data:Matrix[T], b_data:Matrix[T], c_init:Matrix[T]) = {

    val dim = ArgIn[Int]
    setArg(dim, a_data.rows); //bound(a_data.length) = DIM 
    val a_dram = DRAM[T](dim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](dim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)
    setMem(c_dram, c_init)

    Accel{
      Foreach(dim by its par loop_ii) { ii => // this loop defenitilely cant be parallelized right now
        Foreach(dim by ts par loop_jj) { jj => 
          val c_col = SRAM[T](its,ts)
          MemReduce(c_col par reduce_col)(dim by ts par loop_kk) { kk => 
            val c_col_partial = SRAM[T](its,ts)
            val b_sram = SRAM[T](ts,ts)
            b_sram load b_dram(kk::kk+ts, jj::jj+ts par par_load)
            Foreach(its by 1 par loop_i) { i => 
              val a_sram = SRAM[T](ts)
              a_sram load a_dram(ii+i, kk::kk+ts)
              val c_tmp = SRAM[T](ts)
              MemReduce(c_tmp par reduce_tmp)(ts by 1 par loop_k) { k => 
                val c_tmp_partial = SRAM[T](ts)
                val temp_a = a_sram(k)
                Foreach(ts by 1 par loop_j) { j => 
                  c_tmp_partial(j) = b_sram(k, j) * temp_a
                }
                c_tmp_partial
              }{_+_}
            Foreach(ts by 1){cpy => c_col_partial(i,cpy) = c_tmp(cpy)}
            }
          c_col_partial
          }{_+_}
          c_dram(ii::ii+its, jj::jj+ts par par_store) store c_col
        }
      }
    }
    getMatrix(c_dram)
  }

  @virtualize
  def main() {

    val a_data = (0::DIM,0::DIM){(i,j) => random[T](5)}
    val b_data = (0::DIM,0::DIM){(i,j) => random[T](5)}
    // val a_data = loadCSV1D[T](sys.env("SPATIAL_HOME") + "/apps/data/gemm/gemm_a.csv", "\n").reshape(dim,dim)
    // val b_data = loadCSV1D[T](sys.env("SPATIAL_HOME") + "/apps/data/gemm/gemm_b.csv", "\n").reshape(dim,dim)
    val c_init = (0::DIM, 0::DIM){(i,j) => 0.to[T]}


    // val c_gold = loadCSV1D[T](sys.env("SPATIAL_HOME") + "/apps/data/gemm/gemm_gold.csv", "\n").reshape(dim,dim)
    val c_gold = (0::DIM,0::DIM){(i,j) => 
      Array.tabulate(DIM){k => a_data(i,k) * b_data(k,j)}.reduce{_+_}
    }
    val c_result = gemm(a_data, b_data, c_init)

    printMatrix(c_gold, "C Gold: ")
    printMatrix(c_result, "C Result: ")

    val margin = 0.5.to[T]
    val cksum = c_gold.zip(c_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (GEMM_Blocked)")
  }
}

