import spatial.dsl._
import virtualized._
import spatial.targets._

object GEMM_Blocked__DIM_512_IDIM_256_ts_256_its_128_loop_ii_1_loop_jj_1_loop_kk_1_loop_i_1_loop_k_16 extends SpatialApp { // Regression (Dense) // Args: 128
  override val target = AWS_F1
                                                                                                  
val DIM = 512
val IDIM = 256

val ts = 256
val its = 128
val loop_ii = 1
val loop_jj = 1
val loop_kk = 1
val loop_i = 1
val loop_k = 16

  val ip = 16
  type T = FixPt[TRUE,_16,_16] // Fatter type so that ts is burst aligned

  def gemm(a_data:Matrix[T], b_data:Matrix[T], c_init:Matrix[T]) = {

    val dim = ArgIn[Int]
    val idim = ArgIn[Int]
    setArg(dim, a_data.rows); bound(dim) = DIM
    setArg(idim, IDIM); bound(idim) = IDIM
    val a_dram = DRAM[T](idim,dim)
    val b_dram = DRAM[T](dim,dim)
    val c_dram = DRAM[T](idim,dim)

    setMem(a_dram, a_data)
    setMem(b_dram, b_data)
    setMem(c_dram, c_init)

    Accel{
      Foreach(idim by its par loop_ii) { ii => // this loop defenitilely cant be parallelized right now
        Foreach(dim by ts par loop_jj) { jj => 
          val c_col = SRAM[T](its,ts)
          MemReduce(c_col par ip)(dim by ts par loop_kk) { kk => 
            val c_col_partial = SRAM[T](its,ts)
            val b_sram = SRAM[T](ts,ts)
            b_sram load b_dram(kk::kk+ts, jj::jj+ts par ip)
            Foreach(its by 1 par loop_i) { i => 
              val a_sram = SRAM[T](ts)
              a_sram load a_dram(ii+i, kk::kk+ts par ip)
              val c_tmp = SRAM[T](ts)
              MemReduce(c_tmp par ip)(ts by 1 par loop_k) { k => 
                val c_tmp_partial = SRAM[T](ts)
                val temp_a = a_sram(k)
                Foreach(ts by 1 par ip) { j => c_tmp_partial(j) = b_sram(k, j) * temp_a }
                c_tmp_partial
              }{_+_}
              Foreach(ts by 1 par ip){cpy => c_col_partial(i,cpy) = c_tmp(cpy)}
            }
            c_col_partial
          }{_+_}
          c_dram(ii::ii+its, jj::jj+ts par ip) store c_col
        }
      }
    }
    getMatrix(c_dram)
  }

  @virtualize
  def main() {

    val a_data = (0::DIM,0::DIM){(i,j) => random[T](5)}
    val b_data = (0::DIM,0::DIM){(i,j) => random[T](5)}
    val c_init = (0::DIM, 0::DIM){(i,j) => 0.to[T]}


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

