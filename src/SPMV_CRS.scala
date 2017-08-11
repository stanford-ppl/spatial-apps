import spatial.dsl._
import org.virtualized._
import spatial.targets._


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

    val innerPar = 16
    val par_load = innerPar
    val par_store = innerPar
    val tileSize = 494
    val tile_par = 2 (1 -> 1 -> 16)
    val pt_par = 4 (1 -> 1 -> 16)
    val red_par = innerPar (1 -> 1 -> 16)

    setMem(values_dram, raw_values)
    setMem(cols_dram, raw_cols)
    setMem(rowid_dram, raw_rowid)
    setMem(vec_dram, raw_vec)

    Accel {
      Foreach(N/tileSize by 1 par tile_par) { tile =>
        val rowid_sram = SRAM[Int](tileSize+1) // Should be tileSize+1 and result_sram should be tileSize
        val result_sram = SRAM[T](tileSize)

        rowid_sram load rowid_dram(tile*(tileSize+1) :: (tile+1)*(tileSize+1) par par_load) // tileSize+1 x N/tileSize times ~= N
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

