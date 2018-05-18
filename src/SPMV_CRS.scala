import spatial.dsl._
import virtualized._
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

    val NNZ = 1666
    val N = 494
    val tileSize = 494

    val raw_values = loadCSV1D[T](sys.env("SPATIAL_HOME") + "/apps/data/SPMV/crs_values.csv", "\n")
    val raw_cols = loadCSV1D[Int](sys.env("SPATIAL_HOME") + "/apps/data/SPMV/crs_cols.csv", "\n")
    val raw_rowid = loadCSV1D[Int](sys.env("SPATIAL_HOME") + "/apps/data/SPMV/crs_rowid.csv", "\n")
    val raw_vec = loadCSV1D[T](sys.env("SPATIAL_HOME") + "/apps/data/SPMV/crs_vec.csv", "\n")

    val values_dram = DRAM[T](NNZ) 
    val cols_dram = DRAM[Int](NNZ) 
    val rowid_dram = DRAM[Int](N+1) 
    val vec_dram = DRAM[T](N) 
    val result_dram = DRAM[T](N)

    val par_load = 16
    val par_segment_load = 1 // Do not change
    val par_store = 1 // Do not change
    val tile_par = 1 (1 -> 1 -> 16) // Do not change
    val pt_par = 1 (1 -> 1 -> 16) // Do not change?
    val red_par = 2 (1 -> 1 -> 16)

    val size = ArgIn[Int]
    bound(size) = N
    setMem(values_dram, raw_values)
    setMem(cols_dram, raw_cols)
    setMem(rowid_dram, raw_rowid)
    setMem(vec_dram, raw_vec)

    Accel {
      Foreach(size/tileSize by 1 par tile_par) { tile =>
        val rowid_sram = SRAM[Int](tileSize+1)
        val result_sram = SRAM[T](tileSize)

        rowid_sram load rowid_dram(tile*(tileSize+1) :: (tile+1)*(tileSize+1) par par_load)
        Foreach(tileSize by 1 par pt_par) { i => 
          val cols_sram = SRAM[Int](tileSize)
          val values_sram = SRAM[T](tileSize)
          val vec_sram = SRAM[T](tileSize)

          val start_id = rowid_sram(i)
          val stop_id = rowid_sram(i+1)
          Parallel{
            cols_sram load cols_dram(start_id :: stop_id par par_segment_load)
            values_sram load values_dram(start_id :: stop_id par par_segment_load)
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

    val data_gold = loadCSV1D[T](sys.env("SPATIAL_HOME") + "/apps/data/SPMV/crs_gold.csv", "\n")
    val data_result = getMem(result_dram)

    printArray(data_gold, "Gold: ")
    printArray(data_result, "Result: ")

    val margin = 0.2.to[T] // Scala does not stay in bounds as tightly as chisel
    val cksum = data_gold.zip(data_result){(a,b) => abs(a-b) < margin}.reduce{_&&_}

    println("PASS: " + cksum + " (SPMV_CRS) * Fix gather on arbitrary width elements (64 for better prec here), issue #126")


  }
}

