import spatial.dsl._
import virtualized._

object lenetUnit extends SpatialApp {

  type T = FixPt[TRUE,_5,_11] // Use higher precision for more accuracy
  val BATCH_SIZE = 17         // TODO: Make this an argin instead of hard-coded
  
  @virtualize
  def lenet_Dec6[T:Type:Num](
    i0: Array[T],
    c0: Array[T],
    c1: Array[T],
    c2: Array[T],
    c3: Array[T],
    c4: Array[T],
    c5: Array[T],
    c6: Array[T],
    c7: Array[T]
  ) : Matrix[T] = {

    val c0_DRAM = DRAM[T](20,1,32)
    val i0_DRAM = DRAM[T](BATCH_SIZE,28,32)
    val c1_DRAM = DRAM[T](32)
    val c2_DRAM = DRAM[T](50,512)
    val c3_DRAM = DRAM[T](64)
    val c4_DRAM = DRAM[T](100, 4000)

    val c5_DRAM = DRAM[T](512)
    val c6_DRAM = DRAM[T](10,512)

    val c7_DRAM = DRAM[T](32)
    val tmp5_DRAM = DRAM[T](BATCH_SIZE,32)

    val c0_reshaped = c0.reshape(20,1,25)
    val c0_new = (0::20, 0::1, 0::32){(i,j,k) => if (k < 25) c0_reshaped(i,j,k) else 0 };
    setMem(c0_DRAM, c0_new)

    val i0_reshaped = i0.reshape(BATCH_SIZE,28,28)
    val i0_new = (0::BATCH_SIZE, 0::28, 0::32){(i,k,l) => if (l < 28) i0_reshaped(i,k,l) else 0 };
    setMem(i0_DRAM, i0_new)
    
    setMem(c1_DRAM, c1)

    val c2_reshaped = c2.reshape(50,500)
    val c2_new = (0::50, 0::512){(i,j) => if (j < 500) c2_reshaped(i,j) else 0 };
    setMem(c2_DRAM, c2_new)
    
    setMem(c3_DRAM, c3)
    
    setMem(c4_DRAM, c4.reshape(100,4000))
    
    setMem(c5_DRAM, c5)

    val c6_reshaped = c6.reshape(10,500)
    val c6_new = (0::10, 0::512){(i,j) => if (j < 500) c6_reshaped(i,j) else 0 };
    setMem(c6_DRAM, c6_new)
    
    setMem(c7_DRAM, c7)

    Accel {
    
      val c1_SRAM = SRAM[T](32)
      c1_SRAM load c1_DRAM(0::32 par 1)

      //val c3_SRAM = SRAM[T](64)
      //c3_SRAM load c3_DRAM(0::64 par 1)

      //val c5_SRAM = SRAM[T](512)
      //c5_SRAM load c5_DRAM(0::512 par 1)
      
      //val c7_SRAM = SRAM[T](32)
      //c7_SRAM load c7_DRAM(0::32)
      
      Foreach(BATCH_SIZE by 1 par 1) { batch_img =>

        // Move data on-chip
        val i0_SRAM = SRAM[T](28,32)
        i0_SRAM load i0_DRAM(batch_img, 0::28, 0::32 par 1)
        
        // Conv2D
        val tmp1_SRAM = SRAM[T](20,12,12)
        Foreach(20 by 1 par 1) { outD_i => // out channels
          val nr = 28
          val nc = 28
          val kr = 5
          val kc = 5
          val or = 24
          val oc = 24
          //val tmp1_SRAM_conv = SRAM[T](or, oc) // 24 x 24
          val c0_RF = RegFile[T](32)
          c0_RF load c0_DRAM(outD_i, 0, 0::32 par 1)
          //Foreach(0 until or, 0 until oc par 1) { (r,c) =>
            //val window = Reduce(Reg[T](0.to[T]))(0 until kr par 1, 0 until kc par 1){ (i,j) =>
              //i0_SRAM(r+i,c+j) * c0_RF(i*5+j)
            //}{_+_}
            //tmp1_SRAM_conv(r, c) = window.value
          //}
          //Foreach(12 by 1, 12 by 1) { (i,j) =>
            //// pulling + relu + biasAdd
            //val out = Reduce(Reg[T](0.to[T]))(2 by 1, 2 by 1) { (ii, jj) =>
              //max(0.to[T], tmp1_SRAM_conv(i*2 + ii, j*2 + jj) + c1_SRAM(outD_i))
            //} { (x,y) => max(x,y) }
            //tmp1_SRAM(outD_i, i, j) = out.value
          //}
          Foreach(12 by 1, 12 by 1) { (i,j) =>
            // pulling + relu + biasAdd
            val out = Reduce(Reg[T](0.to[T]))(2 by 1, 2 by 1) { (ii, jj) =>
              val window = Reduce(Reg[T](0.to[T]))(0 until kr par 1, 0 until kc par 1){ (ki,kj) =>
                val r = i*2 + ii
                val c = j*2 + jj
                i0_SRAM(r+ki,c+kj) * c0_RF(ki*5+j)
              }{_+_}
              max(0.to[T], window.value + c1_SRAM(outD_i))
            } { (x,y) => max(x,y) }
            tmp1_SRAM(outD_i, i, j) = out.value
          }

        }
        // Optimization: BiasAdd was merged into Conv2D above
        // Optimization: ReLU was merged into Conv2D above
        // Optimization: MaxPool was merged into Conv2D above

        val tmp_DRAM = DRAM[T](20, 12, 12)
        tmp_DRAM(0::20, 0::12, 0::12) store tmp1_SRAM
      } // Metapipeline over all images
    }
    
    getMatrix(tmp5_DRAM)
  }

  @virtualize
  def main() {
    val i0 = loadCSV1D[T]("/home/shadjis/spatial/DEVELOP_spatial-lang/csv_lenetDNNW/data_in2.csv", "\n")
    val c0 = loadCSV1D[T]("/home/shadjis/spatial/DEVELOP_spatial-lang/csv_lenetDNNW/c0.csv", "\n") // conv1/Variable
    val c1 = loadCSV1D[T]("/home/shadjis/spatial/DEVELOP_spatial-lang/csv_lenetDNNW/c1.csv", "\n") // conv1/Variable_1
    val c2 = loadCSV1D[T]("/home/shadjis/spatial/DEVELOP_spatial-lang/csv_lenetDNNW/c2.csv", "\n") // conv2/Variable
    val c3 = loadCSV1D[T]("/home/shadjis/spatial/DEVELOP_spatial-lang/csv_lenetDNNW/c3.csv", "\n") // conv2/Variable_1
    val c4 = loadCSV1D[T]("/home/shadjis/spatial/DEVELOP_spatial-lang/csv_lenetDNNW/c4.csv", "\n") // fc1/Variable
    val c5 = loadCSV1D[T]("/home/shadjis/spatial/DEVELOP_spatial-lang/csv_lenetDNNW/c5.csv", "\n") // fc1/Variable_1
    val c6 = loadCSV1D[T]("/home/shadjis/spatial/DEVELOP_spatial-lang/csv_lenetDNNW/c6.csv", "\n") // fc2/Variable
    val c7 = loadCSV1D[T]("/home/shadjis/spatial/DEVELOP_spatial-lang/csv_lenetDNNW/c7.csv", "\n") // fc2/Variable_1
    val output = lenet_Dec6(i0, c0, c1, c2, c3, c4, c5, c6, c7)
    val output_no_extra = Array.tabulate(170){i => output(i/10, i%10)}
    printArray(output_no_extra, "output")
    val gold = loadCSV1D[T]("/home/shadjis/spatial/DEVELOP_spatial-lang/csv_lenetDNNW/data_out.csv", "\n")
    printArray(gold, "gold")
    val margin = 1.882.to[T]
  	val cksum = gold.zip(output_no_extra){(a,b) => abs(a-b) < margin}.reduce{_&&_}
  	println("PASS: " + cksum)
  }
}