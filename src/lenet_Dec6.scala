import spatial.dsl._
import org.virtualized._

object lenet_Dec6 extends SpatialApp {

  type T = FixPt[TRUE,_5,_11]
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

      val c3_SRAM = SRAM[T](64)
      c3_SRAM load c3_DRAM(0::64 par 1)

      val c5_SRAM = SRAM[T](512)
      c5_SRAM load c5_DRAM(0::512 par 1)
      
      val c7_SRAM = SRAM[T](32)
      c7_SRAM load c7_DRAM(0::32)
      
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
          val tmp1_SRAM_conv = SRAM[T](or, oc)
          val c0_RF = RegFile[T](32)
          c0_RF load c0_DRAM(outD_i, 0, 0::32 par 1)
          Foreach(0 until or, 0 until oc par 1) { (r,c) =>
          
            // The following loop should be used, but Spatial does not automatically unroll it currently,
            // so instead I unrolled it manually below.
            /*
            val window = Reduce(Reg[T](0.to[T]))(0 until kr par 1, 0 until kc par kc){ (i,j) =>
              i0_SRAM(0, r.to[Index]+i.to[Index],c.to[Index]+j.to[Index]) * c0_RF(i*5+j)
            }{_+_}
            tmp1_SRAM_conv(r, c) = window.value
            */
            
            // Manual version
            
            val prod00 = i0_SRAM(r.to[Index]+0.to[Index],c.to[Index]+0.to[Index]) * c0_RF(0*5+0)
            val prod01 = i0_SRAM(r.to[Index]+0.to[Index],c.to[Index]+1.to[Index]) * c0_RF(0*5+1)
            val prod02 = i0_SRAM(r.to[Index]+0.to[Index],c.to[Index]+2.to[Index]) * c0_RF(0*5+2)
            val prod03 = i0_SRAM(r.to[Index]+0.to[Index],c.to[Index]+3.to[Index]) * c0_RF(0*5+3)
            val prod04 = i0_SRAM(r.to[Index]+0.to[Index],c.to[Index]+4.to[Index]) * c0_RF(0*5+4)
            val prod05 = i0_SRAM(r.to[Index]+1.to[Index],c.to[Index]+0.to[Index]) * c0_RF(1*5+0)
            val prod06 = i0_SRAM(r.to[Index]+1.to[Index],c.to[Index]+1.to[Index]) * c0_RF(1*5+1)
            val prod07 = i0_SRAM(r.to[Index]+1.to[Index],c.to[Index]+2.to[Index]) * c0_RF(1*5+2)
            val prod08 = i0_SRAM(r.to[Index]+1.to[Index],c.to[Index]+3.to[Index]) * c0_RF(1*5+3)
            val prod09 = i0_SRAM(r.to[Index]+1.to[Index],c.to[Index]+4.to[Index]) * c0_RF(1*5+4)
            val prod10 = i0_SRAM(r.to[Index]+2.to[Index],c.to[Index]+0.to[Index]) * c0_RF(2*5+0)
            val prod11 = i0_SRAM(r.to[Index]+2.to[Index],c.to[Index]+1.to[Index]) * c0_RF(2*5+1)
            val prod12 = i0_SRAM(r.to[Index]+2.to[Index],c.to[Index]+2.to[Index]) * c0_RF(2*5+2)
            val prod13 = i0_SRAM(r.to[Index]+2.to[Index],c.to[Index]+3.to[Index]) * c0_RF(2*5+3)
            val prod14 = i0_SRAM(r.to[Index]+2.to[Index],c.to[Index]+4.to[Index]) * c0_RF(2*5+4)
            val prod15 = i0_SRAM(r.to[Index]+3.to[Index],c.to[Index]+0.to[Index]) * c0_RF(3*5+0)
            val prod16 = i0_SRAM(r.to[Index]+3.to[Index],c.to[Index]+1.to[Index]) * c0_RF(3*5+1)
            val prod17 = i0_SRAM(r.to[Index]+3.to[Index],c.to[Index]+2.to[Index]) * c0_RF(3*5+2)
            val prod18 = i0_SRAM(r.to[Index]+3.to[Index],c.to[Index]+3.to[Index]) * c0_RF(3*5+3)
            val prod19 = i0_SRAM(r.to[Index]+3.to[Index],c.to[Index]+4.to[Index]) * c0_RF(3*5+4)
            val prod20 = i0_SRAM(r.to[Index]+4.to[Index],c.to[Index]+0.to[Index]) * c0_RF(4*5+0)
            val prod21 = i0_SRAM(r.to[Index]+4.to[Index],c.to[Index]+1.to[Index]) * c0_RF(4*5+1)
            val prod22 = i0_SRAM(r.to[Index]+4.to[Index],c.to[Index]+2.to[Index]) * c0_RF(4*5+2)
            val prod23 = i0_SRAM(r.to[Index]+4.to[Index],c.to[Index]+3.to[Index]) * c0_RF(4*5+3)
            val prod24 = i0_SRAM(r.to[Index]+4.to[Index],c.to[Index]+4.to[Index]) * c0_RF(4*5+4)

            val tree_level_0_00 = prod00 + prod01
            val tree_level_0_01 = prod02 + prod03
            val tree_level_0_02 = prod04 + prod05
            val tree_level_0_03 = prod06 + prod07
            val tree_level_0_04 = prod08 + prod09
            val tree_level_0_05 = prod10 + prod11
            val tree_level_0_06 = prod12 + prod13
            val tree_level_0_07 = prod14 + prod15
            val tree_level_0_08 = prod16 + prod17
            val tree_level_0_09 = prod18 + prod19
            val tree_level_0_10 = prod20 + prod21
            val tree_level_0_11 = prod22 + prod23
            val tree_level_0_12 = prod24

            val tree_level_1_00 = tree_level_0_00 + tree_level_0_01
            val tree_level_1_01 = tree_level_0_02 + tree_level_0_03
            val tree_level_1_02 = tree_level_0_04 + tree_level_0_05
            val tree_level_1_03 = tree_level_0_06 + tree_level_0_07
            val tree_level_1_04 = tree_level_0_08 + tree_level_0_09
            val tree_level_1_05 = tree_level_0_10 + tree_level_0_11
            val tree_level_1_06 = tree_level_0_12

            val tree_level_2_00 = tree_level_1_00 + tree_level_1_01
            val tree_level_2_01 = tree_level_1_02 + tree_level_1_03
            val tree_level_2_02 = tree_level_1_04 + tree_level_1_05
            val tree_level_2_03 = tree_level_1_06
            
            val tree_level_3_00 = tree_level_2_00 + tree_level_2_01
            val tree_level_3_01 = tree_level_2_02 + tree_level_2_03
              
            tmp1_SRAM_conv(r, c) = tree_level_3_00 + tree_level_3_01
          }
          // Fused BiasAdd
          Foreach(12 by 1, 12 by 1) { (i,j) =>
            val px0 = max(0.to[T], tmp1_SRAM_conv(i.to[Int]*2 + 0.to[Int], j.to[Int]*2 + 0.to[Int]) + c1_SRAM(outD_i))
            val px1 = max(0.to[T], tmp1_SRAM_conv(i.to[Int]*2 + 0.to[Int], j.to[Int]*2 + 1.to[Int]) + c1_SRAM(outD_i))
            val px2 = max(0.to[T], tmp1_SRAM_conv(i.to[Int]*2 + 1.to[Int], j.to[Int]*2 + 0.to[Int]) + c1_SRAM(outD_i))
            val px3 = max(0.to[T], tmp1_SRAM_conv(i.to[Int]*2 + 1.to[Int], j.to[Int]*2 + 1.to[Int]) + c1_SRAM(outD_i))

            val tree_level_0_0 = max(px0, px1)
            val tree_level_0_1 = max(px2, px3)

            tmp1_SRAM(outD_i, i, j) = max(tree_level_0_0, tree_level_0_1)
          }
        }
        // Optimization: BiasAdd was merged into Conv2D above
        // Optimization: ReLU was merged into Conv2D above
        // Optimization: MaxPool was merged into Conv2D above

        // Conv2D
        val tmp2_SRAM = SRAM[T](50,4,4)
        Foreach(50 by 1 par 2) { outD_i => // out channels
          val nr = 12
          val nc = 12
          val kr = 5
          val kc = 5
          val or = 8
          val oc = 8
          val d = 20
          val tmp2_SRAM_conv = SRAM[T](or, oc)
          val c2_RF = SRAM[T](512)
          c2_RF load c2_DRAM(outD_i, 0::512 par 1)                            // DAVID TODO: Should be par 8, but causes banking error currently
          MemReduce(tmp2_SRAM_conv)(d by 1 par 1) { inD_i => // in channels   // DAVID TODO: Should be par 2, but causes banking error currently
            val result = SRAM[T](or, oc)
            Foreach(0 until or, 0 until oc par 1) { (r,c) =>
              
              val prod00 = tmp1_SRAM(inD_i, r.to[Index]+0.to[Index],c.to[Index]+0.to[Index]) * c2_RF(inD_i.to[Index]*25 + 0*5+0)
              val prod01 = tmp1_SRAM(inD_i, r.to[Index]+0.to[Index],c.to[Index]+1.to[Index]) * c2_RF(inD_i.to[Index]*25 + 0*5+1)
              val prod02 = tmp1_SRAM(inD_i, r.to[Index]+0.to[Index],c.to[Index]+2.to[Index]) * c2_RF(inD_i.to[Index]*25 + 0*5+2)
              val prod03 = tmp1_SRAM(inD_i, r.to[Index]+0.to[Index],c.to[Index]+3.to[Index]) * c2_RF(inD_i.to[Index]*25 + 0*5+3)
              val prod04 = tmp1_SRAM(inD_i, r.to[Index]+0.to[Index],c.to[Index]+4.to[Index]) * c2_RF(inD_i.to[Index]*25 + 0*5+4)
              val prod05 = tmp1_SRAM(inD_i, r.to[Index]+1.to[Index],c.to[Index]+0.to[Index]) * c2_RF(inD_i.to[Index]*25 + 1*5+0)
              val prod06 = tmp1_SRAM(inD_i, r.to[Index]+1.to[Index],c.to[Index]+1.to[Index]) * c2_RF(inD_i.to[Index]*25 + 1*5+1)
              val prod07 = tmp1_SRAM(inD_i, r.to[Index]+1.to[Index],c.to[Index]+2.to[Index]) * c2_RF(inD_i.to[Index]*25 + 1*5+2)
              val prod08 = tmp1_SRAM(inD_i, r.to[Index]+1.to[Index],c.to[Index]+3.to[Index]) * c2_RF(inD_i.to[Index]*25 + 1*5+3)
              val prod09 = tmp1_SRAM(inD_i, r.to[Index]+1.to[Index],c.to[Index]+4.to[Index]) * c2_RF(inD_i.to[Index]*25 + 1*5+4)
              val prod10 = tmp1_SRAM(inD_i, r.to[Index]+2.to[Index],c.to[Index]+0.to[Index]) * c2_RF(inD_i.to[Index]*25 + 2*5+0)
              val prod11 = tmp1_SRAM(inD_i, r.to[Index]+2.to[Index],c.to[Index]+1.to[Index]) * c2_RF(inD_i.to[Index]*25 + 2*5+1)
              val prod12 = tmp1_SRAM(inD_i, r.to[Index]+2.to[Index],c.to[Index]+2.to[Index]) * c2_RF(inD_i.to[Index]*25 + 2*5+2)
              val prod13 = tmp1_SRAM(inD_i, r.to[Index]+2.to[Index],c.to[Index]+3.to[Index]) * c2_RF(inD_i.to[Index]*25 + 2*5+3)
              val prod14 = tmp1_SRAM(inD_i, r.to[Index]+2.to[Index],c.to[Index]+4.to[Index]) * c2_RF(inD_i.to[Index]*25 + 2*5+4)
              val prod15 = tmp1_SRAM(inD_i, r.to[Index]+3.to[Index],c.to[Index]+0.to[Index]) * c2_RF(inD_i.to[Index]*25 + 3*5+0)
              val prod16 = tmp1_SRAM(inD_i, r.to[Index]+3.to[Index],c.to[Index]+1.to[Index]) * c2_RF(inD_i.to[Index]*25 + 3*5+1)
              val prod17 = tmp1_SRAM(inD_i, r.to[Index]+3.to[Index],c.to[Index]+2.to[Index]) * c2_RF(inD_i.to[Index]*25 + 3*5+2)
              val prod18 = tmp1_SRAM(inD_i, r.to[Index]+3.to[Index],c.to[Index]+3.to[Index]) * c2_RF(inD_i.to[Index]*25 + 3*5+3)
              val prod19 = tmp1_SRAM(inD_i, r.to[Index]+3.to[Index],c.to[Index]+4.to[Index]) * c2_RF(inD_i.to[Index]*25 + 3*5+4)
              val prod20 = tmp1_SRAM(inD_i, r.to[Index]+4.to[Index],c.to[Index]+0.to[Index]) * c2_RF(inD_i.to[Index]*25 + 4*5+0)
              val prod21 = tmp1_SRAM(inD_i, r.to[Index]+4.to[Index],c.to[Index]+1.to[Index]) * c2_RF(inD_i.to[Index]*25 + 4*5+1)
              val prod22 = tmp1_SRAM(inD_i, r.to[Index]+4.to[Index],c.to[Index]+2.to[Index]) * c2_RF(inD_i.to[Index]*25 + 4*5+2)
              val prod23 = tmp1_SRAM(inD_i, r.to[Index]+4.to[Index],c.to[Index]+3.to[Index]) * c2_RF(inD_i.to[Index]*25 + 4*5+3)
              val prod24 = tmp1_SRAM(inD_i, r.to[Index]+4.to[Index],c.to[Index]+4.to[Index]) * c2_RF(inD_i.to[Index]*25 + 4*5+4)

              val tree_level_0_00 = prod00 + prod01
              val tree_level_0_01 = prod02 + prod03
              val tree_level_0_02 = prod04 + prod05
              val tree_level_0_03 = prod06 + prod07
              val tree_level_0_04 = prod08 + prod09
              val tree_level_0_05 = prod10 + prod11
              val tree_level_0_06 = prod12 + prod13
              val tree_level_0_07 = prod14 + prod15
              val tree_level_0_08 = prod16 + prod17
              val tree_level_0_09 = prod18 + prod19
              val tree_level_0_10 = prod20 + prod21
              val tree_level_0_11 = prod22 + prod23
              val tree_level_0_12 = prod24

              val tree_level_1_00 = tree_level_0_00 + tree_level_0_01
              val tree_level_1_01 = tree_level_0_02 + tree_level_0_03
              val tree_level_1_02 = tree_level_0_04 + tree_level_0_05
              val tree_level_1_03 = tree_level_0_06 + tree_level_0_07
              val tree_level_1_04 = tree_level_0_08 + tree_level_0_09
              val tree_level_1_05 = tree_level_0_10 + tree_level_0_11
              val tree_level_1_06 = tree_level_0_12

              val tree_level_2_00 = tree_level_1_00 + tree_level_1_01
              val tree_level_2_01 = tree_level_1_02 + tree_level_1_03
              val tree_level_2_02 = tree_level_1_04 + tree_level_1_05
              val tree_level_2_03 = tree_level_1_06
              
              val tree_level_3_00 = tree_level_2_00 + tree_level_2_01
              val tree_level_3_01 = tree_level_2_02 + tree_level_2_03
                
              result(r, c) = tree_level_3_00 + tree_level_3_01
            }
            result
          }{_+_} // Reduce across in channels

          // Fused BiasAdd
          Foreach(4 by 1, 4 by 1) { (i,j) =>
            val px0 = max(0.to[T], tmp2_SRAM_conv(i.to[Int]*2 + 0.to[Int], j.to[Int]*2 + 0.to[Int]) + c3_SRAM(outD_i))
            val px1 = max(0.to[T], tmp2_SRAM_conv(i.to[Int]*2 + 0.to[Int], j.to[Int]*2 + 1.to[Int]) + c3_SRAM(outD_i))
            val px2 = max(0.to[T], tmp2_SRAM_conv(i.to[Int]*2 + 1.to[Int], j.to[Int]*2 + 0.to[Int]) + c3_SRAM(outD_i))
            val px3 = max(0.to[T], tmp2_SRAM_conv(i.to[Int]*2 + 1.to[Int], j.to[Int]*2 + 1.to[Int]) + c3_SRAM(outD_i))

            val tree_level_0_0 = max(px0, px1)
            val tree_level_0_1 = max(px2, px3)

            tmp2_SRAM(outD_i, i, j) = max(tree_level_0_0, tree_level_0_1)
          }
        }
        // Optimization: BiasAdd was merged into Conv2D above
        // Optimization: ReLU was merged into Conv2D above
        // Optimization: MaxPool was merged into Conv2D above

        // Reshape
        val tmp3_SRAM = SRAM[T](4*4*50)
        Foreach(50 by 1, 4 by 1, 4 by 1) { (j,i,k) =>
          tmp3_SRAM(k.to[Int]*50 + i.to[Int]*4*50 + j.to[Int]) = tmp2_SRAM(j, i, k)
        }

        // MatMul
        val tmp4_SRAM = SRAM[T](500)
        Foreach(100 by 1 par 4){out_i =>
          val c4_row_SRAM = SRAM[T](4000)
          c4_row_SRAM load c4_DRAM(out_i, 0::4000 par 16)
          Foreach(5 by 1 par 1){block_i =>
            val prod = Reduce(Reg[T](0.to[T]))(800 by 1 par 1){ in_i =>             // DAVID TODO: Should be par 2, but causes banking error currently
              tmp3_SRAM(in_i) * c4_row_SRAM(block_i.to[Index]*800 + in_i.to[Index])
            }{_+_}
            tmp4_SRAM(out_i.to[Index]*5 + block_i.to[Index]) = max(0.to[T], prod.value + c5_SRAM(out_i.to[Index]*5 + block_i.to[Index]))
          }
        }
        // Optimization: BiasAdd was merged into MatMul above
        // Optimization: ReLU was merged into MatMul above

        // MatMul
        val tmp5_SRAM = SRAM[T](32)
        Foreach(10 by 1 par 1){out_i =>
          val c6_row_SRAM = SRAM[T](512)
          c6_row_SRAM load c6_DRAM(out_i, 0::512 par 1)
          val prod = Reduce(Reg[T](0.to[T]))(500 by 1 par 1){ in_i => tmp4_SRAM(in_i) * c6_row_SRAM(in_i) }{_+_}
          tmp5_SRAM(out_i) = prod.value + c7_SRAM(out_i)
        }
        // Optimization: BiasAdd was merged into MatMul above
        // Optimization: ReLU was merged into MatMul above

        tmp5_DRAM(batch_img, 0::32) store tmp5_SRAM
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

