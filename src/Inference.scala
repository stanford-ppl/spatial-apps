import spatial.dsl._
import virtualized._
import spatial.stdlib._
import spatial.targets._


object SingleLayerConv_RCIO extends SpatialApp {
  type T = FixPt[TRUE,_16,_0]

  @virtualize
  def main() {

    val debug:scala.Boolean = false

    val PX = 1
    val P1 = 1 // Unsafe parallelization if OC < 1 burst (16)
    val P2 = 1 // Unsafe parallelization if OC < 1 burst (16)
    val P3 = 1
    val P4 = 1
    val P5 = 1
    val loadPar = 32 (1 -> 16)
    val storePar = 32 (1 -> 16)
    // Scalar params
    val INPUT_ROWS = ArgIn[Int]
    val INPUT_COLS = ArgIn[Int]
    val INPUT_CHANS = ArgIn[Int]
    val OUTPUT_CHANS = ArgIn[Int]
    val STRIDE = ArgIn[Int] // Assume horiz and vert stride match
    val KERNEL_ROWS = 3
    val KERNEL_COLS = 3

    // Shadow params (input args)
    val input_rows = args(0).to[Int]
    val input_cols = args(1).to[Int]
    val input_chans = args(2).to[Int]
    val output_chans = args(3).to[Int]
    val stride = args(4).to[Int]
    val print_data = args(5).to[Bit]

    // Set args
    setArg(INPUT_ROWS, input_rows)
    setArg(INPUT_COLS, input_cols)
    setArg(INPUT_CHANS, input_chans)
    setArg(OUTPUT_CHANS, output_chans)
    setArg(STRIDE, stride)

    // HW Design properties
    val INPUT_COLS_MAX = 640
    val INPUT_CHANS_MAX = 64
    val OUTPUT_CHANS_MAX = 64

    // Memories
    val INPUT_DATA = DRAM[T](INPUT_ROWS, INPUT_COLS, INPUT_CHANS)
    val OUTPUT_DATA = DRAM[T]((INPUT_ROWS-(KERNEL_ROWS-STRIDE))/STRIDE, (INPUT_COLS-(KERNEL_COLS-STRIDE))/STRIDE, OUTPUT_CHANS)
    val KERNEL_DATA = DRAM[T](KERNEL_ROWS, KERNEL_COLS, INPUT_CHANS, OUTPUT_CHANS)
    val BIAS_DATA =   DRAM[T]((INPUT_ROWS-(KERNEL_ROWS-STRIDE))/STRIDE, (INPUT_COLS-(KERNEL_COLS-STRIDE))/STRIDE, OUTPUT_CHANS)

    // Load data (placeholder)
    val input = (0::INPUT_ROWS, 0::INPUT_COLS, 0::INPUT_CHANS) {(i,j,k) => ((i + j + k) % 8).to[T]}
    val output = (0::(INPUT_ROWS-(KERNEL_ROWS-STRIDE))/STRIDE, 0::(INPUT_COLS-(KERNEL_COLS-STRIDE))/STRIDE, 0::OUTPUT_CHANS){(i,j,k) => 0.to[T]}
    val kernel = (0::KERNEL_ROWS, 0::KERNEL_COLS, 0::INPUT_CHANS, 0::OUTPUT_CHANS) {(i,j,k,l) => if (random[Int](10) > 8) 1.to[T] else 0.to[T]}
    val bias =   (0::(INPUT_ROWS-(KERNEL_ROWS-STRIDE))/STRIDE, 0::(INPUT_COLS-(KERNEL_COLS-STRIDE))/STRIDE, 0::OUTPUT_CHANS){(i,j,k) => 1.to[T]}

    printTensor3(input, "Input")
    printTensor4(kernel, "Kernel")
    printTensor3(bias, "Bias")

    // Set data
    setMem(INPUT_DATA, input)
    setMem(OUTPUT_DATA, output)
    setMem(KERNEL_DATA, kernel)
    setMem(BIAS_DATA, bias)

    // setMem(KERNEL_COPY_CPU, kernel)
    // println("MANUALLY COPY KERNEL_DATA PTR TO ME!")

    Accel{
      val kernel_sram = SRAM[T](KERNEL_ROWS, KERNEL_COLS, INPUT_CHANS_MAX, OUTPUT_CHANS_MAX)
      kernel_sram load KERNEL_DATA(0::KERNEL_ROWS, 0::KERNEL_COLS, 0::INPUT_CHANS, 0::OUTPUT_CHANS)
      val rowspan = if (STRIDE.value == 1) INPUT_ROWS-(KERNEL_ROWS-STRIDE) else (INPUT_ROWS-(KERNEL_ROWS-STRIDE) >> 1)
      val colspan = if (STRIDE.value == 1) INPUT_COLS-(KERNEL_COLS-STRIDE) else (INPUT_COLS-(KERNEL_COLS-STRIDE) >> 1)
      Foreach(rowspan par P1){ R =>
        val row = R*2 
        Foreach(colspan par P2){ C =>
          val col = C*2
          val local_data = SRAM[T](3,3,INPUT_CHANS_MAX)
          val accum_line = SRAM.buffer[T](OUTPUT_CHANS_MAX)
          Parallel{
            local_data load INPUT_DATA(row::row+3, col::col+3, 0::INPUT_CHANS par loadPar)
            accum_line load BIAS_DATA(row/STRIDE, col/STRIDE, 0::OUTPUT_CHANS par loadPar)
          }
          MemFold((0 until OUTPUT_CHANS par P5), accum_line)(INPUT_CHANS by 1 par P3){ ic =>
            val local_accum_line = SRAM[T](OUTPUT_CHANS_MAX)
            Foreach(OUTPUT_CHANS by 1 par P4){ oc =>
              val filter_elements = List.tabulate(3){i => List.tabulate(3){j => 
                kernel_sram(i,j,ic,oc)
              }}.flatten
              val data_elements = List.tabulate(3){i => List.tabulate(3){j => 
                local_data(i,j,ic)
              }}.flatten
              val accum = data_elements.zip(filter_elements).map{case(a,b) => a*b}.reduce{_+_}
              local_accum_line(oc) = accum
            }
            local_accum_line
          }{_+_}
          OUTPUT_DATA(row/STRIDE,col/STRIDE,0::OUTPUT_CHANS par storePar) store accum_line
        }
      }
    }

    // Get results
    val results = getTensor3(OUTPUT_DATA)
    println("Results are " + results.dim0 + " x " + results.dim1 + " x " + results.dim2)

    // Compute Checks
    val gold = (0::(INPUT_ROWS-(KERNEL_ROWS-STRIDE)) / STRIDE, 0::(INPUT_COLS-(KERNEL_COLS-STRIDE)) / STRIDE, 0::OUTPUT_CHANS){(i,j,k) => 
      Array.tabulate(INPUT_CHANS){page => 
        if (debug && print_data) {
            println("Result working on " + k + "," + page + "," + i + "," + j)
            println(" Window: ")
            for (ii <- 0 until KERNEL_ROWS) { 
              for (jj <- 0 until KERNEL_COLS){
                print(" " + input(i*STRIDE+ii,j*STRIDE+jj, page))
              } 
              println(" ")
            }
        }


        Array.tabulate(KERNEL_ROWS){ii => Array.tabulate(KERNEL_COLS){jj => 
          val pxl = input(i*STRIDE+ii,j*STRIDE+jj, page)
          val f = kernel(ii, jj, page, k)
          if (debug && print_data) println(" Partial is " + pxl + " * " + f + " @ " + ii + "," + jj)
          pxl * f
        }}.flatten.reduce{_+_}
      }.reduce{_+_} + bias(i, j, k)
    }
  
    if (print_data) {
      printTensor3(input, "Input")
      printTensor4(kernel, "Kernel")
      printTensor3(gold, "Gold")
      printTensor3(results, "Extracted")  

      val bitmask = (0::(INPUT_ROWS-(KERNEL_ROWS-STRIDE)) / STRIDE, 0::(INPUT_COLS-(KERNEL_COLS-STRIDE)) / STRIDE, 0::OUTPUT_CHANS){(i,j,k) =>
        if (results(i,j, k) == gold(i,j, k)) 1.to[Int] else 0.to[Int]
      }
      val num_wrong = bitmask.length - bitmask.reduce{_+_}
      printTensor3(bitmask, "Matchup")
      println("Error rate: " + num_wrong + " / " + bitmask.length + " incorrect")

    }
    val cksum = gold.zip(results){_==_}.reduce{_&&_}    

    println("PASS: " + cksum + " (SingleLayerConv_design3)")

  }
}


object SingleLayerConv_IRCO extends SpatialApp {
  type T = FixPt[TRUE,_16,_0]

  @virtualize
  def main() {

    val debug:scala.Boolean = false

    val PX = 1
    val P1 = 1
    val P2 = 1
    val P3 = 1 // Not working
    val loadPar = 32 (1 -> 16)
    val storePar = 32 (1 -> 16)
    // Scalar params
    val INPUT_ROWS = ArgIn[Int]
    val INPUT_COLS = ArgIn[Int]
    val INPUT_CHANS = ArgIn[Int]
    val OUTPUT_CHANS = ArgIn[Int]
    val STRIDE = ArgIn[Int] // Assume horiz and vert stride match
    val KERNEL_ROWS = 3
    val KERNEL_COLS = 3

    // Shadow params (input args)
    val input_rows = args(0).to[Int]
    val input_cols = args(1).to[Int]
    val input_chans = args(2).to[Int]
    val output_chans = args(3).to[Int]
    val stride = args(4).to[Int]
    val print_data = args(5).to[Bit]

    // Set args
    setArg(INPUT_ROWS, input_rows)
    setArg(INPUT_COLS, input_cols)
    setArg(INPUT_CHANS, input_chans)
    setArg(OUTPUT_CHANS, output_chans)
    setArg(STRIDE, stride)

    // HW Design properties
    val INPUT_COLS_MAX = 640
    val INPUT_CHANS_MAX = 64
    val OUTPUT_CHANS_MAX = 64

    // Memories
    val INPUT_DATA = DRAM[T](INPUT_CHANS, INPUT_ROWS, INPUT_COLS)
    val OUTPUT_DATA = DRAM[T](OUTPUT_CHANS, INPUT_ROWS/STRIDE, INPUT_COLS/STRIDE)
    val KERNEL_DATA = DRAM[T](OUTPUT_CHANS, INPUT_CHANS, KERNEL_ROWS, KERNEL_COLS)
    val BIAS_DATA = DRAM[T](OUTPUT_CHANS, INPUT_ROWS/STRIDE, INPUT_COLS/STRIDE)

    // Load data (placeholder)
    val input = (0::INPUT_CHANS, 0::INPUT_ROWS, 0::INPUT_COLS) {(i,j,k) => ((i + j + k) % 8).to[T]}
    val output = (0::OUTPUT_CHANS, 0::INPUT_ROWS/STRIDE, 0::INPUT_COLS/STRIDE){(i,j,k) => 0.to[T]}
    val kernel = (0::OUTPUT_CHANS, 0::INPUT_CHANS, 0::KERNEL_ROWS, 0::KERNEL_COLS) {(i,j,k,l) => if (random[Int](10) > 8) 1.to[T] else 0.to[T]}
    val bias = (0::OUTPUT_CHANS, 0::INPUT_ROWS/STRIDE, 0::INPUT_COLS/STRIDE){(i,j,k) => 1.to[T]}

    // Set data
    setMem(INPUT_DATA, input)
    setMem(OUTPUT_DATA, output)
    setMem(KERNEL_DATA, kernel)
    setMem(BIAS_DATA, bias)

    // setMem(KERNEL_COPY_CPU, kernel)
    // println("MANUALLY COPY KERNEL_DATA PTR TO ME!")

    Accel{
      val kernel_sram = SRAM[T](OUTPUT_CHANS_MAX, INPUT_CHANS_MAX, KERNEL_ROWS, KERNEL_COLS)
      kernel_sram load KERNEL_DATA(0::OUTPUT_CHANS, 0::INPUT_CHANS, 0::KERNEL_ROWS, 0::KERNEL_COLS)
      Foreach(INPUT_CHANS by 1){ ic => 
        val lb1 = LineBuffer.strided[T](KERNEL_ROWS, INPUT_COLS_MAX, 1)
        val lb2 = LineBuffer.strided[T](KERNEL_ROWS, INPUT_COLS_MAX, 2)
        Foreach(INPUT_ROWS by STRIDE){ row => 
          // val bias_srams = SRAM[T](OUTPUT_CHANS_MAX, INPUT_COLS_MAX) // / STRIDE?
          val accum_lines = SRAM.buffer[T](OUTPUT_CHANS_MAX, INPUT_COLS_MAX)
          Parallel {
            Foreach(OUTPUT_CHANS by 1 par P3) {oc => 
              if (ic == 0) accum_lines.loadOrigin(BIAS_DATA(oc, row/STRIDE::(row/STRIDE)+1, 0::INPUT_COLS/STRIDE par loadPar), (oc,0))
              else accum_lines.loadOrigin(OUTPUT_DATA(oc, row/STRIDE::(row/STRIDE)+1, 0::INPUT_COLS/STRIDE par loadPar), (oc,0))
            }
            Pipe{
              if (STRIDE.value == 1) lb1 load INPUT_DATA(ic, row,0::INPUT_COLS par loadPar)
              else lb2 load INPUT_DATA(ic, row::row+2,0::INPUT_COLS par loadPar)
            }
          }
          Foreach(INPUT_COLS by STRIDE par PX){ col =>
            val sr1 = RegFile[T](KERNEL_ROWS,KERNEL_COLS)
            val sr2 = RegFile[T](KERNEL_ROWS,KERNEL_COLS)
            if (STRIDE.value == 1) Foreach(3 by 1 par 3){i => sr1(i,*) <<= lb1(i, col)}
            else Foreach(3 by 1 par 3){i => sr2(i,*) <<= lb2(i, col::col+2)}
            val data_elements = List.tabulate(3){i => List.tabulate(3){j => 
              if (row.to[Int] + STRIDE.value - KERNEL_ROWS + i.to[Int] < 0 
                  || col.to[Int] + STRIDE.value - KERNEL_COLS + j.to[Int] < 0 ) 
                       {0.to[T]}
              else 
                if (STRIDE.value == 1) {sr1(i,KERNEL_COLS - 1 - j)} 
                else {sr2(i,KERNEL_COLS - 1 - j)}
            }}.flatten

            Foreach(OUTPUT_CHANS by 1 par P1) { oc =>
              val filter_elements = List.tabulate(3){i => List.tabulate(3){j => 
                kernel_sram(oc,ic,i,j)
              }}.flatten

              val accum = data_elements.zip(filter_elements).map{case(a,b) => a*b}.reduce{_+_}

              accum_lines(oc, col/STRIDE) = accum + accum_lines(oc, col/STRIDE)
              if (debug) println(" = " + accum_lines(oc, col/STRIDE))

            }
            // val accum = Reduce(Reg[T](0))(3 by 1, 3 by 1 par P2){(i,j) => 
            //   val data = if (    row.to[Int] + STRIDE.value - KERNEL_ROWS + i.to[Int] < 0 
            //                   || col.to[Int] + STRIDE.value - KERNEL_COLS + j.to[Int] < 0 ) 
            //                {0.to[T]}
            //              else 
            //                 if (STRIDE.value == 1) {sr1(i,KERNEL_COLS - 1 - j)} 
            //                 else {sr2(i,KERNEL_COLS - 1 - j)}
            //     if (debug) println(" Partial is " + data + " * " + kernel_sram(oc, ic, i, j) + " @ " + i + "," + j)
            //     val k = kernel_sram(oc, ic, i, j)
            //     data * k
            //   }{_+_}
          }

          // Store line back
          Foreach(OUTPUT_CHANS by 1 par P2) { oc =>
            OUTPUT_DATA(oc, row/STRIDE::(row/STRIDE)+1, 0::INPUT_COLS/STRIDE par storePar).storeOrigin(accum_lines, (oc,0))
          }
        }
      }
    }

    // Get results
    val results = getTensor3(OUTPUT_DATA)
    println("Results are " + results.dim0 + " x " + results.dim1 + " x " + results.dim2)

    // Compute Checks
    val gold = (0::OUTPUT_CHANS, 0::INPUT_ROWS by STRIDE, 0::INPUT_COLS by STRIDE){(k,i,j) => 
      Array.tabulate(INPUT_CHANS){page => 
        if (debug && print_data) {
            println("Result working on " + k + "," + page + "," + i/STRIDE + "," + j/STRIDE)
            println(" Window: ")
            for (ii <- 0 until KERNEL_ROWS) { 
              for (jj <- 0 until KERNEL_COLS){
                if (i-KERNEL_ROWS+STRIDE+ii < 0 || j-KERNEL_COLS+STRIDE+jj < 0) print(" X") 
                else print(" " + input(page,i-(KERNEL_ROWS-STRIDE-ii),j-(KERNEL_COLS-STRIDE-jj)))
              } 
              println(" ")
            }
        }


        Array.tabulate(KERNEL_ROWS){ii => Array.tabulate(KERNEL_COLS){jj => 
          val pxl = if (i-KERNEL_ROWS+STRIDE+ii < 0 || j-KERNEL_COLS+STRIDE+jj < 0) 0.to[T] 
                    else input(page,i-(KERNEL_ROWS-STRIDE-ii),j-(KERNEL_COLS-STRIDE-jj))
          val f = kernel(k,page, ii, jj)
          if (debug) println(" Partial is " + pxl + " * " + f + " @ " + ii + "," + jj)
          pxl * f
        }}.flatten.reduce{_+_}
      }.reduce{_+_} + bias(k, i/STRIDE, j/STRIDE)
    }
  
    if (print_data) {
      printTensor3(input, "Input")
      printTensor4(kernel, "Kernel")
      printTensor3(gold, "Gold")
      printTensor3(results, "Extracted")  

      val bitmask = (0::OUTPUT_CHANS, 0::INPUT_ROWS by STRIDE, 0::INPUT_COLS by STRIDE){(k,i,j) =>
        if (results(k,i/STRIDE,j/STRIDE) == gold(k,i/STRIDE,j/STRIDE)) 1.to[Int] else 0.to[Int]
      }
      val num_wrong = bitmask.length - bitmask.reduce{_+_}
      printTensor3(bitmask, "Matchup")
      println("Error rate: " + num_wrong + " / " + bitmask.length + " incorrect")

    }
    val cksum = gold.zip(results){_==_}.reduce{_&&_}    

    println("PASS: " + cksum + " (SingleLayerConv_design2)")

  }
}


object SingleLayerConv_OIRC extends SpatialApp {
  type T = FixPt[TRUE,_16,_0]

  @virtualize
  def main() {

    val debug:scala.Boolean = false

    val PX = 1
    val loadPar = 16 (1 -> 16)
    val storePar = 16 (1 -> 16)
    // Scalar params
    val INPUT_ROWS = ArgIn[Int]
    val INPUT_COLS = ArgIn[Int]
    val INPUT_CHANS = ArgIn[Int]
    val OUTPUT_CHANS = ArgIn[Int]
    val STRIDE = ArgIn[Int] // Assume horiz and vert stride match
    val KERNEL_ROWS = 3
    val KERNEL_COLS = 3

    // Shadow params (input args)
    val input_rows = args(0).to[Int]
    val input_cols = args(1).to[Int]
    val input_chans = args(2).to[Int]
    val output_chans = args(3).to[Int]
    val stride = args(4).to[Int]
    val print_data = args(5).to[Bit]

    // Set args
    setArg(INPUT_ROWS, input_rows)
    setArg(INPUT_COLS, input_cols)
    setArg(INPUT_CHANS, input_chans)
    setArg(OUTPUT_CHANS, output_chans)
    setArg(STRIDE, stride)

    // HW Design properties
    val INPUT_COLS_MAX = 640
    val INPUT_CHANS_MAX = 64
    val OUTPUT_CHANS_MAX = 64

    // Memories
    val INPUT_DATA = DRAM[T](INPUT_CHANS, INPUT_ROWS, INPUT_COLS)
    val OUTPUT_DATA = DRAM[T](OUTPUT_CHANS, INPUT_ROWS/STRIDE, INPUT_COLS/STRIDE)
    val KERNEL_DATA = DRAM[T](OUTPUT_CHANS, INPUT_CHANS, KERNEL_ROWS, KERNEL_COLS)
    val BIAS_DATA = DRAM[T](OUTPUT_CHANS, INPUT_ROWS/STRIDE, INPUT_COLS/STRIDE)

    // Load data (placeholder)
    val input = (0::INPUT_CHANS, 0::INPUT_ROWS, 0::INPUT_COLS) {(i,j,k) => ((i + j + k) % 8).to[T]}
    val output = (0::OUTPUT_CHANS, 0::INPUT_ROWS/STRIDE, 0::INPUT_COLS/STRIDE){(i,j,k) => 0.to[T]}
    val kernel = (0::OUTPUT_CHANS, 0::INPUT_CHANS, 0::KERNEL_ROWS, 0::KERNEL_COLS) {(i,j,k,l) => if (random[Int](10) > 8) 1.to[T] else 0.to[T]}
    val bias = (0::OUTPUT_CHANS, 0::INPUT_ROWS/STRIDE, 0::INPUT_COLS/STRIDE){(i,j,k) => 1.to[T]}

    // Set data
    setMem(INPUT_DATA, input)
    setMem(OUTPUT_DATA, output)
    setMem(KERNEL_DATA, kernel)
    setMem(BIAS_DATA, bias)

    // setMem(KERNEL_COPY_CPU, kernel)
    // println("MANUALLY COPY KERNEL_DATA PTR TO ME!")

    Accel{
      val kernel_sram = SRAM[T](OUTPUT_CHANS_MAX, INPUT_CHANS_MAX, KERNEL_ROWS, KERNEL_COLS)
      kernel_sram load KERNEL_DATA(0::OUTPUT_CHANS, 0::INPUT_CHANS, 0::KERNEL_ROWS, 0::KERNEL_COLS)

      Foreach(OUTPUT_CHANS by 1){ oc => 
        Foreach(INPUT_CHANS by 1){ ic => 
          val lb1 = LineBuffer.strided[T](KERNEL_ROWS, INPUT_COLS_MAX, 1)
          val lb2 = LineBuffer.strided[T](KERNEL_ROWS, INPUT_COLS_MAX, 2)
          Foreach(INPUT_ROWS by STRIDE){ row => 
            val bias_sram = SRAM[T](INPUT_COLS_MAX) // / STRIDE?
            val accum_line = SRAM.buffer[T](INPUT_COLS_MAX)
            if (ic == 0) bias_sram load BIAS_DATA(oc, row/STRIDE, 0::INPUT_COLS/STRIDE par loadPar)
            // Parallel{
              Pipe{accum_line load OUTPUT_DATA(oc, row/STRIDE, 0::INPUT_COLS/STRIDE par loadPar)}
              Pipe{
                if (STRIDE.value == 1) lb1 load INPUT_DATA(ic, row,0::INPUT_COLS par loadPar)
                else lb2 load INPUT_DATA(ic, row::row+2,0::INPUT_COLS par loadPar)
              }
            // }
            Foreach(INPUT_COLS by STRIDE par PX){ col =>
              val sr1 = RegFile[T](KERNEL_ROWS,KERNEL_COLS)
              val sr2 = RegFile[T](KERNEL_ROWS,KERNEL_COLS)
              if (STRIDE.value == 1) Foreach(3 by 1 par 3){i => sr1(i,*) <<= lb1(i, col)}
              else Foreach(3 by 1 par 3){i => sr2(i,*) <<= lb2(i, col::col+2)}
              val filter_elements = List.tabulate(3){i => List.tabulate(3){j => 
                kernel_sram(oc,ic,i,j)
              }}.flatten
              val data_elements = List.tabulate(3){i => List.tabulate(3){j => 
                if (row.to[Int] + STRIDE.value - KERNEL_ROWS + i.to[Int] < 0 
                    || col.to[Int] + STRIDE.value - KERNEL_COLS + j.to[Int] < 0 ) 
                         {0.to[T]}
                else 
                  if (STRIDE.value == 1) {sr1(i,KERNEL_COLS - 1 - j)} 
                  else {sr2(i,KERNEL_COLS - 1 - j)}
              }}.flatten

              val accum = data_elements.zip(filter_elements).map{case(a,b) => a*b}.reduce{_+_}
              accum_line(col/STRIDE) = accum + accum_line(col/STRIDE) + mux(ic == 0, bias_sram(col/STRIDE), 0.to[T])
              if (debug) println(" = " + accum_line(col/STRIDE) + ", bias = " + mux(ic == 0, bias_sram(col/STRIDE), 0.to[T]))
            }

            // Store line back
            OUTPUT_DATA(oc, row/STRIDE, 0::INPUT_COLS/STRIDE par storePar) store accum_line
          }
        }
      }
    }

    // Get results
    val results = getTensor3(OUTPUT_DATA)
    println("Results are " + results.dim0 + " x " + results.dim1 + " x " + results.dim2)

    // Compute Checks
    val gold = (0::OUTPUT_CHANS, 0::INPUT_ROWS by STRIDE, 0::INPUT_COLS by STRIDE){(k,i,j) => 
      Array.tabulate(INPUT_CHANS){page => 
        if (debug && print_data) {
            println("Result working on " + k + "," + page + "," + i/STRIDE + "," + j/STRIDE)
            println(" Window: ")
            for (ii <- 0 until KERNEL_ROWS) { 
              for (jj <- 0 until KERNEL_COLS){
                if (i-KERNEL_ROWS+STRIDE+ii < 0 || j-KERNEL_COLS+STRIDE+jj < 0) print(" X") 
                else print(" " + input(page,i-(KERNEL_ROWS-STRIDE-ii),j-(KERNEL_COLS-STRIDE-jj)))
              } 
              println(" ")
            }
        }


        Array.tabulate(KERNEL_ROWS){ii => Array.tabulate(KERNEL_COLS){jj => 
          val pxl = if (i-KERNEL_ROWS+STRIDE+ii < 0 || j-KERNEL_COLS+STRIDE+jj < 0) 0.to[T] 
                    else input(page,i-(KERNEL_ROWS-STRIDE-ii),j-(KERNEL_COLS-STRIDE-jj))
          val f = kernel(k,page, ii, jj)
          if (debug) println(" Partial is " + pxl + " * " + f + " @ " + ii + "," + jj)
          pxl * f
        }}.flatten.reduce{_+_}
      }.reduce{_+_} + bias(k, i/STRIDE, j/STRIDE)
    }
  
    if (print_data) {
      printTensor3(input, "Input")
      printTensor4(kernel, "Kernel")
      printTensor3(gold, "Gold")
      printTensor3(results, "Extracted")  

      val bitmask = (0::OUTPUT_CHANS, 0::INPUT_ROWS by STRIDE, 0::INPUT_COLS by STRIDE){(k,i,j) =>
        if (results(k,i/STRIDE,j/STRIDE) == gold(k,i/STRIDE,j/STRIDE)) 1.to[Int] else 0.to[Int]
      }
      val num_wrong = bitmask.length - bitmask.reduce{_+_}
      printTensor3(bitmask, "Matchup")
      println("Error rate: " + num_wrong + " / " + bitmask.length + " incorrect")

    }
    val cksum = gold.zip(results){_==_}.reduce{_&&_}    

    println("PASS: " + cksum + " (SingleLayerConv_design1)")

  }
}

object SingleLayerConv extends SpatialApp {
  type T = FixPt[TRUE,_16,_0]

  @virtualize
  def main() {

    val debug:scala.Boolean = false

    val PX = 1
    val loadPar = 32 (1 -> 16)
    val storePar = 32 (1 -> 16)
    val P1 = 2 (1 -> 16)
    val rowJump = 50 (10 -> 200)

    // Scalar params
    val INPUT_ROWS = ArgIn[Int]
    val INPUT_COLS = ArgIn[Int]
    val INPUT_CHANS = ArgIn[Int]
    val OUTPUT_CHANS = ArgIn[Int]
    val STRIDE = ArgIn[Int] // Assume horiz and vert stride match
    val KERNEL_ROWS = 3
    val KERNEL_COLS = 3

    // Shadow params (input args)
    val input_rows = args(0).to[Int]
    val input_cols = args(1).to[Int]
    val input_chans = args(2).to[Int]
    val output_chans = args(3).to[Int]
    val stride = args(4).to[Int]
    val print_data = args(5).to[Bit]

    // Set args
    setArg(INPUT_ROWS, input_rows)
    setArg(INPUT_COLS, input_cols)
    setArg(INPUT_CHANS, input_chans)
    setArg(OUTPUT_CHANS, output_chans)
    setArg(STRIDE, stride)

    // HW Design properties
    val INPUT_COLS_MAX = 640
    val INPUT_CHANS_MAX = 64
    val OUTPUT_CHANS_MAX = 64

    // Memories
    val INPUT_DATA = DRAM[T](INPUT_CHANS, INPUT_ROWS, INPUT_COLS)
    val OUTPUT_DATA = DRAM[T](OUTPUT_CHANS, INPUT_ROWS/STRIDE, INPUT_COLS/STRIDE)
    val KERNEL_DATA = DRAM[T](OUTPUT_CHANS, INPUT_CHANS, KERNEL_ROWS, KERNEL_COLS)
    val BIAS_DATA = DRAM[T](OUTPUT_CHANS, INPUT_ROWS/STRIDE, INPUT_COLS/STRIDE)

    // Load data (placeholder)
    val input = (0::INPUT_CHANS, 0::INPUT_ROWS, 0::INPUT_COLS) {(i,j,k) => ((i + j + k) % 8).to[T]}
    val output = (0::OUTPUT_CHANS, 0::INPUT_ROWS/STRIDE, 0::INPUT_COLS/STRIDE){(i,j,k) => 0.to[T]}
    val kernel = (0::OUTPUT_CHANS, 0::INPUT_CHANS, 0::KERNEL_ROWS, 0::KERNEL_COLS) {(i,j,k,l) => if (random[Int](10) > 8) 1.to[T] else 0.to[T]}
    val bias = (0::OUTPUT_CHANS, 0::INPUT_ROWS/STRIDE, 0::INPUT_COLS/STRIDE){(i,j,k) => 1.to[T]}

    // Set data
    setMem(INPUT_DATA, input)
    setMem(OUTPUT_DATA, output)
    setMem(KERNEL_DATA, kernel)
    setMem(BIAS_DATA, bias)

    // setMem(KERNEL_COPY_CPU, kernel)
    // println("MANUALLY COPY KERNEL_DATA PTR TO ME!")

    Accel{
      val kernel_sram = SRAM[T](OUTPUT_CHANS_MAX, INPUT_CHANS_MAX, KERNEL_ROWS, KERNEL_COLS)
      kernel_sram load KERNEL_DATA(0::OUTPUT_CHANS, 0::INPUT_CHANS, 0::KERNEL_ROWS, 0::KERNEL_COLS)

      Foreach(OUTPUT_CHANS by 1){ oc => 
        Foreach(INPUT_CHANS by 1){ ic =>           
          Foreach(INPUT_ROWS by rowJump par P1){ rr => 
            val lb1 = LineBuffer.strided[T](KERNEL_ROWS, INPUT_COLS_MAX, 1)
            val lb2 = LineBuffer.strided[T](KERNEL_ROWS, INPUT_COLS_MAX, 2)
            val rows_todo = min(rowJump, INPUT_ROWS - rr)
            val bias_sram = SRAM[T](INPUT_COLS_MAX) // / STRIDE?
            val accum_line = SRAM.buffer[T](INPUT_COLS_MAX)
            Foreach(-2 until rows_todo by STRIDE par PX) { r => 
              val ldrow = if ((r.to[Index]+rr.to[Index]) < 0.to[Index] || (r.to[Index]+rr.to[Index]) > INPUT_ROWS.value) 0.to[Index] else {r.to[Index]+rr.to[Index]} 
              if (ic == 0) bias_sram load BIAS_DATA(oc, ldrow/STRIDE, 0::INPUT_COLS/STRIDE par loadPar)
              // Parallel{
                Pipe{accum_line load OUTPUT_DATA(oc, ldrow/STRIDE, 0::INPUT_COLS/STRIDE par loadPar)}
                Pipe{
                  if (STRIDE.value == 1) lb1 load INPUT_DATA(ic, ldrow,0::INPUT_COLS par loadPar)
                  else lb2 load INPUT_DATA(ic, ldrow::ldrow+2,0::INPUT_COLS par loadPar)
                }
              // }
              Foreach(INPUT_COLS by STRIDE par PX){ col =>
                val sr1 = RegFile[T](KERNEL_ROWS,KERNEL_COLS)
                val sr2 = RegFile[T](KERNEL_ROWS,KERNEL_COLS)
                if (STRIDE.value == 1) Foreach(3 by 1 par 3){i => sr1(i,*) <<= lb1(i, col)}
                else Foreach(3 by 1 par 3){i => sr2(i,*) <<= lb2(i, col::col+2)}
                val filter_elements = List.tabulate(3){i => List.tabulate(3){j => 
                  kernel_sram(oc,ic,i,j)
                }}.flatten
                val data_elements = List.tabulate(3){i => List.tabulate(3){j => 
                  if (ldrow.to[Int] + STRIDE.value - KERNEL_ROWS + i.to[Int] < 0 
                      || col.to[Int] + STRIDE.value - KERNEL_COLS + j.to[Int] < 0 ) 
                           {0.to[T]}
                  else 
                    if (STRIDE.value == 1) {sr1(i,KERNEL_COLS - 1 - j)} 
                    else {sr2(i,KERNEL_COLS - 1 - j)}
                }}.flatten

                val accum = data_elements.zip(filter_elements).map{case(a,b) => a*b}.reduce{_+_}
                accum_line(col/STRIDE) = accum + accum_line(col/STRIDE) + mux(ic == 0, bias_sram(col/STRIDE), 0.to[T])
                if (debug) println(" = " + accum_line(col/STRIDE) + ", bias = " + mux(ic == 0, bias_sram(col/STRIDE), 0.to[T]))
              }
              // Store line back
              if (r.to[Index]+rr.to[Index] < INPUT_ROWS && r.to[Index] >= 0.to[Index]) OUTPUT_DATA(oc, ldrow/STRIDE, 0::INPUT_COLS/STRIDE par storePar) store accum_line
            }
          }
        }
      }
    }

    // Get results
    val results = getTensor3(OUTPUT_DATA)
    println("Results are " + results.dim0 + " x " + results.dim1 + " x " + results.dim2)

    // Compute Checks
    val gold = (0::OUTPUT_CHANS, 0::INPUT_ROWS by STRIDE, 0::INPUT_COLS by STRIDE){(k,i,j) => 
      Array.tabulate(INPUT_CHANS){page => 
        if (debug && print_data) {
            println("Result working on " + k + "," + page + "," + i/STRIDE + "," + j/STRIDE)
            println(" Window: ")
            for (ii <- 0 until KERNEL_ROWS) { 
              for (jj <- 0 until KERNEL_COLS){
                if (i-KERNEL_ROWS+STRIDE+ii < 0 || j-KERNEL_COLS+STRIDE+jj < 0) print(" X") 
                else print(" " + input(page,i-(KERNEL_ROWS-STRIDE-ii),j-(KERNEL_COLS-STRIDE-jj)))
              } 
              println(" ")
            }
        }


        Array.tabulate(KERNEL_ROWS){ii => Array.tabulate(KERNEL_COLS){jj => 
          val pxl = if (i-KERNEL_ROWS+STRIDE+ii < 0 || j-KERNEL_COLS+STRIDE+jj < 0) 0.to[T] 
                    else input(page,i-(KERNEL_ROWS-STRIDE-ii),j-(KERNEL_COLS-STRIDE-jj))
          val f = kernel(k,page, ii, jj)
          if (debug) println(" Partial is " + pxl + " * " + f + " @ " + ii + "," + jj)
          pxl * f
        }}.flatten.reduce{_+_}
      }.reduce{_+_} + bias(k, i/STRIDE, j/STRIDE)
    }
  
    if (print_data) {
      printTensor3(input, "Input")
      printTensor4(kernel, "Kernel")
      printTensor3(gold, "Gold")
      printTensor3(results, "Extracted")  

      val bitmask = (0::OUTPUT_CHANS, 0::INPUT_ROWS by STRIDE, 0::INPUT_COLS by STRIDE){(k,i,j) =>
        if (results(k,i/STRIDE,j/STRIDE) == gold(k,i/STRIDE,j/STRIDE)) 1.to[Int] else 0.to[Int]
      }
      val num_wrong = bitmask.length - bitmask.reduce{_+_}
      printTensor3(bitmask, "Matchup")
      println("Error rate: " + num_wrong + " / " + bitmask.length + " incorrect")

    }
    val cksum = gold.zip(results){_==_}.reduce{_&&_}    

    println("PASS: " + cksum + " (pilot)")

  }
}

object test extends SpatialApp { // Regression (Unit) // Args: 100


  val N = 192

  @virtualize
  def simpleLoadStore[T:Type:Num](srcHost: Array[T], value: T) = {
    val loadPar  = 1 (1 -> 1)
    val storePar = 1 (1 -> 1)
    val tileSize = 16 (16 -> 16)

    val srcFPGA = DRAM[T](N)
    val dstFPGA = DRAM[T](N)
    setMem(srcFPGA, srcHost)

    val x = ArgIn[T]
    setArg(x, value)
    Accel {
      Sequential.Foreach(N by tileSize par 1) { i =>
        val b1 = SRAM[T](tileSize*2)

        b1 load srcFPGA(i::i+tileSize par 1)

        val b2 = SRAM[T](tileSize*2)
        Foreach(tileSize by 1 par 4) { ii =>
          b2(ii) = b1(ii) * x
        }

        dstFPGA(i::i+tileSize par 1) store b2
      }
    }
    getMem(dstFPGA)
  }

  @virtualize
  def main() {
    val arraySize = N
    val value = args(0).to[Int]

    val src = Array.tabulate[Int](arraySize) { i => i % 256 }
    val dst = simpleLoadStore(src, value)

    val gold = src.map { _ * value }

    println("Sent in: ")
    (0 until arraySize) foreach { i => print(gold(i) + " ") }
    println("Got out: ")
    (0 until arraySize) foreach { i => print(dst(i) + " ") }
    println("")

    val cksum = dst.zip(gold){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (SimpleTileLoadStore)")
  }
}



object SingleLayerConv_MemDebug extends SpatialApp {
  type T = FixPt[TRUE,_16,_0]

  @virtualize
  def main() {

    val debug:scala.Boolean = false

    // Scalar params
    val INPUT_ROWS = ArgIn[Int]
    val INPUT_COLS = ArgIn[Int]
    val INPUT_CHANS = ArgIn[Int]
    val OUTPUT_CHANS = ArgIn[Int]
    val STRIDE = ArgIn[Int] // Assume horiz and vert stride match
    val KERNEL_ROWS = 3
    val KERNEL_COLS = 3

    // Shadow params (input args)
    val input_rows = args(0).to[Int]
    val input_cols = args(1).to[Int]
    val input_chans = args(2).to[Int]
    val output_chans = args(3).to[Int]
    val stride = args(4).to[Int]
    val print_data = args(5).to[Bit]

    // Set args
    setArg(INPUT_ROWS, input_rows)
    setArg(INPUT_COLS, input_cols)
    setArg(INPUT_CHANS, input_chans)
    setArg(OUTPUT_CHANS, output_chans)
    setArg(STRIDE, stride)

    // HW Design properties
    val INPUT_COLS_MAX = 480
    val INPUT_CHANS_MAX = 64
    val OUTPUT_CHANS_MAX = 64

    // Memories
    val INPUT_DATA = DRAM[T](INPUT_CHANS, INPUT_ROWS, INPUT_COLS)
    val OUTPUT_DATA = DRAM[T](OUTPUT_CHANS, INPUT_CHANS, INPUT_ROWS/STRIDE, INPUT_COLS/STRIDE)
    val KERNEL_DATA = DRAM[T](OUTPUT_CHANS, INPUT_CHANS, KERNEL_ROWS, KERNEL_COLS)
    val BIAS_DATA = DRAM[T](OUTPUT_CHANS, INPUT_ROWS/STRIDE, INPUT_COLS/STRIDE)
    val INPUT_CHECK = DRAM[T](INPUT_CHANS, INPUT_ROWS, INPUT_COLS)

    // Load data (placeholder)
    val input = (0::INPUT_CHANS, 0::INPUT_ROWS, 0::INPUT_COLS) {(i,j,k) => ((i + j + k) % 8).to[T]}
    val output = (0::OUTPUT_CHANS, 0::INPUT_CHANS, 0::INPUT_ROWS/STRIDE, 0::INPUT_COLS/STRIDE){(i,j,k,l) => -1.to[T]}
    val kernel = (0::OUTPUT_CHANS, 0::INPUT_CHANS, 0::KERNEL_ROWS, 0::KERNEL_COLS) {(i,j,k,l) => if (random[Int](10) > 8) 1.to[T] else 0.to[T]}
    val bias = (0::OUTPUT_CHANS, 0::INPUT_ROWS/STRIDE, 0::INPUT_COLS/STRIDE){(i,j,k) => 1.to[T]}

    // Set data
    setMem(INPUT_DATA, input)
    setMem(OUTPUT_DATA, output)
    setMem(KERNEL_DATA, kernel)
    setMem(BIAS_DATA, bias)

    Accel{
      val kernel_sram = SRAM[T](OUTPUT_CHANS_MAX, INPUT_CHANS_MAX, KERNEL_ROWS, KERNEL_COLS)
      kernel_sram load KERNEL_DATA

      Foreach(OUTPUT_CHANS by 1){ oc => 
        Foreach(INPUT_CHANS by 1){ ic => 
          // val lb1 = LineBuffer.strided[T](KERNEL_ROWS, INPUT_COLS_MAX, 1)
          // val lb2 = LineBuffer.strided[T](KERNEL_ROWS, INPUT_COLS_MAX, 2)
          Foreach(INPUT_ROWS by STRIDE){ row => 
            // val bias_sram = SRAM[T](INPUT_COLS_MAX) // / STRIDE?
            val accum_line = SRAM.buffer[T](INPUT_COLS_MAX)
            val input_line = SRAM.buffer[T](INPUT_COLS_MAX)
            // if (ic == 0) bias_sram load BIAS_DATA(oc, row/STRIDE, 0::INPUT_COLS/STRIDE par 16)
            // // Parallel{
              Pipe{accum_line load OUTPUT_DATA(oc, ic, row/STRIDE, 0::INPUT_COLS/STRIDE par 16)}
              Pipe{input_line load INPUT_DATA(ic, row, 0::INPUT_COLS par 16)}
            //   Pipe{
            //     if (STRIDE.value == 1) lb1 load INPUT_DATA(ic, row,0::INPUT_COLS par 16)
            //     else lb2 load INPUT_DATA(ic, row::row+2,0::INPUT_COLS par 16)
            //   }
            // }
            Foreach(INPUT_COLS by STRIDE){ col =>
              // val sr1 = RegFile[T](KERNEL_ROWS,KERNEL_COLS)
              // val sr2 = RegFile[T](KERNEL_ROWS,KERNEL_COLS)
              // // if (STRIDE.value == 1) Foreach(3 by 1 par 3){i => sr1(i,*) <<= lb1(i, col)}
              // // else Foreach(3 by 1 par 3){i => sr2(i,*) <<= lb2(i, col::col+2)}
              // if (debug) {
              //     println("Working on " + oc + "," + ic + "," + row/STRIDE + "," + col/STRIDE )
              //     println(" Window:\\n")
              //     Foreach(3 by 1){i => 
              //       Foreach(3 by 1){j => 
              //           if (    row.to[Int] + STRIDE.value - KERNEL_ROWS + i.to[Int] < 0 
              //               || col.to[Int] + STRIDE.value - KERNEL_COLS + j.to[Int] < 0 ) 
              //                {print(" X")}
              //              else 
              //                if (STRIDE.value == 1) {print(" " + sr1(i,KERNEL_COLS - 1 - j))}
              //                else {print(" " + sr2(i,KERNEL_COLS - 1 - j))}
              //            } 
              //       println(" ")
              //     }
              // }
              val id = row/STRIDE + ic * INPUT_ROWS/STRIDE + oc * INPUT_CHANS * INPUT_ROWS/STRIDE
              Foreach(INPUT_COLS_MAX by 1){k => accum_line(k) = id.to[T]}

            }
            // Store line back
            OUTPUT_DATA(oc, ic, row/STRIDE, 0::INPUT_COLS/STRIDE par 16) store accum_line
            INPUT_CHECK(ic, row, 0::INPUT_COLS par 16) store input_line
          }
        }
      }
    }

    // Get results
    val results = getTensor4(OUTPUT_DATA)
    println("Results are " + results.dim0 + " x " + results.dim1 + " x " + results.dim2)
    val input_check = getTensor3(INPUT_CHECK)

    // Compute Checks
    val gold = (0::OUTPUT_CHANS, 0::INPUT_CHANS, 0::INPUT_ROWS/STRIDE, 0::INPUT_COLS/STRIDE){(i,j,k,l) => (k + j*INPUT_ROWS/STRIDE + i*INPUT_CHANS*INPUT_ROWS/STRIDE).to[T]}// output

    if (print_data) {
      printTensor3(input, "Input")
      printTensor3(input_check, "Input Check")
      printTensor4(kernel, "Kernel")
      printTensor4(gold, "Gold")
      printTensor4(results, "Extracted")  

      val bitmask = (0::OUTPUT_CHANS, 0::INPUT_CHANS, 0::INPUT_ROWS by STRIDE, 0::INPUT_COLS by STRIDE){(k,m, i,j) =>
        if (results(k,m,i/STRIDE,j/STRIDE) == gold(k,m,i/STRIDE,j/STRIDE)) 1.to[Int] else 0.to[Int]
      }
      val num_wrong = bitmask.length - bitmask.reduce{_+_}
      printTensor4(bitmask, "Matchup")
      println("Error rate: " + num_wrong + " / " + bitmask.length + " incorrect")

    }
    val cksum = gold.zip(results){_==_}.reduce{_&&_}  
    val cksum_input = input.zip(input_check){_==_}.reduce{_&&_}

    println("PASS: " + cksum + " && " + cksum_input + " (memdebug)")

  }
}

object NOTUCHY extends SpatialApp {
  type T = FixPt[TRUE,_16,_0]

  @virtualize
  def main() {

    val debug:scala.Boolean = false

    val PX = 1
    val loadPar = 32 (1 -> 16)
    val storePar = 32 (1 -> 16)
    val P1 = 2 (1 -> 16)
    val rowJump = 50 (10 -> 200)

    // Scalar params
    val INPUT_ROWS = ArgIn[Int]
    val INPUT_COLS = ArgIn[Int]
    val INPUT_CHANS = ArgIn[Int]
    val OUTPUT_CHANS = ArgIn[Int]
    val STRIDE = ArgIn[Int] // Assume horiz and vert stride match
    val KERNEL_ROWS = 3
    val KERNEL_COLS = 3

    // Shadow params (input args)
    val input_rows = args(0).to[Int]
    val input_cols = args(1).to[Int]
    val input_chans = args(2).to[Int]
    val output_chans = args(3).to[Int]
    val stride = args(4).to[Int]
    val print_data = args(5).to[Bit]

    // Set args
    setArg(INPUT_ROWS, input_rows)
    setArg(INPUT_COLS, input_cols)
    setArg(INPUT_CHANS, input_chans)
    setArg(OUTPUT_CHANS, output_chans)
    setArg(STRIDE, stride)

    // HW Design properties
    val INPUT_COLS_MAX = 640
    val INPUT_CHANS_MAX = 64
    val OUTPUT_CHANS_MAX = 64

    // Memories
    val INPUT_DATA = DRAM[T](INPUT_CHANS, INPUT_ROWS, INPUT_COLS)
    val OUTPUT_DATA = DRAM[T](OUTPUT_CHANS, INPUT_ROWS/STRIDE, INPUT_COLS/STRIDE)
    val KERNEL_DATA = DRAM[T](OUTPUT_CHANS, INPUT_CHANS, KERNEL_ROWS, KERNEL_COLS)
    val BIAS_DATA = DRAM[T](OUTPUT_CHANS, INPUT_ROWS/STRIDE, INPUT_COLS/STRIDE)

    // Load data (placeholder)
    val input = (0::INPUT_CHANS, 0::INPUT_ROWS, 0::INPUT_COLS) {(i,j,k) => ((i + j + k) % 8).to[T]}
    val output = (0::OUTPUT_CHANS, 0::INPUT_ROWS/STRIDE, 0::INPUT_COLS/STRIDE){(i,j,k) => 0.to[T]}
    val kernel = (0::OUTPUT_CHANS, 0::INPUT_CHANS, 0::KERNEL_ROWS, 0::KERNEL_COLS) {(i,j,k,l) => if (random[Int](10) > 8) 1.to[T] else 0.to[T]}
    val bias = (0::OUTPUT_CHANS, 0::INPUT_ROWS/STRIDE, 0::INPUT_COLS/STRIDE){(i,j,k) => 1.to[T]}

    // Set data
    setMem(INPUT_DATA, input)
    setMem(OUTPUT_DATA, output)
    setMem(KERNEL_DATA, kernel)
    setMem(BIAS_DATA, bias)

    // setMem(KERNEL_COPY_CPU, kernel)
    // println("MANUALLY COPY KERNEL_DATA PTR TO ME!")

    Accel{
      val kernel_sram = SRAM[T](OUTPUT_CHANS_MAX, INPUT_CHANS_MAX, KERNEL_ROWS, KERNEL_COLS)
      kernel_sram load KERNEL_DATA(0::OUTPUT_CHANS, 0::INPUT_CHANS, 0::KERNEL_ROWS, 0::KERNEL_COLS)

      Foreach(OUTPUT_CHANS by 1){ oc => 
        Foreach(INPUT_CHANS by 1){ ic =>           
          Foreach(INPUT_ROWS by rowJump par P1){ rr => 
            val lb1 = LineBuffer.strided[T](KERNEL_ROWS, INPUT_COLS_MAX, 1)
            val lb2 = LineBuffer.strided[T](KERNEL_ROWS, INPUT_COLS_MAX, 2)
            val rows_todo = min(rowJump, INPUT_ROWS - rr)
            val bias_sram = SRAM[T](INPUT_COLS_MAX) // / STRIDE?
            val accum_line = SRAM.buffer[T](INPUT_COLS_MAX)
            Foreach(-2 until rows_todo by STRIDE par PX) { r => 
              val ldrow = if ((r.to[Index]+rr.to[Index]) < 0.to[Index] || (r.to[Index]+rr.to[Index]) > INPUT_ROWS.value) 0.to[Index] else {r.to[Index]+rr.to[Index]} 
              if (ic == 0) bias_sram load BIAS_DATA(oc, ldrow/STRIDE, 0::INPUT_COLS/STRIDE par loadPar)
              // Parallel{
                Pipe{accum_line load OUTPUT_DATA(oc, ldrow/STRIDE, 0::INPUT_COLS/STRIDE par loadPar)}
                Pipe{
                  if (STRIDE.value == 1) lb1 load INPUT_DATA(ic, ldrow,0::INPUT_COLS par loadPar)
                  else lb2 load INPUT_DATA(ic, ldrow::ldrow+2,0::INPUT_COLS par loadPar)
                }
              // }
              Foreach(INPUT_COLS by STRIDE par PX){ col =>
                val sr1 = RegFile[T](KERNEL_ROWS,KERNEL_COLS)
                val sr2 = RegFile[T](KERNEL_ROWS,KERNEL_COLS)
                if (STRIDE.value == 1) Foreach(3 by 1 par 3){i => sr1(i,*) <<= lb1(i, col)}
                else Foreach(3 by 1 par 3){i => sr2(i,*) <<= lb2(i, col::col+2)}
                val filter_elements = List.tabulate(3){i => List.tabulate(3){j => 
                  kernel_sram(oc,ic,i,j)
                }}.flatten
                val data_elements = List.tabulate(3){i => List.tabulate(3){j => 
                  if (ldrow.to[Int] + STRIDE.value - KERNEL_ROWS + i.to[Int] < 0 
                      || col.to[Int] + STRIDE.value - KERNEL_COLS + j.to[Int] < 0 ) 
                           {0.to[T]}
                  else 
                    if (STRIDE.value == 1) {sr1(i,KERNEL_COLS - 1 - j)} 
                    else {sr2(i,KERNEL_COLS - 1 - j)}
                }}.flatten

                val accum = data_elements.zip(filter_elements).map{case(a,b) => a*b}.reduce{_+_}
                accum_line(col/STRIDE) = accum + accum_line(col/STRIDE) + mux(ic == 0, bias_sram(col/STRIDE), 0.to[T])
                if (debug) println(" = " + accum_line(col/STRIDE) + ", bias = " + mux(ic == 0, bias_sram(col/STRIDE), 0.to[T]))
              }
              // Store line back
              if (r.to[Index]+rr.to[Index] < INPUT_ROWS && r.to[Index] >= 0.to[Index]) OUTPUT_DATA(oc, ldrow/STRIDE, 0::INPUT_COLS/STRIDE par storePar) store accum_line
            }
          }
        }
      }
    }

    // Get results
    val results = getTensor3(OUTPUT_DATA)
    println("Results are " + results.dim0 + " x " + results.dim1 + " x " + results.dim2)

    // Compute Checks
    val gold = (0::OUTPUT_CHANS, 0::INPUT_ROWS by STRIDE, 0::INPUT_COLS by STRIDE){(k,i,j) => 
      Array.tabulate(INPUT_CHANS){page => 
        if (debug && print_data) {
            println("Result working on " + k + "," + page + "," + i/STRIDE + "," + j/STRIDE)
            println(" Window: ")
            for (ii <- 0 until KERNEL_ROWS) { 
              for (jj <- 0 until KERNEL_COLS){
                if (i-KERNEL_ROWS+STRIDE+ii < 0 || j-KERNEL_COLS+STRIDE+jj < 0) print(" X") 
                else print(" " + input(page,i-(KERNEL_ROWS-STRIDE-ii),j-(KERNEL_COLS-STRIDE-jj)))
              } 
              println(" ")
            }
        }


        Array.tabulate(KERNEL_ROWS){ii => Array.tabulate(KERNEL_COLS){jj => 
          val pxl = if (i-KERNEL_ROWS+STRIDE+ii < 0 || j-KERNEL_COLS+STRIDE+jj < 0) 0.to[T] 
                    else input(page,i-(KERNEL_ROWS-STRIDE-ii),j-(KERNEL_COLS-STRIDE-jj))
          val f = kernel(k,page, ii, jj)
          if (debug) println(" Partial is " + pxl + " * " + f + " @ " + ii + "," + jj)
          pxl * f
        }}.flatten.reduce{_+_}
      }.reduce{_+_} + bias(k, i/STRIDE, j/STRIDE)
    }
  
    if (print_data) {
      printTensor3(input, "Input")
      printTensor4(kernel, "Kernel")
      printTensor3(gold, "Gold")
      printTensor3(results, "Extracted")  

      val bitmask = (0::OUTPUT_CHANS, 0::INPUT_ROWS by STRIDE, 0::INPUT_COLS by STRIDE){(k,i,j) =>
        if (results(k,i/STRIDE,j/STRIDE) == gold(k,i/STRIDE,j/STRIDE)) 1.to[Int] else 0.to[Int]
      }
      val num_wrong = bitmask.length - bitmask.reduce{_+_}
      printTensor3(bitmask, "Matchup")
      println("Error rate: " + num_wrong + " / " + bitmask.length + " incorrect")

    }
    val cksum = gold.zip(results){_==_}.reduce{_&&_}    

    println("PASS: " + cksum + " (pilot)")

  }
}
