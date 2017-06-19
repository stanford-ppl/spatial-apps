import spatial._
import org.virtualized._


/*

  Implemntation based on http://cs.stanford.edu/people/karpathy/visml/ising_example.html
   pi(x) = exp(J* 𝚺x_j*x_i + J_b * 𝚺b_i*x_i)        

   let x' = x with one entry flipped

   Prob(accept x') ~ min(1, pi(x')/pi(x)) = exp(-2*J*𝚺x_j*x_i)*exp(-2*J_b*𝚺b_i*x_i)

  Use args 100 0.4 0 to get a nice looking lava lamp pattern
*/

object Gibbs_Ising2D extends SpatialApp { // DISABLED Regression (Dense) // Args: 200 0.3 2
  import IR._
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
