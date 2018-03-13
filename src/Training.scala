import spatial.dsl._
import virtualized._
import spatial.targets._


object SVRG extends SpatialApp {  // Test Args: 25 30 256 0.0001 0.0009 10

  type TM = FixPt[TRUE, _8, _24]
  type TX = FixPt[TRUE, _8, _8]
  val margin = 2 // Maximum distance between gold weights and computed weights to consider the app "passing"

  val tileSize = 16 (16 -> 128)

  val loadPar = 1
  val storePar = 1
  val P1 = 2 (1 -> 8)
  val P2 = 2 (1 -> 8)
  val P3 = 2 (1 -> 8)
  val P4 = 2 (1 -> 8)
  val P5 = 2 (1 -> 8)
  val P6 = 2 (1 -> 8)
  val P7 = 2 (1 -> 8)
  val P8 = 2 (1 -> 8)
  val P9 = 2 (1 -> 8)
  val P10 = 2 (1 -> 8)
  val P11 = 2 (1 -> 8)
  val P12 = 2 (1 -> 8)
  val PX = 1

  @virtualize
  def main() {
    val epochs = args(0).to[Int] // Epochs
    val len_epoch = args(1).to[Int] // Epoch Length
    val points = args(2).to[Int] // Total Points
    val alpha1 = args(3).to[TM] // Step size
    val alpha2 = args(4).to[TM] // Step size
    val D = 16
    val bump_epoch = args(5).to[Int]

    val noise_num = 2
    val noise_denom = 10
    // Generate some test data
    val sX = (0::points, 0::D){(i,j) => random[TX](3.to[TX]) + 1.to[TX]}
    val W_gold = Array.tabulate(D) { i => random[TM](3.to[TM]) / 2.to[TM]}
    val sY = Array.tabulate(points) { i => (random[TX](noise_num.to[TX]) / noise_denom - noise_num/2) + Array.tabulate(D){j => W_gold(j) * sX(i,j).to[TM]}.reduce{_+_}.to[TX] }

    val E = ArgIn[Int]
    val N = ArgIn[Int]
    val T = ArgIn[Int]
    val BUMP_EPOCH = ArgIn[Int]
    val A1 = ArgIn[TM]
    val A2 = ArgIn[TM]

    setArg(E, epochs)
    setArg(N, points)
    setArg(T, len_epoch)
    setArg(A1, alpha1)
    setArg(A2, alpha2)
    setArg(BUMP_EPOCH, bump_epoch)

    val x = DRAM[TX](N, D)
    val y = DRAM[TX](N)
    val w = DRAM[TM](D)

    printMatrix(sX, "X Data")
    printArray(sY, "Y Data")
    printArray(W_gold, "W Gold")
    setMem(x, sX)
    setMem(y, sY)
    setMem(w, Array.fill(D)(0.to[TM]))


    Accel { Sequential{
      // Create model and gradient memories
      val w_k = SRAM[TM](D)
      val g_k = SRAM[TM](D)
      val y_cache = SRAM[TX](tileSize)
      val y_cache_base = Reg[Int](-1)

      w_k load w(0::D par loadPar)

      // Outer loop (epochs)
      Sequential.Foreach(E by 1 par PX) { e =>
        // Choose correct step for this epoch
        val A = mux(e < BUMP_EPOCH, A1.value, A2.value)

        // Do full update over all points to get g_k and w_k (outer loop)
        MemReduce(g_k par P3)(N by tileSize par P4){i => 
          val y_tile = SRAM[TX](tileSize)
          val x_tile = SRAM[TX](tileSize,D)
          Parallel{
            y_tile load y(i::i + tileSize)
            x_tile load x(i::i + tileSize, 0::D par loadPar)            
          }
          val g_k_partial = SRAM[TM](D)
          // Full update tile (inner loop)
          MemReduce(g_k_partial par P5)(tileSize by 1 par P6){ii =>
            val g_k_local = SRAM[TM](D)
            val y_err = Reduce(Reg[TX](0.to[TX]))(D by 1 par P12){j => (w_k(j) *&! x_tile(ii, j).to[TM]).to[TX]}{_+!_} -! y_tile(ii)
            Foreach(D by 1 par P7){j => g_k_local(j) =  -A *&! y_err.to[TM] *&! x_tile(ii, j).to[TM]}
            g_k_local
          }{_+!_}
        }{(a,b) => a +! b/tileSize.to[TM]}

        // Accumulation here may not be necessary
        // // Accumulate g_k into w_k
        // MemFold(w_k par P8)(1 by 1){_ => g_k}{_+!_}

        val w_k_t = SRAM[TM](D)

        // Copy w_k to w_k_t
        Foreach(D by 1 par P9){i => w_k_t(i) = w_k(i)}

        // Run len_epoch number of SGD points
        Foreach(T by 1 par PX){t => 
          // Choose random point
          val i = random[Int](1024) % N

          // Get y for this point
          val y_point = Reg[TX](0)
          if (i - y_cache_base >= 0 && i - y_cache_base < tileSize) {
            y_point := y_cache(i-y_cache_base)
          } else {
            y_cache_base := i - (i % tileSize)
            y_cache load y(y_cache_base::y_cache_base + tileSize)
            y_point := y_cache(i % tileSize)
          }

          // Get x for this point
          val x_point = SRAM[TX](D)
          x_point load x(i, 0::D par loadPar)

          // Compute gradient against w_k_t
          val y_err_t = Reduce(Reg[TX](0.to[TX]))(D by 1){j => (w_k_t(j) *&! x_point(j).to[TM]).to[TX]}{_+!_} -! y_point

          // Compute gradient against w_k
          val y_err_k = Reduce(Reg[TX](0.to[TX]))(D by 1){j => (w_k(j) *&! x_point(j).to[TM]).to[TX]}{_+!_} -! y_point

          // Update w_k_t with reduced variance update
          Foreach(D by 1 par P10){i => w_k_t(i) = w_k_t(i) -! (A *&! ((y_err_t *&! x_point(i)).to[TM] +! (y_err_k *&! x_point(i)).to[TM] -! g_k(i)))}

        }
        // Copy w_k_t to w_k
        Foreach(D by 1 par P11){i => w_k(i) = w_k_t(i)}
      }

      // Store back values
      w(0 :: D par storePar) store w_k
    }} // Close accel

    
    val w_result = getMem(w)
    val cartesian_dist = W_gold.zip(w_result) { case (a, b) => (a - b) * (a - b) }.reduce{_+_}

    val cksum = cartesian_dist < margin
    printArray(w_result, "result: ")
    printArray(W_gold, "gold: ")
    println("Cartesian Distance From W_gold: " + cartesian_dist.to[TM] + " <? " + {margin.to[TM]})

    println("PASS: " + cksum + " (SVRG)")
  }
}

object LP_SVRG extends SpatialApp {  // Test Args: 40 5 256 0.05 1 0.00005

  val margin = 2 // Maximum distance between gold weights and computed weights to consider the app "passing"

  val tileSize = 16 (16 -> 128)

  val loadPar = 1
  val storePar = 1
  val P1 =  1 (1 -> 8)
  val P2 =  1 (1 -> 8)
  val P3 =  1 (1 -> 8)
  val P4 =  1 (1 -> 8)
  val P5 =  1 (1 -> 8)
  val P6 =  1 (1 -> 8)
  val P7 =  1 (1 -> 8)
  val P8 =  1 (1 -> 8)
  val P9 =  1 (1 -> 8)
  val P10 = 1 (1 -> 8)
  val P11 = 1 (1 -> 8)
  val P12 = 1 (1 -> 8)
  val PX = 1

  val shift_factor = 16

  type B = Int8
  type BB = Int16
  type BBBB = Int32

  val debug = true
  /*

    Basic type relationships:
      X ->  (DX,    B)
      W ->  (DM,    B)
      Y ->  (DM*DX, BB)
      GR -> (DG,    BBBB)   -> (DM*DX*DX*DA, BBBB)

      ... Choose DA so that DG is off by a power of 2 from DM.  I.E.- DM = DG*2^8 if shift_factor=8 ...

      A ->  (DA,    B)      -> (1/(2^8*DX*DX), B)

  */

  @virtualize
  def toDM(in: BBBB): B = { ((in + random[BBBB](scala.math.pow(2.0,shift_factor).to[BBBB])) >> shift_factor).to[B] }
  @virtualize
  def toDG(in: B): BBBB = { in.to[BBBB] << shift_factor }

  @virtualize 
  def FloatToLP[T:Type:Num](in: Float, delta: Float, precision: scala.Int): T = {
    val exact = in / delta
    
    if (exact < -scala.math.pow(2,(precision-1))) -(scala.math.pow(2,(precision-1))).to[T]
    else if (exact > scala.math.pow(2, (precision-1)-1)) scala.math.pow(2, (precision-1)-1).to[T]
    else (exact + random[Float](1)).to[T]
  }

  @virtualize 
  def LPToFloat[T:Type:Num](in: T, delta: Float, precision: scala.Int): Float = {delta * in.to[Float]}

  @virtualize
  def main() {
    val epochs = args(0).to[Int] // Epochs
    val len_epoch = args(1).to[Int] // Epoch Length
    val points = args(2).to[Int] // Total Points
    val dm = args(3).to[Float] // delta for model
    val dx = args(4).to[Float] // delta for data
    val da = 1/(scala.math.pow(2.0,shift_factor).to[Float]*dx*dx)
    // val da = args(5).to[Float] // delta for step (alpha)
    val alpha1 = args(5).to[Float] // Step size
    val D = 6

    val noise_num = 3
    val noise_denom = 10
    
    // Generate some test data
    val sX = (0::points, 0::D){(i,j) => (random[Float](6) - 2.to[Float])}
    val W_gold = Array.tabulate(D) { i => (random[Float](3) / 2)}
    val Y_noise = Array.tabulate(points) { i => (random[Float](noise_num) - (noise_num/2).to[Float]) / noise_denom.to[Float] }
    val sY = Array.tabulate(points) { i => Array.tabulate(D){j => (W_gold(j) * sX(i,j))}.reduce{_+_} + Y_noise(i) }

    // Convert data to LP
    val W_bits = Array.tabulate(D) { i => FloatToLP[B](W_gold(i), dm, 8)}

    val Y_noise_bits = Array.tabulate(points) {i => FloatToLP[BB](Y_noise(i), dx*dm, 16)}
    val X_bits = (0::points, 0::D){(i,j) => FloatToLP[B](sX(i,j), dx, 8)}
    val Y_bits = Array.tabulate(points){ i => FloatToLP[BB](sY(i), dx*dm, 16)}
    val alpha1_bits = FloatToLP[B](alpha1, da, 8)

    // Debug
    val W_recompute = Array.tabulate(D) { i => LPToFloat[B](W_bits(i), dm, 8)}
    printArray(W_gold, "W_gold")
    printArray(W_bits, "W_bits")
    printArray(W_recompute, "W_gold Reconstructed")
    println("dm = " + dm)
    println("dx = " + dx)
    println("da = " + da)
    println("dm*dx = " + dm*dx)
    println("dm*dx*dx*da = " + dm*dx*dx*da)
    println("Alpha bits: " + alpha1_bits)
    printMatrix(X_bits, "X_bits")
    printArray(Y_bits, "Y_bits")
    printArray(W_bits, "W_bits")
    printArray(Y_noise_bits, "Y_noise_bits")


    val E = ArgIn[Int]
    val N = ArgIn[Int]
    val T = ArgIn[Int]
    val DM = HostIO[Float]
    val DX = HostIO[Float]
    val DMDX = HostIO[Float]
    val DG = HostIO[Float]
    val DA = HostIO[Float]
    val A1 = ArgIn[B]

    setArg(E, epochs)
    setArg(N, points)
    setArg(T, len_epoch)
    setArg(A1, alpha1_bits)
    setArg(DM,   dm)
    setArg(DX,   dx)
    setArg(DA,   da)
    setArg(DMDX, dm*dx)
    setArg(DG,   dm*dx*dx*da)


    val x = DRAM[B](N, D)
    val y = DRAM[BB](N)
    val w = DRAM[B](D)

    setMem(x, X_bits)
    setMem(y, Y_bits)
    setMem(w, Array.fill(D)(0.to[B]))


    Accel {
      // Create model and gradient memories
      val w_k = SRAM[B](D) // DM
      val g_k = SRAM[BBBB](D) // DG
      val y_cache = SRAM[BB](tileSize) // DM*DX
      val y_cache_base = Reg[Int](0) 

      w_k load w(0::D par loadPar)

      // Outer loop (epochs)
      Sequential.Foreach(E by 1 par PX) { e =>
        // Choose correct step for this epoch
        val A = A1.value

        // Do full update over all points to get g_k and w_k (outer loop)
        MemReduce(g_k par P3)(N by tileSize par P4){i => 
          val x_tile = SRAM[B](tileSize,D) // DX
          Parallel{
            y_cache load y(i::i + tileSize par loadPar)
            y_cache_base := i
            x_tile load x(i::i + tileSize, 0::D par loadPar)            
          }
          val g_k_partial = SRAM[BBBB](D)    // DG
          // Full update tile (inner loop)
          MemReduce(g_k_partial par P5)(tileSize by 1 par P6){ii =>
            val g_k_local = SRAM[BBBB](D)  // DG
            val y_hat = Reg[BB](0.to[BB]) // DM*DX
            Reduce(y_hat)(D by 1 par P12){j => w_k(j).to[BB] *! x_tile(ii, j).to[BB]}{_+!_} // DM*DX
            val y_err = y_hat.value -! y_cache(ii) // DM*DX
            Foreach(D by 1 par P7){j => 
              g_k_local(j) =  -A.to[BBBB] *! y_err.to[BBBB] *! x_tile(ii, j).to[BBBB]
            } // DG
            g_k_local
          }{_+!_}
        }{(a,b) => a +! b/tileSize.to[BBBB]}
 
        // Accumulation here may not be necessary
        // // Accumulate g_k into w_k
        // MemFold(w_k par P8)(1 by 1){_ => g_k}{_+!_}

        // Do SGD
        val w_k_t = SRAM[B](D) // DM

        // Copy w_k to w_k_t
        Foreach(D by 1 par P9){i => w_k_t(i) = w_k(i)}

        // Run len_epoch number of SGD points
        Foreach(T by 1 par PX){t => 
          // Choose random point
          val i = random[Int](1024) % N

          // Get y for this point
          val y_point = Reg[BB](0) // DM*DX
          if (i - y_cache_base >= 0 && i - y_cache_base < tileSize) {
            y_point := y_cache(i-y_cache_base)
          } else {
            y_cache_base := i - (i % tileSize)
            y_cache load y(y_cache_base::y_cache_base + tileSize par loadPar)
            y_point := y_cache(i % tileSize)
          }

          // Get x for this point
          val x_point = SRAM[B](D) // DX
          x_point load x(i, 0::D par loadPar)

          // Compute gradient against w_k_t
          val y_hat_t = Reg[BB](0.to[BB])
          Reduce(y_hat_t)(D by 1){j => (w_k_t(j).to[BB] *&! x_point(j).to[BB])}{_+!_}
          val y_err_t = y_hat_t.value -! y_point

          // Compute gradient against w_k
          val y_hat_k = Reg[BB](0.to[BB])
          Reduce(y_hat_k)(D by 1){j => (w_k(j).to[BB] *&! x_point(j).to[BB])}{_+!_}
          val y_err_k = y_hat_k.value -! y_point

          // Update w_k_t with reduced variance update
          Foreach(D by 1 par P10){i => w_k_t(i) = toDM(toDG(w_k_t(i)) -! 
                                                    A.to[BBBB] *! (
                                                      (y_err_t.to[BBBB] *! x_point(i).to[BBBB]) +! 
                                                      (y_err_k.to[BBBB] *&! x_point(i).to[BBBB]) -! 
                                                      g_k(i)
                                                    )
                                                  )
                                                }

          if (debug) { 
            println("*** Step " + {t + e*T} + ": ")
            println("y_err_t = " + y_err_t + " ( = " + y_hat_t.value + " - " + y_point + ")")
            Foreach(D by 1) { i => 
              val gradientLP = A.to[BBBB] *! (
                               (y_err_t.to[BBBB] *! x_point(i).to[BBBB]) +! 
                               (y_err_k.to[BBBB] *&! x_point(i).to[BBBB]) -! 
                               g_k(i)
                             )

              print(" " + gradientLP)
            }
            println("\nWeights: ")
            Foreach(D by 1) { i => 
              print(" " + w_k_t(i))
            }
            println("\n")
          }

        }
        // Copy w_k_t to w_k
        Foreach(D by 1 par P11){i => w_k(i) = w_k_t(i)}
      }

      // Store back values
      w(0 :: D par storePar) store w_k
    }

    
    val w_result = getMem(w)

    val w_result_fullprecision = Array.tabulate(D){i => LPToFloat[B](w_result(i), dm, 8)}
    val cartesian_dist = W_gold.zip(w_result_fullprecision) { case (a, b) => (a - b) * (a - b) }.reduce{_+_}
    val cksum =  cartesian_dist < margin
    printArray(w_result_fullprecision, "result: ")
    printArray(W_gold, "gold: ")
    println("Cartesian Distance From W_gold: " + cartesian_dist + " <? " + {margin.to[Float]})

    println("PASS: " + cksum + " (LP_SVRG)")
  }
}


object HALP extends SpatialApp {  // Test Args: 40 3 256 0.05 1 0.00003 0.4

  val margin = 2 // Maximum distance between gold weights and computed weights to consider the app "passing"

  val tileSize = 16 (16 -> 128)

  val loadPar = 1
  val storePar = 1
  val P1 =  1 (1 -> 8)
  val P2 =  1 (1 -> 8)
  val P3 =  1 (1 -> 8)
  val P4 =  1 (1 -> 8)
  val P5 =  1 (1 -> 8)
  val P6 =  1 (1 -> 8)
  val P7 =  1 (1 -> 8)
  val P8 =  1 (1 -> 8)
  val P9 =  1 (1 -> 8)
  val P10 = 1 (1 -> 8)
  val P11 = 1 (1 -> 8)
  val P12 = 1 (1 -> 8)
  val PX = 1

  val shift_factor_lower_bound = 15
  val shift_factor_upper_bound = 25

  type B = Int8
  type BB = Int16
  type BBBB = Int32

  val debug = false
  /*

    Basic type relationships:
      X ->  (DX,    B)
      W ->  (DM,    B)
      Y ->  (DM*DX, BB)
      GR -> (DG,    BBBB)   -> (DM*DX*DX*DA, BBBB)

      ... Choose DA so that DG is off by a power of 2 from DM.  I.E.- DM = DG*2^8 if shift_factor=8 ...

      A ->  (DA,    B)      -> (1/(2^8*DX*DX), B)


    We choose:
      DX
      DM
      shift_factor

    We derive:
      DY = DM*DX
      DA = 2^-shift_factor / DX / DX
      DG = DM*DX*DX*DA

    We try rescale:
      DG to keep gradients in range
      ? DM to keep model well resolved ?
  */

  @virtualize
  def toDM(in: BBBB, sf: Int): B = { 
    val shift_factor_range = shift_factor_upper_bound - shift_factor_lower_bound
    val options = List.tabulate(shift_factor_range){i => 
      ((in + random[BBBB](scala.math.pow(2.0,(i+shift_factor_lower_bound)).to[BBBB])) >> (i+shift_factor_lower_bound)).to[B]   
    }
    if (sf == shift_factor_lower_bound) options(0)
    else if (sf == (shift_factor_lower_bound + 1)) options(1)
    else if (sf == (shift_factor_lower_bound + 2)) options(2)
    else if (sf == (shift_factor_lower_bound + 3)) options(3)
    else if (sf == (shift_factor_lower_bound + 4)) options(4)
    else if (sf == (shift_factor_lower_bound + 5)) options(5)
    else if (sf == (shift_factor_lower_bound + 6)) options(6)
    else if (sf == (shift_factor_lower_bound + 7)) options(7)
    else if (sf == (shift_factor_lower_bound + 8)) options(8)
    else options(9)
  }
  @virtualize
  def toDG(in: B, sf: Int): BBBB = { 
    val options = List.tabulate(shift_factor_upper_bound - shift_factor_lower_bound){i => 
      in.to[BBBB] << (i+shift_factor_lower_bound)
    }
    val selects = List.tabulate(shift_factor_upper_bound - shift_factor_lower_bound){i => (sf-shift_factor_lower_bound) == i}
    if (sf == shift_factor_lower_bound) options(0)
    else if (sf == (shift_factor_lower_bound + 1)) options(1)
    else if (sf == (shift_factor_lower_bound + 2)) options(2)
    else if (sf == (shift_factor_lower_bound + 3)) options(3)
    else if (sf == (shift_factor_lower_bound + 4)) options(4)
    else if (sf == (shift_factor_lower_bound + 5)) options(5)
    else if (sf == (shift_factor_lower_bound + 6)) options(6)
    else if (sf == (shift_factor_lower_bound + 7)) options(7)
    else if (sf == (shift_factor_lower_bound + 8)) options(8)
    else options(9)

  }

  @virtualize 
  def FloatToLP[T:Type:Num](in: Float, delta: Float, precision: scala.Int): T = {
    val exact = in / delta
    
    if (exact < -scala.math.pow(2,(precision-1))) -(scala.math.pow(2,(precision-1))).to[T]
    else if (exact > scala.math.pow(2, (precision-1))-1) (scala.math.pow(2, (precision-1))-1).to[T]
    else (exact + random[Float](1)).to[T]
  }

  @virtualize 
  def LPToFloat[T:Type:Num](in: T, delta: Float, precision: scala.Int): Float = {delta * in.to[Float]}

  @virtualize
  def main() {
    val init_shift_factor = 16
    val epochs = args(0).to[Int] // Epochs
    val len_epoch = args(1).to[Int] // Epoch Length
    val points = args(2).to[Int] // Total Points
    val dm = args(3).to[Float] // delta for model
    val dx = args(4).to[Float] // delta for data
    val alpha1 = args(5).to[Float] // Step size
    val mu = args(6).to[Float] // Set point (0 to 1) for gradient radius 
    val D = 6

    val noise_num = 3
    val noise_denom = 10
    
    // Generate some test data
    val sX = (0::points, 0::D){(i,j) => (random[Float](6) - 2.to[Float])}
    val W_gold = Array.tabulate(D) { i => (random[Float](3) / 2)}
    val Y_noise = Array.tabulate(points) { i => (random[Float](noise_num) - (noise_num/2).to[Float]) / noise_denom.to[Float] }
    val sY = Array.tabulate(points) { i => Array.tabulate(D){j => (W_gold(j) * sX(i,j))}.reduce{_+_} + Y_noise(i) }

    // Convert data to LP
    val W_bits = Array.tabulate(D) { i => FloatToLP[B](W_gold(i), dm, 8)}

    val Y_noise_bits = Array.tabulate(points) {i => FloatToLP[BB](Y_noise(i), dx*dm, 16)}
    val X_bits = (0::points, 0::D){(i,j) => FloatToLP[B](sX(i,j), dx, 8)}
    val Y_bits = Array.tabulate(points){ i => FloatToLP[BB](sY(i), dx*dm, 16)}
    val da = 1/(scala.math.pow(2.0,init_shift_factor).to[Float]*dx*dx)
    val alpha1_bits = FloatToLP[B](alpha1, da, 8)

    // Debug
    val W_recompute = Array.tabulate(D) { i => LPToFloat[B](W_bits(i), dm, 8)}
    printArray(W_gold, "W_gold")
    printArray(W_bits, "W_bits")
    printArray(W_recompute, "W_gold Reconstructed")
    println("dm = " + dm)
    println("dx = " + dx)
    println("dm*dx = " + dm*dx)
    printMatrix(X_bits, "X_bits")
    printArray(Y_bits, "Y_bits")
    printArray(W_bits, "W_bits")
    printArray(Y_noise_bits, "Y_noise_bits")

 
    val E = ArgIn[Int] // current epoch
    val N = ArgIn[Int]
    val T = ArgIn[Int]
    val DM = HostIO[Float]
    val DX = HostIO[Float]
    val DMDX = HostIO[Float]
    val DG = HostIO[Float]
    val DA = HostIO[Float]
    val A = ArgIn[B]
    val MU = ArgIn[Float]
    val PHASE = ArgIn[Int]
    val SF = ArgIn[Int]

    setArg(N, points)
    setArg(T, len_epoch)
    setArg(DM,   dm)
    setArg(DX,   dx)
    setArg(DMDX, dm*dx)
    setArg(MU, mu)
    setArg(SF, init_shift_factor)
    setArg(A, alpha1_bits)
    setArg(DA, da)


    val x = DRAM[B](N, D)
    val y = DRAM[BB](N)
    val w = DRAM[B](D)
    val g = DRAM[BBBB](D)

    setMem(x, X_bits)
    setMem(y, Y_bits)
    setMem(w, Array.tabulate[B](D){i => 0})

    val gradient_phase = 0
    val update_phase = 1

    for (e <- 0 until epochs*2) {
      // Set phase (hack until multi-accel works)
      val phase = e % 2
      setArg(PHASE, phase)

      // Set epoch info
      setArg(E, e)

      // Recenter gradient and model
      if (e > 2 && phase == update_phase) {
        val gradient_LP = getMem(g)
        val gradient_mag = Array.tabulate(D){i => gradient_LP(i) * gradient_LP(i) / D}.reduce{_+_}
        val current_gradient_mu = gradient_mag.to[Float] / (scala.math.pow(2,31) - 1).to[Float]
        val ratio = current_gradient_mu / mu
        if (debug) println(" Gradients magnitude = " + gradient_mag + ", DG = " + DG.value)
        if ((1/ratio >= 2 || ratio >= 2) && getArg(SF) < shift_factor_upper_bound && getArg(SF) > shift_factor_lower_bound) { // Apply recentering
          val sf_old = getArg(SF)
          if (debug) println(" Old SF: " + sf_old)
          val new_dg = if (1/ratio >= 2) { // Delta too small
            setArg(SF, sf_old + 1)
            DG.value * 2
          } else { // Delta too large
            setArg(SF, sf_old - 1)
            DG.value / 2
          }
          if (debug) println(" New SF: " + getArg(SF))
          // Set DA and DG after recentering
          val da = 1/(pow(2.to[Float],SF.value.to[Float]).to[Float]*dx*dx)
          val alpha1_bits = FloatToLP[B](alpha1, da, 8)

          if (debug) println(" Gradient mu = " + current_gradient_mu + " (ratio = " + {1/ratio} + ")")
          if (debug) println("da = " + da)
          if (debug) println("dm*dx*dx*da = " + dm*dx*dx*da)
          if (debug) println("Alpha bits: " + alpha1_bits)
          println("\nRecentering on iter : " + getArg(E)/2 + " (SHIFT_FACTOR: " + sf_old + " -> " + getArg(SF) + ", alpha bits: " + alpha1_bits + ")")
          setArg(DA, da)
          setArg(A, alpha1_bits)
          setArg(DG, new_dg)

        }
      }

      Accel {

        // Create model and gradient memories
        val w_k = SRAM[B](D) // DM
        val g_k = SRAM[BBBB](D) // DG
        val y_cache = SRAM[BB](tileSize) // DM*DX
        val y_cache_base = Reg[Int](-1) 
        val w_k_t = SRAM[B](D) // DM

        Parallel{
          w_k load w(0 :: D par loadPar)
          g_k load g(0 :: D par loadPar)
          w_k_t load w(0 :: D par loadPar)
        }

        if (PHASE.value == gradient_phase) {
          // Do full update over all points to get g_k and w_k (outer loop)
          MemReduce(g_k par P3)(N by tileSize par P4){i => 
            val x_tile = SRAM[B](tileSize,D) // DX
            Parallel{
              y_cache load y(i::i + tileSize par loadPar)
              x_tile load x(i::i + tileSize, 0::D par loadPar)              
            }
            val g_k_partial = SRAM[BBBB](D)    // DG
            // Full update tile (inner loop)
            MemReduce(g_k_partial par P5)(tileSize by 1 par P6){ii =>
              val g_k_local = SRAM[BBBB](D)  // DG
              val y_hat = Reg[BB](0.to[BB]) // DM*DX
              Reduce(y_hat)(D by 1 par P12){j => w_k(j).to[BB] *! x_tile(ii, j).to[BB]}{_+!_} // DM*DX
              val y_err = y_hat.value -! y_cache(ii) // DM*DX
              Foreach(D by 1 par P7){j => 
                g_k_local(j) =  -A.value.to[BBBB] *! y_err.to[BBBB] *! x_tile(ii, j).to[BBBB]
              } // DG
              g_k_local
            }{_+!_}
          }{(a,b) => a +! b/tileSize}
          g(0::D par storePar) store g_k
        } else if (PHASE.value == update_phase) {
          val g_k_t = SRAM[BBBB](D)

          // Run len_epoch number of SGD points
          Foreach(T by 1 par PX){t => 
            // Choose random point
            val i = random[Int](1024) % N

            // Get y for this point
            val y_point = Reg[BB](0) // DM*DX
            if (i - y_cache_base >= 0 && i - y_cache_base < tileSize && y_cache_base >= 0) {
              y_point := y_cache(i-y_cache_base)
            } else {
              y_cache_base := i - (i % tileSize)
              y_cache load y(y_cache_base::y_cache_base + tileSize par loadPar)
              y_point := y_cache(i % tileSize)
            }

            // Get x for this point
            val x_point = SRAM[B](D) // DX
            x_point load x(i, 0::D par loadPar)

            // Compute gradient against w_k_t
            val y_hat_t = Reg[BB](0.to[BB])
            Reduce(y_hat_t)(D by 1){j => (w_k_t(j).to[BB] *&! x_point(j).to[BB])}{_+!_}
            val y_err_t = y_hat_t.value -! y_point

            // Compute gradient against w_k
            val y_hat_k = Reg[BB](0.to[BB])
            Reduce(y_hat_k)(D by 1){j => (w_k(j).to[BB] *&! x_point(j).to[BB])}{_+!_}
            val y_err_k = y_hat_k.value -! y_point

            // Update w_k_t with reduced variance update
            Foreach(D by 1 par P10){i => 
              val gradient = - A.value.to[BBBB] *! (
                                (y_err_t.to[BBBB] *! x_point(i).to[BBBB]) +! 
                                (y_err_k.to[BBBB] *&! x_point(i).to[BBBB]) -! 
                                g_k(i)
                              )
              g_k_t(i) = gradient
              w_k_t(i) = toDM(toDG(w_k_t(i), SF.value) +! gradient, SF.value)
            }

            if (debug) {
              println("*** Step " + {t + E.value/2*T} + ": ")
              println("y_err_t = " + y_err_t + " ( = " + y_hat_t.value + " - " + y_point + ")")
              println("Gradient in BBBB")
              Foreach(D by 1) { i => 
                val gradientLP = - A.value.to[BBBB] *! (
                                 (y_err_t.to[BBBB] *! x_point(i).to[BBBB]) +! 
                                 (y_err_k.to[BBBB] *&! x_point(i).to[BBBB]) -! 
                                 g_k(i)
                               )

                print(" " + gradientLP)
              }
              println("\nWeights: ")
              Foreach(D by 1) { i => 
                print(" " + w_k_t(i))
              }
              println("\n")
            }
          }
          // Store back values
          Parallel{
            w(0 :: D par storePar) store w_k_t
            g(0 :: D par storePar) store g_k_t
          }

        }

      } // close Accel

      // Debug hooks
      if (debug) {
        if (phase == gradient_phase) {
          println("Post gradient phase: ")
          printArray(getMem(g), "Gradients: ")
        } else {
          println("Post update phase: ")
          printArray(getMem(w), "Weights: ")
        }
      }

    }

    
    val w_result = getMem(w)

    val w_result_fullprecision = Array.tabulate(D){i => LPToFloat[B](w_result(i), dm, 8)}
    val cartesian_dist = W_gold.zip(w_result_fullprecision) { case (a, b) => (a - b) * (a - b) }.reduce{_+_}
    val cksum =  cartesian_dist < margin
    printArray(w_result_fullprecision, "result: ")
    printArray(W_gold, "gold: ")
    println("Cartesian Distance From W_gold: " + cartesian_dist + " <? " + {margin.to[Float]})

    println("PASS: " + cksum + " (HALP)")
  }
}

/*





         Minibatch impelementation:
                             _
                            | |
                            |M|
                            | |
                            | |
                            | |
                            | |
                  D         |_|
             _____________   _       _                  _
            |             | |^|     | |                | |
          N |      X      | |Y|  -  |Y|  =>            |Y_err
            |_____________| |_|     |_|                |_|
                                                ____    _        _      _
                                               |    |  | |      | |    | |
                                               |    |  | |      |M|    |M|
                                               |    |  |Δ|  +   | | -> | |
                                               | X_T|  | |      | |    | |
                                               |    |  | |      | |    | |
                                               |    |  | |      | |    | |
                                               |____|  |_|      |_|    |_|


*/


object SGD extends SpatialApp { // Test Args: 40 64 0.0001


  type TM = FixPt[TRUE, _16, _16]
  type TX = FixPt[TRUE, _16, _16]
  val modelSize = 16
  val margin = 1

  val innerPar = 16
  val outerPar = 2

  val tileSize = 16 //192

  @virtualize
  def sgd_onept(x_in: Array[TX], y_in: Array[TX], alpha: TM, epochs: Int, nn: Int) = {
    val E = ArgIn[Int]
    val N = ArgIn[Int]
    val A = ArgIn[TM]
    val D = modelSize

    val ip = innerPar(1 -> 1)
    val op = outerPar(1 -> 1)

    setArg(E, epochs)
    setArg(N, nn)
    setArg(A, alpha)

    val x = DRAM[TX](N, D)
    val y = DRAM[TX](N)
    val result = DRAM[TM](D)

    setMem(x, x_in)
    setMem(y, y_in)

    Accel {
      val y_tile = SRAM[TX](tileSize)
      val sgdmodel = SRAM[TM](D)
      Pipe(D by 1) { i => sgdmodel(i) = 0.to[TM] }
      Sequential.Foreach(E by 1) { e =>
        Sequential.Foreach(N by tileSize) { b =>
          y_tile load y(b :: b + tileSize par ip)
          Foreach(tileSize by 1) { i =>
            val y_err = Reg[TX]
            val x_tile = SRAM[TX](D)
            Parallel {
              x_tile load x(b + i, 0 :: D par ip)
            }
            Pipe {
              val y_hat = Reg[TX]
              Reduce(y_hat)(D by 1 par ip) { j => x_tile(j) * sgdmodel(j).to[TX] } {
                _ + _
              }
              y_err := y_tile(i) - y_hat.value
            }

            Foreach(D by 1 par ip) { j =>
              sgdmodel(j) = sgdmodel(j) + x_tile(j).to[TM] * y_err.value.to[TM] * A
            }
          }
        }
      }
      result(0 :: D par ip) store sgdmodel

    }

    getMem(result)

  }

  def printArr(a: Array[TM], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  @virtualize
  def main() {
    val E = args(0).to[Int]
    val N = args(1).to[Int]
    val A = args(2).to[TM] // Should be somewhere around 0.0001 for point-wise sgd
    val D = modelSize

    val sX = Array.fill(N) {
      Array.fill(D) {
        random[TX](3.to[TX]) + 1.to[TX]
      }
    }
    val ideal_model = Array.tabulate(D) { i => 2.to[TM] }
    val sY = Array.tabulate(N) { i => ideal_model.zip(sX.apply(i)){case (a,b) => a.to[TX] * b}.reduce{_+_} }
    val id = Array.tabulate(D) { i => i }
    val ep = Array.tabulate(E) { i => i }

    val result = sgd_onept(sX.flatten, sY, A, E, N)

    val cksum = ideal_model.zip(result) { case (a, b) => abs(a - b) < margin }.reduce{_&&_}
    printArr(result, "result: ")
    printArr(ideal_model, "gold: ")
    println("PASS: " + cksum + " (SGD)")
  }
}


object SGD_minibatch extends SpatialApp { // Test Args: 40 64 0.0001


  type TM = FixPt[TRUE,_16,_16]
  type TX = FixPt[TRUE,_16,_16]
  val modelSize = 16
  val tileSize = 16
  val innerPar = 4
  val outerPar = 1 // Not used right now?
  val margin = 1

  @virtualize
  def sgdminibatch(x_in: Array[TX], y_in: Array[TX], alpha: TM, epochs: Int, nn: Int) = {
    val E = ArgIn[Int]
    val N = ArgIn[Int]
    val A = ArgIn[TM]
    val D = modelSize

    val ip = innerPar (1 -> 1)
    val op = outerPar (1 -> 1)

    setArg(E, epochs)
    setArg(N, nn)
    setArg(A, alpha)

    val x = DRAM[TX](N,D)
    val y = DRAM[TX](N)
    val result = DRAM[TM](D)

    setMem(x, x_in)
    setMem(y, y_in)

    Accel {
      val y_tile = SRAM[TX](tileSize)
      val sgdmodel = SRAM[TM](D)
      val x_tile = SRAM[TX](tileSize,D)
      Pipe(D by 1) { i => sgdmodel(i) = 0.to[TM]}
      Sequential.Foreach(E by 1) { e =>

        Sequential.Foreach (N by tileSize) { b =>
          y_tile load y(b::b+tileSize par ip)
          x_tile load x(b::b+tileSize, 0::D)
          val y_err = SRAM[TX](tileSize)
          Foreach(tileSize by 1) {i => 
            val y_hat = Reg[TX]
            Reduce(y_hat)(D by 1 par ip){ j => x_tile(i,j) * sgdmodel(j).to[TX] }{_+_}
            y_err(i) = y_tile(i) - y_hat.value
          }
          Foreach(D by 1) { i =>
            val raw_update = Reg[TX]
            Reduce(raw_update)(tileSize by 1 par ip){ j => x_tile(j,i) * y_err(j) }{_+_}
            sgdmodel(i) = sgdmodel(i) + raw_update.value.to[TM]*A
          }
        }
      }
      result(0::D par ip) store sgdmodel

    }

    getMem(result)

  }

  def printArr(a: Array[TM], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  @virtualize
  def main() {
    val E = args(0).to[Int]
    val N = args(1).to[Int]
    val A = args(2).to[TM] // Should be somewhere around 0.0001 for point-wise sgd
    val D = modelSize

    val sX = Array.fill(N){ Array.fill(D){ random[TX](3.to[TX]) + 1.to[TX]} }
    val ideal_model = Array.tabulate(D){ i => 2.to[TM] }
    val sY = Array.tabulate(N){i => ideal_model.zip(sX.apply(i)){case (a,b) => a.to[TX]*b}.reduce{_+_}}
    val id = Array.tabulate(D){ i => i }
    val ep = Array.tabulate(E){ i => i }

    val result = sgdminibatch(sX.flatten, sY, A, E, N)

    val cksum = ideal_model.zip(result){ case (a,b) => abs(a - b) < margin }.reduce{_&&_}
    printArr(result, "result: ")
    printArr(ideal_model, "gold: ")
    println("PASS: " + cksum  + " (SGD_minibatch)")
  }
}

