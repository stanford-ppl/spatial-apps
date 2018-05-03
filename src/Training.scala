import spatial.dsl._
import virtualized._
import spatial.targets._


object SVRG extends SpatialApp {  // Test Args: 20 3 256 0.0001 0.0009 10 1 2

  type TM = FixPt[TRUE, _11, _21]
  type TX = FixPt[TRUE, _11, _21]

  val tileSize = 64 (16 -> 128)

  val loadPar = 16
  val storePar = 16
  val max_history = 512
  val P1 = 1 (1 -> 8)
  val P2 = 1 (1 -> 8)
  val P3 = 1 (1 -> 8)
  val P4 = 1 (1 -> 8)
  val P5 = 1 (1 -> 8)
  val P6 = 1 (1 -> 8)
  val P7 = 1 (1 -> 8)
  val P8 = 1 (1 -> 8)
  val P9 = 1 (1 -> 8)
  val P10 = 1 (1 -> 8)
  val P11 = 1 (1 -> 8)
  val P12 = 1 (1 -> 8)
  val PX = 1

  val debug = true

  @virtualize
  def getValue(table: Matrix[MString], key: MString): Float = {
    val sum = Array.tabulate(table.rows){i => if (table(i,0) == key) table(i,1).to[Float] else 0.to[Float]}.reduce{_+_}
    if (sum == 0.to[Float]) println("WARN: Possibly could not find " + key)
    sum
  }

  @virtualize
  def main() {
    val config = loadCSV2D[MString](sys.env("SPATIAL_HOME") + "/apps/data/training/svrg.config", ",", "\n")
    printMatrix(config, "Config")

    val epochs = getValue(config, "epochs").to[Int]
    val len_epoch = getValue(config, "len_epoch").to[Int]
    val points = getValue(config, "points").to[Int] // Total Points
    val alpha1 = getValue(config, "alpha1").to[TM] // Step size
    val alpha2 = getValue(config, "alpha2").to[TM]
    val bump_epoch = getValue(config, "bump_epoch").to[Int]
    val track = getValue(config, "track").to[Int] // Track cost vs time
    val threshold = getValue(config, "threshold").to[TM] // Cost at which to quit (only quits if track is on)
    val variance = getValue(config, "variance").to[Int] // numerator for noise
    val warmup = getValue(config, "warmup").to[Int]

    val maxX = 6
    val D = 128

    val noise_num = variance
    val noise_denom = 10
    // Generate some test data
    val sX = (0::points, 0::D){(i,j) => random[TX](maxX.to[TX]) - (maxX/3).to[TX]}
    val W_gold = Array.tabulate(D) { i => random[TM](3.to[TM]) / 2.to[TM]}
    val Y_noise = Array.tabulate(points) { i => (random[Int](noise_num).to[TX] - (noise_num.to[TX]/2)) / noise_denom.to[TX] }
    val sY = Array.tabulate(points) { i => Y_noise(i) + Array.tabulate(D){j => W_gold(j) * sX(i,j).to[TM]}.reduce{_+_}.to[TX] }

    val E = ArgIn[Int]
    val N = ArgIn[Int]
    val T = ArgIn[Int]
    val BUMP_EPOCH = ArgIn[Int]
    val A1 = ArgIn[TM]
    val A2 = ArgIn[TM]
    val TRACK = ArgIn[Int]
    val THRESH = ArgIn[TM]
    val E_ACTUAL = HostIO[Int]
    val WARMUP = ArgIn[Int]

    setArg(E, epochs)
    setArg(N, points)
    setArg(T, len_epoch)
    setArg(A1, alpha1)
    setArg(A2, alpha2)
    setArg(BUMP_EPOCH, bump_epoch)
    setArg(TRACK, track)
    setArg(THRESH, threshold)
    setArg(E_ACTUAL, epochs*len_epoch-1)
    setArg(WARMUP, warmup-1)

    val x = DRAM[TX](N, D)
    val y = DRAM[TX](N)
    val w = DRAM[TM](D)
    val true_w = DRAM[TM](D)
    val cost = DRAM[TM](max_history)

    if (points < 10) printMatrix(sX, "X Data")
    else {
      printMatrix((0::10, 0::D){(i,j) => sX(i,j)}, "X Data")
      println("... Skipped last " + {points-10} + " rows")
    }
    printArray(Y_noise, "Y noise")
    printArray(sY, "Y Data")
    printArray(W_gold, "W Gold")
    setMem(x, sX)
    setMem(y, sY)
    setMem(w, Array.fill(D)(0.to[TM]))
    setMem(true_w, W_gold)
    setMem(cost, Array.fill(D)(0.to[TM]))

    Accel { 
      // Create model and gradient memories
      val w_k = SRAM[TM](D)
      val g_k = SRAM[TM](D)
      val y_cache = SRAM[TX](tileSize)
      val y_cache_base = Reg[Int](-1)

      // Debug
      val true_w_sram = SRAM[TM](D)
      val cost_sram = SRAM[TM](max_history)
      val w_k_t = SRAM[TM](D)

      if (TRACK.value == 1) true_w_sram load true_w
      w_k load w(0::D par loadPar)
      w_k_t load w(0::D par loadPar)
      

      // Outer loop (epochs)
      Sequential.Foreach(E by 1 par PX) { e =>
        // Choose correct step for this epoch
        val A = mux(e < BUMP_EPOCH, A1.value, A2.value)

        // Do full update over all points to get g_k and w_k (outer loop)
        if (e > WARMUP.value) {
          MemReduce(g_k, 0.to[TM])(N by tileSize par P4){i => 
            val y_tile = SRAM[TX](tileSize)
            val x_tile = SRAM[TX](tileSize,D)
            Parallel{
              y_tile load y(i::i + tileSize)
              x_tile load x(i::i + tileSize, 0::D par loadPar)            
            }
            val g_k_partial = SRAM[TM](D)
            // Full update tile (inner loop)
            MemReduce(g_k_partial, 0.to[TM])(tileSize by 1 par P6){ii =>
              val g_k_local = SRAM[TM](D)
              val y_err = Reduce(Reg[TX](0.to[TX]))(D by 1 par P12){j => (w_k(j) *&! x_tile(ii, j).to[TM]).to[TX]}{_+!_} -! y_tile(ii)
              Foreach(D by 1 par P7){j => g_k_local(j) =  y_err.to[TM] *&! x_tile(ii, j).to[TM] / N.value.to[TM]}
              g_k_local
            }{_+!_}
          }{(a,b) => a +! b}

          // Accumulation here may not be necessary
          // // Accumulate g_k into w_k
          // MemFold(w_k par P8)(1 by 1){_ => g_k}{_+!_}
          ()
        }

        // Run len_epoch number of SGD points
        Sequential.Foreach(T by 1 par PX){t => 
          // Choose random point
          // val i = random[Int](8192) % N
          val i = (e*T+t) % N

          // Get y for this point
          val y_point = Reg[TX](0)
          if (i - y_cache_base >= 0 && i - y_cache_base < tileSize && y_cache_base >= 0) {
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
          val y_err_k = if (e > WARMUP.value) {Reduce(Reg[TX](0.to[TX]))(D by 1){j => (w_k(j) *&! x_point(j).to[TM]).to[TX]}{_+!_} -! y_point} else 0.to[TX]

          // Update w_k_t with reduced variance update
          Foreach(D by 1 par P10){i => w_k_t(i) = w_k_t(i) -! (A *&! ((y_err_t *&! x_point(i)).to[TM] +! mux(e > WARMUP.value, -(y_err_k *&! x_point(i)).to[TM] +! g_k(i),0.to[TM]) ))}

          if (TRACK.value == 1) {
            val current_cost = Reduce(Reg[TM](0))(D by 1){i => pow(w_k_t(i) - true_w_sram(i), 2)}{_+!_}
            cost_sram(min((max_history-1).to[Int], e*T+t)) = current_cost
            if (current_cost < THRESH) {
              E_ACTUAL := min(e*T+t, (max_history-1).to[Int])
              cost(0 :: min((max_history-1).to[Int], getArg(E_ACTUAL))) store cost_sram
              w(0 :: D par storePar) store w_k_t

              breakpoint()
            }
          }

          if (debug) { 
            println("*** Step " + {t + e*T} + ": ")
            println("y_err_t = " + y_err_t + " ( = " + {y_err_t + y_point} + " - " + y_point + "), A = " + A.to[TM])
            Foreach(5 by 1) { i => 
              val gradientLP = A *&! ((y_err_t *&! x_point(i)).to[TM] +! mux(e > WARMUP.value, -(y_err_k *&! x_point(i)).to[TM] +! g_k(i),0.to[TM]) )

              print(" " + gradientLP + " (" + {y_err_t *&! x_point(i)} + " +! " + mux(e > WARMUP.value, -(y_err_k *&! x_point(i)).to[TM],0.to[TM]) + " +! " + mux(e > WARMUP.value, g_k(i).to[TM], 0.to[TM]) + ")")
            }
            println("\nWeights: ")
            Foreach(5 by 1) { i => 
              print(" " + w_k_t(i))
            }
            println("\n")
          }

        }
        // Copy w_k_t to w_k
        if (e >= WARMUP.value) Foreach(D by 1 par P11){i => w_k(i) = w_k_t(i)}
      }

      // Store back values
      if (TRACK.value == 1) cost(0 :: min((max_history-1).to[Int], getArg(E_ACTUAL))) store cost_sram
      w(0 :: D par storePar) store w_k_t
    } // Close accel

    
    val w_result = getMem(w)
    val cartesian_dist = W_gold.zip(w_result) { case (a, b) => (a - b) * (a - b) }.reduce{_+_}


    if (track == 1) {
      val cost_result = getMem(cost)
      val hist_len = min(getArg(E_ACTUAL), max_history.to[Int])
      val relevent_history = Array.tabulate[TM](hist_len){i => i.to[TM]} ++ Array.tabulate(hist_len){i => cost_result(i)}
      printMatrix(relevent_history.reshape(2, hist_len).transpose, "Cost vs iter:")
    }

    val cksum = cartesian_dist < threshold
    printArray(w_result, "result: ")
    printArray(W_gold, "gold: ")
    println("Cartesian Distance From W_gold: " + cartesian_dist.to[TM] + " <? " + {threshold.to[TM]})
    println("Finished in " + getArg(E_ACTUAL) + " epochs (out of " + {epochs*len_epoch} + ")")


    println("PASS: " + cksum + " (SVRG)")
  }
}

object LP_SVRG extends SpatialApp {  // Test Args: 


  val tileSize = 64 (16 -> 128)

  val loadPar = 16
  val storePar = 16
  val max_history = 512
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
  val P13 = 1 (1 -> 8)
  val P14 = 1 (1 -> 8)
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

      A ->  (DA,    B)      -> (1/(2^SF*DX*DX), B)

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
  def getValue(table: Matrix[MString], key: MString): Float = {
    val sum = Array.tabulate(table.rows){i => if (table(i,0) == key) table(i,1).to[Float] else 0.to[Float]}.reduce{_+_}
    if (sum == 0.to[Float]) println("WARN: Possibly could not find " + key)
    sum
  }

  @virtualize
  def main() {
    val config = loadCSV2D[MString](sys.env("SPATIAL_HOME") + "/apps/data/training/lp_svrg.config", ",", "\n")
    printMatrix(config, "Config")

    val epochs = getValue(config, "epochs").to[Int]
    val len_epoch = getValue(config, "len_epoch").to[Int]
    val points = getValue(config, "points").to[Int] // Total Points
    val dm = getValue(config, "dm").to[Float]
    val dx = getValue(config, "dx").to[Float]
    val alpha1 = getValue(config, "alpha1").to[Float] // Step size
    val alpha2 = getValue(config, "alpha2").to[Float]
    val bump_epoch = getValue(config, "bump_epoch").to[Int]
    val track = getValue(config, "track").to[Int] // Track cost vs time
    val threshold = getValue(config, "threshold").to[Float] // Cost at which to quit (only quits if track is on)
    val variance = getValue(config, "variance").to[Int] // numerator for noise
    val warmup = getValue(config, "warmup").to[Int]

    val da = 1/(scala.math.pow(2.0,shift_factor).to[Float]*dx*dx)
    val maxX = 6
    val D = 128

    val noise_num = variance
    val noise_denom = 10
    
    // Generate some test data
    val sX = (0::points, 0::D){(i,j) => (random[Float](maxX) - (maxX/3).to[Float])}
    val W_gold = Array.tabulate(D) { i => (random[Float](3) / 2)}
    val Y_noise = Array.tabulate(points) { i => (random[Int](noise_num).to[Float] - (noise_num.to[Float]/2)) / noise_denom.to[Float] }
    val sY = Array.tabulate(points) { i => Array.tabulate(D){j => (W_gold(j) * sX(i,j))}.reduce{_+_} + Y_noise(i) }

    // Convert data to LP
    val W_bits = Array.tabulate(D) { i => FloatToLP[B](W_gold(i), dm, 8)}

    val Y_noise_bits = Array.tabulate(points) {i => FloatToLP[BB](Y_noise(i), dx*dm, 16)}
    val X_bits = (0::points, 0::D){(i,j) => FloatToLP[B](sX(i,j), dx, 8)}
    val Y_bits = Array.tabulate(points){ i => FloatToLP[BB](sY(i), dx*dm, 16)}
    val alpha1_bits = FloatToLP[B](alpha1, da, 8)
    val alpha2_bits = FloatToLP[B](alpha2, da, 8)

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
    println("Alpha1 bits: " + alpha1_bits)
    println("Alpha2 bits: " + alpha2_bits)
    if (points < 10) printMatrix(X_bits, "X Data")
    else {
      printMatrix((0::10, 0::D){(i,j) => X_bits(i,j)}, "X Data")
      println("... Skipped last " + {points-10} + " rows")
    }
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
    val A2 = ArgIn[B]
    val BUMP_EPOCH = ArgIn[Int]
    val TRACK = ArgIn[Int]
    val THRESH = ArgIn[BB]
    val E_ACTUAL = HostIO[Int]
    val WARMUP = ArgIn[Int]

    setArg(E, epochs)
    setArg(N, points)
    setArg(T, len_epoch)
    setArg(A1, alpha1_bits)
    setArg(A2, alpha2_bits)
    setArg(DM,   dm)
    setArg(DX,   dx)
    setArg(DA,   da)
    setArg(DMDX, dm*dx)
    setArg(DG,   dm*dx*dx*da)
    setArg(BUMP_EPOCH, bump_epoch)
    setArg(TRACK, track)
    setArg(THRESH, FloatToLP[BB](threshold, dm*dm, 16))
    setArg(E_ACTUAL, epochs*len_epoch-1)
    setArg(WARMUP, warmup-1)

    val x = DRAM[B](N, D)
    val y = DRAM[BB](N)
    val w = DRAM[B](D)
    val cost = DRAM[BB](max_history)
    val true_w = DRAM[B](D)

    setMem(x, X_bits)
    setMem(y, Y_bits)
    setMem(w, Array.fill(D)(0.to[B]))
    setMem(cost, Array.fill(max_history)(0.to[BB]))
    setMem(true_w, W_bits)

    Accel {
      // Create model and gradient memories
      val w_k = SRAM[B](D) // DM
      val g_k = SRAM[BBBB](D) // DG
      val y_cache = SRAM[BB](tileSize) // DM*DX
      val y_cache_base = Reg[Int](-1) 
      val true_w_sram = SRAM[B](D)
      val cost_sram = SRAM[BB](max_history)
      val w_k_t = SRAM[B](D) // DM

      if (TRACK.value == 1) true_w_sram load true_w(0 :: D)
      w_k load w(0::D par loadPar)
      w_k_t load w(0::D par loadPar)

      // Outer loop (epochs)
      Sequential.Foreach(E by 1 par PX) { e =>
        // Choose correct step for this epoch
        val A = if (e < BUMP_EPOCH) A1.value else A2.value

        if (e > WARMUP.value) {
          // Do full update over all points to get g_k and w_k (outer loop)
          MemReduce(g_k par P3)(N by tileSize par P4){i => 
            val y_tile = SRAM[BB](tileSize) // DM*DX
            y_tile load y(i::i + tileSize par loadPar)
            val g_k_partial = SRAM[BBBB](D)    // DG
            // Full update tile (inner loop)
            MemReduce(g_k_partial par P5)(tileSize by 1 par P6){ii =>
              val x_tile = SRAM[B](D) // DX
              x_tile load x(i+ii, 0::D par loadPar)            
              val g_k_local = SRAM[BBBB](D)  // DG
              val y_hat = Reg[BB](0.to[BB]) // DM*DX
              Reduce(y_hat)(D by 1 par P12){j => w_k(j).to[BB] *! x_tile(j).to[BB]}{_+!_} // DM*DX
              val y_err = y_hat.value -! y_tile(ii) // DM*DX
              Foreach(D by 1 par P7){j => 
                g_k_local(j) =  y_err.to[BBBB] *! x_tile(j).to[BBBB] /& N.value.to[BBBB]
              } // DG
              g_k_local
            }{_+!_}
          }{(a,b) => a +! b}
   
          // Accumulation here may not be necessary
          // // Accumulate g_k into w_k
          // MemFold(w_k par P8)(1 by 1){_ => g_k}{_+!_}
          ()
        }

        // Run len_epoch number of SGD points
        Sequential.Foreach(T by 1 par PX){t => 
          // Choose random point
          val i = (e*T+t) % N

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
          Reduce(y_hat_t)(D by 1 par P13){j => (w_k_t(j).to[BB] *&! x_point(j).to[BB])}{_+!_}
          val y_err_t = y_hat_t.value -! y_point

          // Compute gradient against w_k
          val y_hat_k = Reg[BB](0.to[BB])
          val y_err_k = Reg[BB](0.to[BB])
          if (e > WARMUP.value) {
            Reduce(y_hat_k)(D by 1 par P14){j => (w_k(j).to[BB] *&! x_point(j).to[BB])}{_+!_}
            y_err_k := y_hat_k.value -! y_point            
          }

          // Update w_k_t with reduced variance update
          Foreach(D by 1 par P10){i => w_k_t(i) = toDM(toDG(w_k_t(i)) -! 
                                                    A.to[BBBB] *! (
                                                      (y_err_t.to[BBBB] *! x_point(i).to[BBBB]) +! 
                                                      mux(e > WARMUP.value, -(y_err_k.value.to[BBBB] *&! x_point(i).to[BBBB]) +! g_k(i), 0.to[BBBB])
                                                    )
                                                  )
                                                }

          if (debug) { 
            println("*** Step " + {t + e*T} + ": ")
            println("y_err_t = " + y_err_t + " ( = " + y_hat_t.value + " - " + y_point + "), A = " + A.to[BBBB])
            Foreach(5 by 1) { i => 
              val part1 = y_err_t.to[BBBB] *! x_point(i).to[BBBB]
              val part2 = mux(e > WARMUP.value, -(y_err_k.value.to[BBBB] *&! x_point(i).to[BBBB]), 0.to[BBBB])
              val part3 = mux(e > WARMUP.value, g_k(i), 0.to[BBBB])
              val gradientLP = A.to[BBBB] *! (part1 +! part2 +! part3)

              print(" " + gradientLP + " ( " + part1 + " +! " + part2 + " +! " + part3 + ")")
            }
            println("\nWeights: ")
            Foreach(5 by 1) { i => 
              print(" " + w_k_t(i))
            }
            println("\n")
          }


          if (TRACK.value == 1) {
            val current_cost = Reduce(Reg[BB](0))(D by 1){i => pow((w_k_t(i) - true_w_sram(i)).to[BB], 2)}{_+!_}
            cost_sram(min((max_history-1).to[Int], e*T+t)) = current_cost
            if (current_cost < THRESH) {
              E_ACTUAL := min(e*T+t, (max_history-1).to[Int])
              cost(0 :: min((max_history-1).to[Int], getArg(E_ACTUAL))) store cost_sram
              w(0 :: D par storePar) store w_k_t

              breakpoint()
            }
          }

        }
        // Copy w_k_t to w_k
        Foreach(D by 1 par P11){i => w_k(i) = w_k_t(i)}
      }

      // Store back values
      if (TRACK.value == 1) cost(0 :: min((max_history-1).to[Int], getArg(E_ACTUAL))) store cost_sram 
      w(0 :: D par storePar) store w_k
    }

    
    val w_result = getMem(w)

    val w_result_fullprecision = Array.tabulate(D){i => LPToFloat[B](w_result(i), dm, 8)}
    val cartesian_dist = W_gold.zip(w_result_fullprecision) { case (a, b) => pow(a - b, 2) }.reduce{_+_}
    val cksum =  cartesian_dist < threshold
    printArray(w_result_fullprecision, "result: ")
    printArray(W_gold, "gold: ")
    println("Finished in " + getArg(E_ACTUAL) + " epochs (out of " + {epochs*len_epoch} + ")")

    if (track == 1) {
      val cost_result = getMem(cost)
      val hist_len = min(getArg(E_ACTUAL), max_history.to[Int])
      val relevent_history_LP = Array.tabulate(hist_len){i => cost_result(i)}
      val relevent_history = Array.tabulate(hist_len){i => LPToFloat[BB](cost_result(i), dm*dm, 16)}
      printMatrix(relevent_history.reshape(hist_len, 1), "Cost vs iter:")
      printMatrix(relevent_history_LP.reshape(hist_len, 1), "Cost vs iter (LP):")
    }

    println("Cartesian Distance From W_gold: " + cartesian_dist + " <? " + {threshold.to[Float]})

    println("PASS: " + cksum + " (LP_SVRG)")
  }
}


object HALP extends SpatialApp {  // Test Args: 30 2 256 0.05 1 0.00003 0.4

  val tileSize = 64 (16 -> 128)

  val loadPar = 16
  val storePar = 16
  val max_history = 512
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
  val P13 = 1 (1 -> 8)
  val P14 = 1 (1 -> 8)
  val PX = 1

  val init_SF = 16
  val SF_lower_bound = 16
  val SF_upper_bound = 26
  val init_SFE = 2
  val SFE_lower_bound = 1
  val SFE_upper_bound = 11

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


    We choose:
      DX
      DM
      shift_factor

    We derive:
      DY = DM*DX
      DYE = DM*2^-SFE*DX
      DA = 2^-shift_factor / DX / DX
      DG = DM*DX*DX*DA
      DME = DM*2^-SFE

    We try rescale:
      DG to keep gradients in range
      DME to keep model well resolved
  */

  @virtualize
  def toDM(in: BBBB, sf: Int): B = { 
    val shift_factor_range = SF_upper_bound - SF_lower_bound
    val options = List.tabulate(shift_factor_range){i => 
      ((in + random[BBBB](scala.math.pow(2.0,(i+SF_lower_bound)).to[BBBB])) >> (i+SF_lower_bound)).to[B]   
    }
    if (sf == SF_lower_bound) options(0)
    else if (sf == (SF_lower_bound + 1)) options(1)
    else if (sf == (SF_lower_bound + 2)) options(2)
    else if (sf == (SF_lower_bound + 3)) options(3)
    else if (sf == (SF_lower_bound + 4)) options(4)
    else if (sf == (SF_lower_bound + 5)) options(5)
    else if (sf == (SF_lower_bound + 6)) options(6)
    else if (sf == (SF_lower_bound + 7)) options(7)
    else if (sf == (SF_lower_bound + 8)) options(8)
    else options(9)
  }
  @virtualize
  def toDG(in: B, sf: Int): BBBB = { 
    val options = List.tabulate(SF_upper_bound - SF_lower_bound){i => 
      in.to[BBBB] << (i+SF_lower_bound)
    }
    val selects = List.tabulate(SF_upper_bound - SF_lower_bound){i => (sf-SF_lower_bound) == i}
    if (sf == SF_lower_bound) options(0)
    else if (sf == (SF_lower_bound + 1)) options(1)
    else if (sf == (SF_lower_bound + 2)) options(2)
    else if (sf == (SF_lower_bound + 3)) options(3)
    else if (sf == (SF_lower_bound + 4)) options(4)
    else if (sf == (SF_lower_bound + 5)) options(5)
    else if (sf == (SF_lower_bound + 6)) options(6)
    else if (sf == (SF_lower_bound + 7)) options(7)
    else if (sf == (SF_lower_bound + 8)) options(8)
    else options(9)

  }

  @virtualize 
  def FloatToLP[T:Type:Num](in: Float, delta: Float, precision: scala.Int): T = {
    val exact = in / delta
    
    if (exact < -scala.math.pow(2,(precision-1))) -(scala.math.pow(2,(precision-1))).to[T]
    else if (exact > (scala.math.pow(2, (precision-1))-1).to[Float]) (scala.math.pow(2, (precision-1))-1).to[T]
    else (exact + random[Float](1)).to[T]
  }

  @virtualize 
  def LPToFloat[T:Type:Num](in: T, delta: Float, precision: scala.Int): Float = {delta * in.to[Float]}

  @virtualize
  def getValue(table: Matrix[MString], key: MString): Float = {
    val sum = Array.tabulate(table.rows){i => if (table(i,0) == key) table(i,1).to[Float] else 0.to[Float]}.reduce{_+_}
    if (sum == 0.to[Float]) println("WARN: Possibly could not find " + key)
    sum
  }

  @virtualize
  def main() {
    val config = loadCSV2D[MString](sys.env("SPATIAL_HOME") + "/apps/data/training/halp.config", ",", "\n")
    printMatrix(config, "Config")

    val epochs = getValue(config, "epochs").to[Int]
    val len_epoch = getValue(config, "len_epoch").to[Int] // Total Points
    val points = getValue(config, "points").to[Int] // Total Points
    val dm = getValue(config, "dm").to[Float]
    val dx = getValue(config, "dx").to[Float]
    val mu = getValue(config, "mu").to[Float]
    val alpha1 = getValue(config, "alpha1").to[Float] // Step size
    val alpha2 = getValue(config, "alpha2").to[Float]
    val bump_epoch = getValue(config, "bump_epoch").to[Int]
    val track = getValue(config, "track").to[Int] // Track cost vs time
    val threshold = getValue(config, "threshold").to[Float] // Cost at which to quit (only quits if track is on)
    val variance = getValue(config, "variance").to[Int] // numerator for noise
    val warmup = getValue(config, "warmup").to[Int]
    val allow_recenter = getValue(config, "allow_recenter").to[Int]

    val da = 1/(scala.math.pow(2.0,init_SF).to[Float]*dx*dx)
    val maxX = 6
    val D = 128

    val noise_num = variance
    val noise_denom = 10
    
    // Generate some test data
    val sX = (0::points, 0::D){(i,j) => (random[Float](maxX) - (maxX/3).to[Float])}
    val W_gold = Array.tabulate(D) { i => (random[Float](3) / 2)}
    val Y_noise = Array.tabulate(points) { i => (random[Int](noise_num).to[Float] - (noise_num.to[Float]/2)) / noise_denom.to[Float] }
    val sY = Array.tabulate(points) { i => Array.tabulate(D){j => (W_gold(j) * sX(i,j))}.reduce{_+_} + Y_noise(i) }

    // Convert data to LP
    val W_bits = Array.tabulate(D) { i => FloatToLP[B](W_gold(i), dm, 8)}

    val Y_noise_bits = Array.tabulate(points) {i => FloatToLP[BB](Y_noise(i), dx*dm, 16)}
    val X_bits = (0::points, 0::D){(i,j) => FloatToLP[B](sX(i,j), dx, 8)}
    val Y_bits = Array.tabulate(points){ i => FloatToLP[BB](sY(i), dx*dm, 16)}
    val alpha1_bits = FloatToLP[BB](alpha1, da, 16)
    val alpha2_bits = FloatToLP[BB](alpha2, da, 16)
    val thresh_bits = FloatToLP[BB](threshold.to[Float], dm*dm, 16)

    // Debug
    val W_recompute = Array.tabulate(D) { i => LPToFloat[B](W_bits(i), dm, 8)}
    printArray(W_gold, "W_gold")
    printArray(W_bits, "W_bits")
    printArray(W_recompute, "W_gold Reconstructed")
    println("dm = " + dm)
    println("dx = " + dx)
    println("dm*dx = " + dm*dx)
    if (points < 10) printMatrix(X_bits, "X Data")
    else {
      printMatrix((0::10, 0::D){(i,j) => X_bits(i,j)}, "X Data")
      println("... Skipped last " + {points-10} + " rows")
    }
    printArray(Y_bits, "Y_bits")
    printArray(W_bits, "W_bits")
    printArray(Y_noise_bits, "Y_noise_bits")
    println("alpha1 = " + alpha1_bits)
    println("alpha2 = " + alpha2_bits)

    val mu_setpoint = (mu*(pow(2.0.to[Float],31.to[Float]) - 1)).to[BBBB]
 
    val E = ArgIn[Int] // current epoch
    val E_ACTUAL = HostIO[Int]
    val N = ArgIn[Int]
    val T = ArgIn[Int]
    val DM = HostIO[Float]
    val DME = HostIO[Float]
    val DX = HostIO[Float]
    val DMDX = HostIO[Float]
    val DG = HostIO[Float]
    val DA = HostIO[Float]
    val A1 = HostIO[BB]
    val A2 = HostIO[BB]
    val BUMP_EPOCH = ArgIn[Int]
    val MU = ArgIn[Float]
    val MU_SETPOINT = ArgIn[BBBB]
    val PHASE = ArgIn[Int]
    val TRACK = ArgIn[Int]
    val THRESH = ArgIn[BB]
    val SF = HostIO[Int]
    val SF_EXTRA = HostIO[Int]
    val INIT_SF = ArgIn[Int]
    val WARMUP = ArgIn[Int]
    val ALLOW_RECENTER = ArgIn[Int]

    setArg(E, epochs)
    setArg(N, points)
    setArg(E_ACTUAL, len_epoch * epochs-1)
    setArg(T, len_epoch)
    setArg(DM,   dm)
    setArg(DX,   dx)
    setArg(DMDX, dm*dx)
    setArg(MU, mu)
    setArg(MU_SETPOINT, mu_setpoint)
    setArg(SF, init_SF)
    setArg(INIT_SF, init_SF)
    setArg(SF_EXTRA, init_SFE)
    setArg(A1, alpha1_bits)
    setArg(A2, alpha2_bits)
    setArg(BUMP_EPOCH, bump_epoch)
    setArg(TRACK, track)
    setArg(THRESH, thresh_bits)
    setArg(DA, da)
    setArg(WARMUP, warmup-1)
    setArg(ALLOW_RECENTER, allow_recenter)

    val x = DRAM[B](N, D)
    val y = DRAM[BB](N)
    val w = DRAM[B](D)
    val true_w = DRAM[B](D)
    val g = DRAM[BBBB](D)
    val cost = DRAM[BB](max_history)
    val recenter_hist = DRAM[Int](SF_upper_bound - SF_lower_bound + 1)

    setMem(x, X_bits)
    setMem(y, Y_bits)
    setMem(w, Array.tabulate[B](D){i => 0})
    setMem(true_w, W_bits)
    
    Accel {

      // Create model and gradient memories
      val w_k = SRAM[B](D) // DM
      val w_k_extra = SRAM[B](D) // DME
      val g_k = SRAM[BBBB](D) // DG
      val y_cache = SRAM[BB](tileSize) // DM*DX
      val y_cache_base = Reg[Int](-1) 
      val w_k_t = SRAM[B](D) // DM
      val w_k_t_extra = SRAM[B](D)
      val cost_sram = SRAM[BB](max_history)
      val true_w_sram = SRAM[B](D)
      val recenter_hist_sram = SRAM[Int](SF_upper_bound - SF_lower_bound + 1)
      val recenter_hist_ptr = Reg[Int](0)

      Parallel{
        w_k load w(0 :: D par loadPar)
        g_k load g(0 :: D par loadPar)
        w_k_t load w(0 :: D par loadPar)
        Foreach(SF_upper_bound - SF_lower_bound + 1 by 1){i => recenter_hist_sram(i) = 999}
        Pipe{if (TRACK.value == 1) {
          true_w_sram load true_w(0::D)
        }}
        Foreach(D by 1){i => w_k_extra(i) = 0; w_k_t_extra(i) = 0}

      }

      Sequential.Foreach(E by 1 par PX){e =>
        // println("Epoch: " + e)
        val A = if (e < BUMP_EPOCH) A1.value else A2.value
        // Do full update over all points to get g_k and w_k (outer loop)
        if (e > WARMUP.value) {
          MemReduce(g_k par P3)(N by tileSize par P4){i => 
            val y_tile = SRAM[BB](tileSize) // DM*DX
            y_tile load y(i::i + tileSize par loadPar)
            val g_k_partial = SRAM[BBBB](D)    // DG
            // Full update tile (inner loop)
            MemReduce(g_k_partial par P5)(tileSize by 1 par P6){ii =>
              val x_tile = SRAM[B](D) // DX
              x_tile load x(i+ii, 0::D par loadPar)              
              val g_k_local = SRAM[BBBB](D)  // DG
              val y_hat = Reg[BB](0.to[BB]) // DM*DX
              Reduce(y_hat)(D by 1 par P12){j => w_k(j).to[BB] *! x_tile(j).to[BB]}{_+!_} // DM*DX
              val y_hat_extra = Reg[BB](0.to[BB])
              Reduce(y_hat_extra)(D by 1 par P12){j => w_k_extra(j).to[BB] *! x_tile(j).to[BB]}{_+!_} // DME*DX
              val y_err = y_hat.value -! y_tile(ii) // DME*DX
              Foreach(D by 1 par P7){j => 
                g_k_local(j) =  y_err.to[BBBB] *! x_tile(j).to[BBBB] /& N.value.to[BBBB]
              } // DG
              g_k_local
            }{_+!_}
          }{(a,b) => a +! b}
          ()
        }

        // Recenter
        if (e > 0 && e > WARMUP.value && ALLOW_RECENTER.value == 1) {
          val gradient_mag = Reg[BBBB](0)
          Sequential{
            Reduce(gradient_mag)(D by 1 par 1){i => pow(g_k(i),2)}{_+!_}
            gradient_mag := gradient_mag.value / D
          } 
          if (debug) println("Gradient magnitude: " + gradient_mag.value + " <?> " + MU_SETPOINT.value)
          if (gradient_mag.value >> 1 <= MU_SETPOINT && gradient_mag.value <= MU_SETPOINT && SF.value < SF_upper_bound) {
            if (A1.value < 0x4000.to[BB]) A1 := A1.value << 1
            if (A2.value < 0x4000.to[BB]) A2 := A2.value << 1
            SF := SF.value + 1
            if (debug) println("recentering ++")
            Pipe{recenter_hist_sram(recenter_hist_ptr) = e; recenter_hist_ptr :+= 1}
            ()
          }
          else if (gradient_mag.value << 1 >= MU_SETPOINT && gradient_mag.value >= MU_SETPOINT  && SF.value > SF_lower_bound) {
            if (A1.value != 1.to[BB]) A1 := A1.value >> 1
            if (A2.value != 1.to[BB]) A2 := A2.value >> 1
            SF := SF.value - 1
            if (debug) println("recentering --")
            Pipe{recenter_hist_sram(recenter_hist_ptr) = -e; recenter_hist_ptr :+= 1}
            ()
          }
          else {
            if (debug) println("No recentering")
            ()
          }
        }

        val A_RECENTERED = if (e < BUMP_EPOCH) A1.value else A2.value


        // Run len_epoch number of SGD points
        Sequential.Foreach(T by 1 par PX){t => 
          // Choose random point
          // val i = random[Int](8192) % N
          val i = (e*T+t) % N

          if (debug) println("Randomly chose point " + i)
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
          Reduce(y_hat_t)(D by 1 par P13){j => (w_k_t(j).to[BB] *&! x_point(j).to[BB])}{_+!_}
          val y_hat_t_extra = Reg[BB](0.to[BB])
          Reduce(y_hat_t_extra)(D by 1 par P13){j => (w_k_t_extra(j).to[BB] *&! x_point(j).to[BB])}{_+!_}
          val y_err_t = y_hat_t.value -! y_point

          // Compute gradient against w_k
          val y_hat_k = Reg[BB](0.to[BB])
          val y_hat_k_extra = Reg[BB](0.to[BB])
          val y_err_k = Reg[BB](0.to[BB])
          if (e > WARMUP.value) {
            Reduce(y_hat_k)(D by 1 par P14){j => (w_k(j).to[BB] *&! x_point(j).to[BB])}{_+!_}
            Reduce(y_hat_k_extra)(D by 1 par P14){j => (w_k_extra(j).to[BB] *&! x_point(j).to[BB])}{_+!_}
            y_err_k := y_hat_k.value -! y_point            
          }

          // Update w_k_t with reduced variance update
          Foreach(D by 1 par P10){i => 
            val gradient = A_RECENTERED.to[BBBB] *! (
                            (y_err_t.to[BBBB] *! x_point(i).to[BBBB]) +! 
                            mux(e > WARMUP.value, -(y_err_k.value.to[BBBB] *&! x_point(i).to[BBBB]) +! g_k(i), 0.to[BBBB])
                           )
            w_k_t(i) = toDM(toDG(w_k_t(i), SF.value) -! gradient, SF.value) 
          }

          if (debug) { 
            println("*** Step " + {t + e*T} + ": ")
            println("y_err_t = " + y_err_t + " ( = " + y_hat_t.value + " - " + y_point + "), A = " + A_RECENTERED.to[BBBB])
            Foreach(5 by 1) { i => 
              val part1 = y_err_t.to[BBBB] *! x_point(i).to[BBBB]
              val part2 = mux(e > WARMUP.value, -(y_err_k.value.to[BBBB] *&! x_point(i).to[BBBB]), 0.to[BBBB])
              val part3 = mux(e > WARMUP.value, g_k(i), 0.to[BBBB])
              val gradientLP = A.to[BBBB] *! (part1 +! part2 +! part3)

              print(" " + gradientLP + " ( " + part1 + " +! " + part2 + " +! " + part3 + ")")
            }
            println("\nWeights: ")
            Foreach(5 by 1) { i => 
              print(" " + w_k_t(i))
            }
            println("\n")
          }
          if (TRACK.value == 1) {
            val current_cost = Reduce(Reg[BB](0))(D by 1){i => pow((w_k_t(i) - true_w_sram(i)).to[BB],2)}{_+!_}
            cost_sram(min((max_history-1).to[Int], e*T + t)) = current_cost
            if (current_cost < THRESH) {
              E_ACTUAL := min(e*T+t, (max_history-1).to[Int])
              cost(0 :: min((max_history-1).to[Int], getArg(E_ACTUAL))) store cost_sram
              w(0 :: D par storePar) store w_k_t

              breakpoint()
            }
          }
        }
        // Copy back
        Foreach(D by 1 par 1) {i => w_k(i) = w_k_t(i)}
      }
      // Store back values
      Parallel {
        Pipe{if (TRACK.value == 1) cost(0 :: E*T) store cost_sram}
        recenter_hist store recenter_hist_sram
        w(0 :: D par storePar) store w_k_t          
      }

    } // close Accel
    
    val w_result = getMem(w)

    val w_result_fullprecision = Array.tabulate(D){i => LPToFloat[B](w_result(i), dm, 8)}
    val cartesian_dist = W_gold.zip(w_result_fullprecision) { case (a, b) => (a - b) * (a - b) }.reduce{_+_}
    val cksum =  cartesian_dist < threshold.to[Float]
    printArray(w_result_fullprecision, "result: ")
    printArray(W_gold, "gold: ")
    println("Cartesian Distance From W_gold: " + cartesian_dist + " <? " + {threshold.to[Float]})

    if (track == 1) {
      val cost_result = getMem(cost)
      val sf_jumps = getMem(recenter_hist)
      val hist_len = min(getArg(E_ACTUAL), max_history.to[Int])
      printArray(sf_jumps, "Jumped at: ")
      val shift_factors = Array.tabulate[BB](hist_len){ i =>
        val adjustment = sf_jumps.map{ s => if (abs(s*len_epoch) <= i) {
          if (s > 0) 1
          else -1
        } else {
          0
        }}.reduce{_+_}
        (init_SF + adjustment).to[BB]

      }
      val relevent_history_LP = shift_factors ++ 
                                Array.tabulate[BB](hist_len){i => i.to[BB]} ++ 
                                Array.tabulate(hist_len){i => cost_result(i)}
      printMatrix(relevent_history_LP.reshape(3, hist_len).transpose, "Cost vs iter bits (shift factor, epoch, costLP):")
      val relevent_history = Array.tabulate(hist_len){i => LPToFloat[BB](cost_result(i), dm*dm, 16)}
      printMatrix(relevent_history.reshape(hist_len, 1), "Cost vs iter (Float):")
    }

    println("PASS: " + cksum + " (HALP)")
  }
}

// object LM_HALP extends SpatialApp {  // Test Args: 30 2 256 0.05 1 0.00003 0.4

//   val tileSize = 64 (16 -> 128)

//   val loadPar = 16
//   val storePar = 16
//   val max_history = 512
//   val P3 =  1 (1 -> 8)
//   val P4 =  1 (1 -> 8)
//   val P5 =  1 (1 -> 8)
//   val P6 =  1 (1 -> 8)
//   val P7 =  1 (1 -> 8)
//   val P8 =  1 (1 -> 8)
//   val P9 =  1 (1 -> 8)
//   val P10 = 1 (1 -> 8)
//   val P11 = 1 (1 -> 8)
//   val P12 = 1 (1 -> 8)
//   val P13 = 1 (1 -> 8)
//   val P14 = 1 (1 -> 8)
//   val PX = 1

//   val init_SF = 16
//   val SF_lower_bound = 16
//   val SF_upper_bound = 26
//   val init_SFE = 2
//   val SFE_lower_bound = 1
//   val SFE_upper_bound = 11

//   type XT = Int8
//   type B = Int8
//   type BB = Int16
//   type BBBB = Int32

//   val debug = true
//   /*

//     Basic type relationships:
//       X ->  (DX,    B)
//       W ->  (DM,    B)
//       Y ->  (DM*DX, BB)
//       GR -> (DG,    BBBB)   -> (DM*DX*DX*DA, BBBB)

//       ... Choose DA so that DG is off by a power of 2 from DM.  I.E.- DM = DG*2^8 if shift_factor=8 ...

//       A ->  (DA,    B)      -> (1/(2^8*DX*DX), B)


//     We choose:
//       DX
//       DM
//       shift_factor

//     We derive:
//       DY = DM*DX
//       DYE = DM*2^-SFE*DX
//       DA = 2^-shift_factor / DX / DX
//       DG = DM*DX*DX*DA
//       DME = DM*2^-SFE

//     We try rescale:
//       DG to keep gradients in range
//       DME to keep model well resolved
//   */

//   @virtualize
//   def toDM(in: BBBB, sf: Int): B = { 
//     val shift_factor_range = SF_upper_bound - SF_lower_bound
//     val options = List.tabulate(shift_factor_range){i => 
//       ((in + random[BBBB](scala.math.pow(2.0,(i+SF_lower_bound)).to[BBBB])) >> (i+SF_lower_bound)).to[B]   
//     }
//     if (sf == SF_lower_bound) options(0)
//     else if (sf == (SF_lower_bound + 1)) options(1)
//     else if (sf == (SF_lower_bound + 2)) options(2)
//     else if (sf == (SF_lower_bound + 3)) options(3)
//     else if (sf == (SF_lower_bound + 4)) options(4)
//     else if (sf == (SF_lower_bound + 5)) options(5)
//     else if (sf == (SF_lower_bound + 6)) options(6)
//     else if (sf == (SF_lower_bound + 7)) options(7)
//     else if (sf == (SF_lower_bound + 8)) options(8)
//     else options(9)
//   }
//   @virtualize
//   def toDG(in: B, sf: Int): BBBB = { 
//     val options = List.tabulate(SF_upper_bound - SF_lower_bound){i => 
//       in.to[BBBB] << (i+SF_lower_bound)
//     }
//     val selects = List.tabulate(SF_upper_bound - SF_lower_bound){i => (sf-SF_lower_bound) == i}
//     if (sf == SF_lower_bound) options(0)
//     else if (sf == (SF_lower_bound + 1)) options(1)
//     else if (sf == (SF_lower_bound + 2)) options(2)
//     else if (sf == (SF_lower_bound + 3)) options(3)
//     else if (sf == (SF_lower_bound + 4)) options(4)
//     else if (sf == (SF_lower_bound + 5)) options(5)
//     else if (sf == (SF_lower_bound + 6)) options(6)
//     else if (sf == (SF_lower_bound + 7)) options(7)
//     else if (sf == (SF_lower_bound + 8)) options(8)
//     else options(9)

//   }

//   @virtualize 
//   def FloatToLP[T:Type:Num](in: Float, delta: Float, precision: scala.Int): T = {
//     val exact = in / delta
    
//     if (exact < -scala.math.pow(2,(precision-1))) -(scala.math.pow(2,(precision-1))).to[T]
//     else if (exact > (scala.math.pow(2, (precision-1))-1).to[Float]) (scala.math.pow(2, (precision-1))-1).to[T]
//     else (exact + random[Float](1)).to[T]
//   }

//   @virtualize 
//   def LPToFloat[T:Type:Num](in: T, delta: Float, precision: scala.Int): Float = {delta * in.to[Float]}

//   @virtualize
//   def getValue(table: Matrix[MString], key: MString): Float = {
//     val sum = Array.tabulate(table.rows){i => if (table(i,0) == key) table(i,1).to[Float] else 0.to[Float]}.reduce{_+_}
//     if (sum == 0.to[Float]) println("WARN: Possibly could not find " + key)
//     sum
//   }

//   @virtualize
//   def main() {
//     val config = loadCSV2D[MString](sys.env("SPATIAL_HOME") + "/apps/data/training/halp.config", ",", "\n")
//     printMatrix(config, "Config")

//     val epochs = getValue(config, "epochs").to[Int]
//     val len_epoch = getValue(config, "len_epoch").to[Int] // Total Points
//     val points = getValue(config, "points").to[Int] // Total Points
//     val dm = getValue(config, "dm").to[Float]
//     val dx = getValue(config, "dx").to[Float]
//     val mu = getValue(config, "mu").to[Float]
//     val alpha1 = getValue(config, "alpha1").to[Float] // Step size
//     val alpha2 = getValue(config, "alpha2").to[Float]
//     val bump_epoch = getValue(config, "bump_epoch").to[Int]
//     val track = getValue(config, "track").to[Int] // Track cost vs time
//     val threshold = getValue(config, "threshold").to[Float] // Cost at which to quit (only quits if track is on)
//     val variance = getValue(config, "variance").to[Int] // numerator for noise
//     val warmup = getValue(config, "warmup").to[Int]
//     val allow_recenter = getValue(config, "allow_recenter").to[Int]

//     val da = 1/(scala.math.pow(2.0,init_SF).to[Float]*dx*dx)
//     val maxX = 6
//     val D = 128

//     val noise_num = variance
//     val noise_denom = 10
    
//     // Generate some test data
//     val sX = (0::points, 0::D){(i,j) => (random[Float](maxX) - (maxX/3).to[Float])}
//     val W_gold = Array.tabulate(D) { i => (random[Float](3) / 2)}
//     val Y_noise = Array.tabulate(points) { i => (random[Int](noise_num).to[Float] - (noise_num.to[Float]/2)) / noise_denom.to[Float] }
//     val sY = Array.tabulate(points) { i => Array.tabulate(D){j => (W_gold(j) * sX(i,j))}.reduce{_+_} + Y_noise(i) }

//     // Convert data to LP
//     val W_bits = Array.tabulate(D) { i => FloatToLP[B](W_gold(i), dm, 8)}

//     val Y_noise_bits = Array.tabulate(points) {i => FloatToLP[BB](Y_noise(i), dx*dm, 16)}
//     val X_bits = (0::points, 0::D){(i,j) => FloatToLP[B](sX(i,j), dx, 8)}
//     val Y_bits = Array.tabulate(points){ i => FloatToLP[BB](sY(i), dx*dm, 16)}
//     val alpha1_bits = FloatToLP[BB](alpha1, da, 16)
//     val alpha2_bits = FloatToLP[BB](alpha2, da, 16)
//     val thresh_bits = FloatToLP[BB](threshold.to[Float], dm*dm, 16)

//     // Debug
//     val W_recompute = Array.tabulate(D) { i => LPToFloat[B](W_bits(i), dm, 8)}
//     printArray(W_gold, "W_gold")
//     printArray(W_bits, "W_bits")
//     printArray(W_recompute, "W_gold Reconstructed")
//     println("dm = " + dm)
//     println("dx = " + dx)
//     println("dm*dx = " + dm*dx)
//     if (points < 10) printMatrix(X_bits, "X Data")
//     else {
//       printMatrix((0::10, 0::D){(i,j) => X_bits(i,j)}, "X Data")
//       println("... Skipped last " + {points-10} + " rows")
//     }
//     printArray(Y_bits, "Y_bits")
//     printArray(W_bits, "W_bits")
//     printArray(Y_noise_bits, "Y_noise_bits")
//     println("alpha1 = " + alpha1_bits)
//     println("alpha2 = " + alpha2_bits)

//     val mu_setpoint = (mu*(pow(2.0.to[Float],31.to[Float]) - 1)).to[BBBB]
 
//     val E = ArgIn[Int] // current epoch
//     val E_ACTUAL = HostIO[Int]
//     val N = ArgIn[Int]
//     val T = ArgIn[Int]
//     val DM = HostIO[Float]
//     val DME = HostIO[Float]
//     val DX = HostIO[Float]
//     val DMDX = HostIO[Float]
//     val DG = HostIO[Float]
//     val DA = HostIO[Float]
//     val A1 = HostIO[BB]
//     val A2 = HostIO[BB]
//     val BUMP_EPOCH = ArgIn[Int]
//     val MU = ArgIn[Float]
//     val MU_SETPOINT = ArgIn[BBBB]
//     val PHASE = ArgIn[Int]
//     val TRACK = ArgIn[Int]
//     val THRESH = ArgIn[BB]
//     val SF = HostIO[Int]
//     val SF_EXTRA = HostIO[Int]
//     val INIT_SF = ArgIn[Int]
//     val WARMUP = ArgIn[Int]
//     val ALLOW_RECENTER = ArgIn[Int]

//     setArg(E, epochs)
//     setArg(N, points)
//     setArg(E_ACTUAL, len_epoch * epochs-1)
//     setArg(T, len_epoch)
//     setArg(DM,   dm)
//     setArg(DX,   dx)
//     setArg(DMDX, dm*dx)
//     setArg(MU, mu)
//     setArg(MU_SETPOINT, mu_setpoint)
//     setArg(SF, init_SF)
//     setArg(INIT_SF, init_SF)
//     setArg(SF_EXTRA, init_SFE)
//     setArg(A1, alpha1_bits)
//     setArg(A2, alpha2_bits)
//     setArg(BUMP_EPOCH, bump_epoch)
//     setArg(TRACK, track)
//     setArg(THRESH, thresh_bits)
//     setArg(DA, da)
//     setArg(WARMUP, warmup-1)
//     setArg(ALLOW_RECENTER, allow_recenter)

//     val X_offchip = DRAM[B](N, D)
//     val Y_offchip = DRAM[Float](N)
//     val W_offchip = DRAM[Float](D)
//     val PHI_offchip = DRAM[Float](N)
//     val true_w = DRAM[B](D)
//     val g = DRAM[BBBB](D)
//     val cost = DRAM[BB](max_history)
//     val recenter_hist = DRAM[Int](SF_upper_bound - SF_lower_bound + 1)

//     setMem(X_offchip, X_bits)
//     setMem(Y_offchip, Y_bits)
//     setMem(W_offchip, Array.tabulate[Float](D){i => 0})
//     setMem(true_w, W_bits)
    
//     def computePhi(w: SRAM1[Float], phi: SRAM1[Float]) {
//       Foreach(N par P1) {i => 
//         val phi = SRAM[Float](tileSize)
//         val x = SRAM[XT](D)
//         if (i % tileSize == 0) phi load PHI_offchip(i :: i + tileSize par loadPar)
//         x load X_offchip(i, 0::D par loadPar)
//         val pred = Reduce(Reg[Float](0))(D by 1 par P2) {j => 
//           x(j).to[Float] * w(j)
//         }{_+_}
//         phi(i % tileSize) = pred
//         if (i % tileSize == tileSize-1) PHI_offchip(i-tileSize+1 :: i + 1 par storePar) store phi
//       }
//     }

//     def computeG(gtilde: SRAM1[Float], A: Float) {
//       MemReduce(gtilde)(N par P3) {i => 
//         val g_partial = SRAM[Float](D)
//         val phi = SRAM[Float](tileSize)
//         val y = SRAM[Float](tileSize)
//         val x = SRAM[XT](D)
//         if (i % tileSize == 0) {
//           phi load PHI_offchip(i :: i + tileSize par loadPar)
//           y load Y_offchip(i :: i + tileSize par loadPar)
//         }
//         x load X_offchip(i, 0::D par loadPar)
//         val pt = i % tileSize
//         val diff = phi(pt) - y(pt)
//         Foreach(D by 1 par P4){j => p_partial(j) = diff * x(j).to[Float] * A / N}
//         g_partial
//       }{_+_}
//     }

//     def computeSk(gtilde: SRAM1[Float]): Float = {
//       val norm = Reduce(Reg[Float])(D by 1){i => pow(gtilde(i), 2)}{_+_}
//       norm / (MU * maxValue[USGN].to[Float])
//     }

//     Accel {
//       // Create model and gradient memories
//       val w = SRAM[Float](D) // DM
//       val gtilde = SRAM[Float](D) // DG
//       val phi = SRAM[Float](tileSize)
//       val Sk = Reg[Float]

//       w load W_offchip(0 :: D par loadPar)

//       Sequential.Foreach(K by 1){k => 
//         val A = if (k < BUMP_EPOCH) A1.value else A2.value
//         computePhi(w)
//         computeG(gtilde, A)
//         Sk := computeSk(gtilde)
//         recenterW()
//       }



//     } // close Accel
    
//     val w_result = getMem(W_offchip)

//     val w_result_fullprecision = Array.tabulate(D){i => LPToFloat[B](w_result(i), dm, 8)}
//     val cartesian_dist = W_gold.zip(w_result_fullprecision) { case (a, b) => (a - b) * (a - b) }.reduce{_+_}
//     val cksum =  cartesian_dist < threshold.to[Float]
//     printArray(w_result_fullprecision, "result: ")
//     printArray(W_gold, "gold: ")
//     println("Cartesian Distance From W_gold: " + cartesian_dist + " <? " + {threshold.to[Float]})

//     if (track == 1) {
//       val cost_result = getMem(cost)
//       val sf_jumps = getMem(recenter_hist)
//       val hist_len = min(getArg(E_ACTUAL), max_history.to[Int])
//       printArray(sf_jumps, "Jumped at: ")
//       val shift_factors = Array.tabulate[BB](hist_len){ i =>
//         val adjustment = sf_jumps.map{ s => if (abs(s*len_epoch) <= i) {
//           if (s > 0) 1
//           else -1
//         } else {
//           0
//         }}.reduce{_+_}
//         (init_SF + adjustment).to[BB]

//       }
//       val relevent_history_LP = shift_factors ++ 
//                                 Array.tabulate[BB](hist_len){i => i.to[BB]} ++ 
//                                 Array.tabulate(hist_len){i => cost_result(i)}
//       printMatrix(relevent_history_LP.reshape(3, hist_len).transpose, "Cost vs iter bits (shift factor, epoch, costLP):")
//       val relevent_history = Array.tabulate(hist_len){i => LPToFloat[BB](cost_result(i), dm*dm, 16)}
//       printMatrix(relevent_history.reshape(hist_len, 1), "Cost vs iter (Float):")
//     }

//     println("PASS: " + cksum + " (HALP)")
//   }
// }



object SGD extends SpatialApp { // Test Args: 


  type TM = FixPt[TRUE, _9, _23]
  type TX = FixPt[TRUE, _9, _7]

  val loadPar = 16 (1 -> 32)
  val storePar = 16 (1 -> 32)
  val max_history = 512
  val PX = 1
  val P10 = 4 (1 -> 8)
  val P13 = 4 (1 -> 8)

  val tileSize = 256 //192

  val debug = true

  @virtualize
  def getValue(table: Matrix[MString], key: MString): Float = {
    val sum = Array.tabulate(table.rows){i => if (table(i,0) == key) table(i,1).to[Float] else 0.to[Float]}.reduce{_+_}
    if (sum == 0.to[Float]) println("WARN: Possibly could not find " + key)
    sum
  }

  @virtualize
  def main() {
    val config = loadCSV2D[MString](sys.env("SPATIAL_HOME") + "/apps/data/training/sgd.config", ",", "\n")
    printMatrix(config, "Config")

    val epochs = getValue(config, "epochs").to[Int]
    val points = getValue(config, "points").to[Int] // Total Points
    val alpha1 = getValue(config, "alpha1").to[TM] // Step size
    val alpha2 = getValue(config, "alpha2").to[TM]
    val bump_epoch = getValue(config, "bump_epoch").to[Int]
    val track = getValue(config, "track").to[Int] // Track cost vs time
    val threshold = getValue(config, "threshold").to[TM] // Cost at which to quit (only quits if track is on)
    val variance = getValue(config, "variance").to[Int] // numerator for noise
    val D = 128
    val maxX = 6

    val noise_num = variance
    val noise_denom = 10
    
    // Generate some test data
    val sX = (0::points, 0::D){(i,j) => (random[TX](maxX) - (maxX/3).to[TX])}
    val W_gold = Array.tabulate(D) { i => (random[TM](3) / 2)}
    val Y_noise = Array.tabulate(points) { i => (random[Int](noise_num).to[TM] - (noise_num.to[TM]/2)) / noise_denom.to[TM] }
    val sY = Array.tabulate(points) { i => Array.tabulate(D){j => (W_gold(j) * sX(i,j).to[TM])}.reduce{_+_} + Y_noise(i) }

    val E = ArgIn[Int]
    val N = ArgIn[Int]
    val A1 = ArgIn[TM]
    val A2 = ArgIn[TM]
    val BUMP_EPOCH = ArgIn[Int]
    val TRACK = ArgIn[Int]
    val THRESH = ArgIn[TM]
    val E_ACTUAL = HostIO[Int]

    setArg(E, epochs)
    setArg(N, points)
    setArg(A1, alpha1)
    setArg(A2, alpha2)
    setArg(BUMP_EPOCH, bump_epoch)
    setArg(TRACK, track)
    setArg(THRESH, threshold)
    setArg(E_ACTUAL, epochs-1)

    val x = DRAM[TX](N, D)
    val y = DRAM[TM](N)
    val w = DRAM[TM](D)
    val true_w = DRAM[TM](D)
    val cost = DRAM[TM](max_history)

    setMem(x, sX)
    setMem(y, sY)
    setMem(w, Array.fill(D)(0.to[TM]))
    setMem(cost, Array.fill(max_history)(0.to[TM]))
    setMem(true_w, W_gold)

    Accel {
      val y_cache = SRAM[TM](tileSize)
      val y_cache_base = Reg[Int](-1)
      val w_k = SRAM[TM](D)

      // Debug
      val cost_sram = SRAM[TM](max_history)
      val true_w_sram = SRAM[TM](D)
      if (TRACK.value == 1) true_w_sram load true_w

      w_k load w(0::D par loadPar)

      Sequential.Foreach(E by 1 par PX){e => 
        val A = if (e < BUMP_EPOCH) A1.value else A2.value
        // Choose random point
        val i = random[Int](8912) % N

        if (debug) println("i is " + i)
        // Get y for this point
        val y_point = Reg[TM](0) // DM*DX
        if (i - y_cache_base >= 0 && i - y_cache_base < tileSize && y_cache_base >= 0) {
          y_point := y_cache(i-y_cache_base)
        } else {
          y_cache_base := i - (i % tileSize)
          y_cache load y(y_cache_base::y_cache_base + tileSize par loadPar)
          y_point := y_cache(i % tileSize)
        }

        // Get x for this point
        val x_point = SRAM[TX](D) // DX
        x_point load x(i, 0::D par loadPar)

        // Compute gradient against w_k_t
        val y_hat = Reg[TM](0.to[TM])
        Reduce(y_hat)(D by 1 par P13){j => (w_k(j) *&! x_point(j).to[TM])}{_+!_}
        val y_err = y_hat.value -! y_point

        // Update w_k_t with reduced variance update
        Foreach(D by 1 par P10){i => w_k(i) = w_k(i) -! A.to[TM] *! y_err *! x_point(i).to[TM] }

        if (debug) { 
          println("*** Step " + {e} + ": ")
          println("y_err = " + y_err + " ( = " + y_hat.value + " - " + y_point + ")")
          Foreach(D by 1) { i => 
            val gradient = A.to[TM] *! y_err *! x_point(i).to[TM]

            print(" " + gradient)
          }
          println("\nWeights: ")
          Foreach(D by 1) { i => 
            print(" " + w_k(i))
          }
          println("\n")
        }

        if (TRACK.value == 1) {
          val current_cost = Reduce(Reg[TM](0))(D by 1){i => (w_k(i) - true_w_sram(i)) *! (w_k(i) - true_w_sram(i))}{_+!_}
          cost_sram(min((max_history-1).to[Int], e)) = current_cost
          if (current_cost < THRESH) {
            E_ACTUAL := min(e, (max_history-1).to[Int])
            cost(0 :: E) store cost_sram
            w(0 :: D par storePar) store w_k

            breakpoint()
          }
        }

      }

      if (TRACK.value == 1) cost(0 :: E) store cost_sram
      w(0 :: D par storePar) store w_k

    }

    val result = getMem(w)

    val cartesian_dist = W_gold.zip(result) { case (a, b) => (a - b) * (a - b) }.reduce{_+_}

    printArray(result, "result: ")
    printArray(W_gold, "gold: ")
    println("Dist: " + cartesian_dist + " <? " + threshold.to[TM])
    println("Finished in " + getArg(E_ACTUAL) + " epochs (out of " + getArg(E) + ")")

    if (track == 1) {
      val cost_result = getMem(cost)
      val hist_len = min(getArg(E_ACTUAL), max_history.to[Int])
      val relevent_history = Array.tabulate(hist_len){i => cost_result(i)}
      printMatrix(relevent_history.reshape(hist_len, 1), "Cost vs iter:")
    }


    val cksum = cartesian_dist < threshold.to[TM]
    println("PASS: " + cksum + " (SGD)")
  }
}


object LP_SGD extends SpatialApp {  // Test Args: 


  val tileSize = 64 (16 -> 128)

  val loadPar = 16
  val storePar = 16
  val max_history = 512
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
  val P13 = 1 (1 -> 8)
  val P14 = 1 (1 -> 8)
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
  def getValue(table: Matrix[MString], key: MString): Float = {
    val sum = Array.tabulate(table.rows){i => if (table(i,0) == key) table(i,1).to[Float] else 0.to[Float]}.reduce{_+_}
    if (sum == 0.to[Float]) println("WARN: Possibly could not find " + key)
    sum
  }

  @virtualize
  def main() {

    val config = loadCSV2D[MString](sys.env("SPATIAL_HOME") + "/apps/data/training/lp_sgd.config", ",", "\n")
    printMatrix(config, "Config")

    val epochs = getValue(config, "epochs").to[Int]
    val points = getValue(config, "points").to[Int] // Total Points
    val dm = getValue(config, "dm").to[Float] // delta for model
    val dx = getValue(config, "dx").to[Float] // delta for data
    val alpha1 = getValue(config, "alpha1").to[Float] // Step size
    val alpha2 = getValue(config, "alpha2").to[Float]
    val bump_epoch = getValue(config, "bump_epoch").to[Int]
    val track = getValue(config, "track").to[Int] // Track cost vs time
    val threshold = getValue(config, "threshold").to[Float] // Cost at which to quit (only quits if track is on)
    val variance = getValue(config, "variance").to[Int] // numerator for noise

    val da = 1/(scala.math.pow(2.0,shift_factor).to[Float]*dx*dx)
    val maxX = 15
    val D = 128

    val noise_num = variance
    val noise_denom = 10
    
    // Generate some test data
    val sX = (0::points, 0::D){(i,j) => (random[Float](maxX) - (maxX/4).to[Float])}
    val W_gold = Array.tabulate(D) { i => (random[Float](3) / 2)}
    val Y_noise = Array.tabulate(points) { i => (random[Int](noise_num).to[Float] - (noise_num.to[Float]/2)) / noise_denom.to[Float] }
    val sY = Array.tabulate(points) { i => Array.tabulate(D){j => (W_gold(j) * sX(i,j))}.reduce{_+_} + Y_noise(i) }

    // Convert data to LP
    val W_bits = Array.tabulate(D) { i => FloatToLP[B](W_gold(i), dm, 8)}

    val Y_noise_bits = Array.tabulate(points) {i => FloatToLP[BB](Y_noise(i), dx*dm, 16)}
    val X_bits = (0::points, 0::D){(i,j) => FloatToLP[B](sX(i,j), dx, 8)}
    val Y_bits = Array.tabulate(points){ i => FloatToLP[BB](sY(i), dx*dm, 16)}
    val alpha1_bits = FloatToLP[B](alpha1, da, 8)
    val alpha2_bits = FloatToLP[B](alpha2, da, 8)

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
    println("Alpha1 bits: " + alpha1_bits)
    println("Alpha2 bits: " + alpha2_bits)
    if (points < 10) printMatrix(X_bits, "X Data")
    else {
      printMatrix((0::10, 0::D){(i,j) => X_bits(i,j)}, "X Data")
      println("... Skipped last " + {points-10} + " rows")
    }
    printArray(Y_bits, "Y_bits")
    printArray(W_bits, "W_bits")
    printArray(Y_noise_bits, "Y_noise_bits")


    val E = ArgIn[Int]
    val N = ArgIn[Int]
    val DM = HostIO[Float]
    val DX = HostIO[Float]
    val DMDX = HostIO[Float]
    val DG = HostIO[Float]
    val DA = HostIO[Float]
    val A1 = ArgIn[B]
    val A2 = ArgIn[B]
    val BUMP_EPOCH = ArgIn[Int]
    val TRACK = ArgIn[Int]
    val THRESH = ArgIn[BB]
    val E_ACTUAL = HostIO[Int]

    setArg(E, epochs)
    setArg(N, points)
    setArg(A1, alpha1_bits)
    setArg(A2, alpha2_bits)
    setArg(DM,   dm)
    setArg(DX,   dx)
    setArg(DA,   da)
    setArg(DMDX, dm*dx)
    setArg(DG,   dm*dx*dx*da)
    setArg(BUMP_EPOCH, bump_epoch)
    setArg(TRACK, track)
    setArg(THRESH, FloatToLP[BB](threshold, dm*dm, 16))
    setArg(E_ACTUAL, epochs-1)


    val x = DRAM[B](N, D)
    val y = DRAM[BB](N)
    val w = DRAM[B](D)
    val cost = DRAM[BB](max_history)
    val true_w = DRAM[B](D)

    setMem(x, X_bits)
    setMem(y, Y_bits)
    setMem(w, Array.fill(D)(0.to[B]))
    setMem(cost, Array.fill(max_history)(0.to[BB]))
    setMem(true_w, W_bits)

    Accel {
      // Create model and gradient memories
      val w_k = SRAM[B](D) // DM
      val g_k = SRAM[BBBB](D) // DG
      val y_cache = SRAM[BB](tileSize) // DM*DX
      val y_cache_base = Reg[Int](-1) 
      val true_w_sram = SRAM[B](D)
      val cost_sram = SRAM[BB](max_history)

      if (TRACK.value == 1) true_w_sram load true_w(0 :: D)
      w_k load w(0::D par loadPar)

      // Outer loop (epochs)
      Sequential.Foreach(E by 1 par PX) { e =>
        // Choose correct step for this epoch
        val A = if (E < BUMP_EPOCH) A1.value else A2.value

        // Choose random point
        val i = random[Int](8192) % N

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

        // Compute gradient against w_k
        val y_hat = Reg[BB](0.to[BB])
        Reduce(y_hat)(D by 1 par P13){j => (w_k(j).to[BB] *&! x_point(j).to[BB])}{_+!_}
        val y_err = y_hat.value -! y_point

        // Update w_k with reduced variance update
        Foreach(D by 1 par P10){i => w_k(i) = toDM(toDG(w_k(i)) -! A.to[BBBB] *! y_err.to[BBBB] *! x_point(i).to[BBBB])}

        if (debug) { 
          println("*** Step " + {e} + ": ")
          println("y_err_t = " + y_err + " ( = " + y_hat.value + " - " + y_point + ")")
          Foreach(D by 1) { i => 
            val gradientLP = A.to[BBBB] *! y_err.to[BBBB] *! x_point(i).to[BBBB]

            print(" " + gradientLP)
          }
          println("\nWeights: ")
          Foreach(D by 1) { i => 
            print(" " + w_k(i))
          }
          println("\n")
        }

        if (TRACK.value == 1) {
          val current_cost = Reduce(Reg[BB](0))(D by 1){i => pow((w_k(i) - true_w_sram(i)).to[BB], 2)}{_+!_}
          cost_sram(min((max_history-1).to[Int], e)) = current_cost
          println("Is " + current_cost + " < " + THRESH.value)
          if (current_cost < THRESH) {
            E_ACTUAL := min(e, max_history-1)
            cost(0 :: E_ACTUAL.value) store cost_sram
            w(0 :: D par storePar) store w_k

            breakpoint()
          }
        }

      }

      // Store back values
      if (TRACK.value == 1) cost(0 :: E) store cost_sram 
      w(0 :: D par storePar) store w_k
    }
    
    val w_result = getMem(w)

    val w_result_fullprecision = Array.tabulate(D){i => LPToFloat[B](w_result(i), dm, 8)}
    val cartesian_dist = W_gold.zip(w_result_fullprecision) { case (a, b) => pow(a - b, 2) }.reduce{_+_}
    val cksum =  cartesian_dist < threshold
    printArray(w_result_fullprecision, "result: ")
    printArray(W_gold, "gold: ")
    println("Finished in " + getArg(E_ACTUAL) + " epochs (out of " + {epochs} + ")")

    if (track == 1) {
      val cost_result = getMem(cost)
      val hist_len = min(getArg(E_ACTUAL), max_history.to[Int])
      val relevent_history = Array.tabulate(hist_len){i => LPToFloat[BB](cost_result(i), dm*dm, 16)}
      printMatrix(relevent_history.reshape(hist_len, 1), "Cost vs iter:")
    }

    println("Cartesian Distance From W_gold: " + cartesian_dist + " <? " + {threshold.to[Float]})

    println("PASS: " + cksum + " (LP_SGD)")
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

object SGD_minibatch extends SpatialApp { // Test Args: 40 64 0.0001


  type TM = FixPt[TRUE,_16,_16]
  type TX = FixPt[TRUE,_16,_16]
  val D = 128
  val tileSize = 16
  val innerPar = 4
  val outerPar = 1 // Not used right now?
  val margin = 1.5

  @virtualize
  def sgdminibatch(x_in: Array[TX], y_in: Array[TX], alpha: TM, epochs: Int, nn: Int) = {
    val E = ArgIn[Int]
    val N = ArgIn[Int]
    val A = ArgIn[TM]

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

    val sX = Array.fill(N){ Array.fill(D){ random[TX](3.to[TX]) + 1.to[TX]} }
    val ideal_model = Array.tabulate(D){ i => 2.to[TM] }
    val sY = Array.tabulate(N){i => ideal_model.zip(sX.apply(i)){case (a,b) => a.to[TX]*b}.reduce{_+_}}
    val id = Array.tabulate(D){ i => i }
    val ep = Array.tabulate(E){ i => i }

    val result = sgdminibatch(sX.flatten, sY, A, E, N)

    val cksum = ideal_model.zip(result){ case (a,b) => abs(a - b) < margin.to[TM] }.reduce{_&&_}
    printArr(result, "result: ")
    printArr(ideal_model, "gold: ")
    println("PASS: " + cksum  + " (SGD_minibatch)")
  }
}

object HALP_handoff extends SpatialApp {  // Test Args: 40 3 256 0.05 1 0.00003 0.4

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

  val SF_lower_bound = 15
  val SF_upper_bound = 25

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
    val shift_factor_range = SF_upper_bound - SF_lower_bound
    val options = List.tabulate(shift_factor_range){i => 
      ((in + random[BBBB](scala.math.pow(2.0,(i+SF_lower_bound)).to[BBBB])) >> (i+SF_lower_bound)).to[B]   
    }
    if (sf == SF_lower_bound) options(0)
    else if (sf == (SF_lower_bound + 1)) options(1)
    else if (sf == (SF_lower_bound + 2)) options(2)
    else if (sf == (SF_lower_bound + 3)) options(3)
    else if (sf == (SF_lower_bound + 4)) options(4)
    else if (sf == (SF_lower_bound + 5)) options(5)
    else if (sf == (SF_lower_bound + 6)) options(6)
    else if (sf == (SF_lower_bound + 7)) options(7)
    else if (sf == (SF_lower_bound + 8)) options(8)
    else options(9)
  }
  @virtualize
  def toDG(in: B, sf: Int): BBBB = { 
    val options = List.tabulate(SF_upper_bound - SF_lower_bound){i => 
      in.to[BBBB] << (i+SF_lower_bound)
    }
    val selects = List.tabulate(SF_upper_bound - SF_lower_bound){i => (sf-SF_lower_bound) == i}
    if (sf == SF_lower_bound) options(0)
    else if (sf == (SF_lower_bound + 1)) options(1)
    else if (sf == (SF_lower_bound + 2)) options(2)
    else if (sf == (SF_lower_bound + 3)) options(3)
    else if (sf == (SF_lower_bound + 4)) options(4)
    else if (sf == (SF_lower_bound + 5)) options(5)
    else if (sf == (SF_lower_bound + 6)) options(6)
    else if (sf == (SF_lower_bound + 7)) options(7)
    else if (sf == (SF_lower_bound + 8)) options(8)
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
    val init_SF = 16
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
    val da = 1/(scala.math.pow(2.0,init_SF).to[Float]*dx*dx)
    val alpha1_bits = FloatToLP[B](alpha1, da, 8)

    // Debug
    val W_recompute = Array.tabulate(D) { i => LPToFloat[B](W_bits(i), dm, 8)}
    printArray(W_gold, "W_gold")
    printArray(W_bits, "W_bits")
    printArray(W_recompute, "W_gold Reconstructed")
    println("dm = " + dm)
    println("dx = " + dx)
    println("dm*dx = " + dm*dx)
    if (points < 10) printMatrix(X_bits, "X Data")
    else {
      printMatrix((0::10, 0::D){(i,j) => X_bits(i,j)}, "X Data")
      println("... Skipped last " + {points-10} + " rows")
    }
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
    setArg(SF, init_SF)
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
        if ((1/ratio >= 2 || ratio >= 2) && getArg(SF) < SF_upper_bound && getArg(SF) > SF_lower_bound) { // Apply recentering
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

    println("PASS: " + cksum + " (HALP_handoff)")
  }
}