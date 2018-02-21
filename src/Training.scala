import spatial.dsl._
import org.virtualized._
import spatial.targets._




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


object SGD extends SpatialApp { // Regression (Dense) // Args: 40 64 0.0001


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
          Sequential.Foreach(tileSize by 1) { i =>
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


    // (0 until E) foreach { i =>
    //   (0 until N) foreach { j =>
    //     val y_hat = sX.apply(j).zip(gold) {_*_}.reduce{_+_}
    //     val y_err = sY.apply(j) - y_hat
    //     val update = sX.apply(j).zip(gold){case (x,g) => g + x*A*y_err}
    //     (0 until D) foreach { q => gold(q) = update(q) }
    //   }
    // }

    val cksum = ideal_model.zip(result) { case (a, b) => abs(a - b) < margin }.reduce{_&&_}
    printArr(result, "result: ")
    printArr(ideal_model, "gold: ")
    println("PASS: " + cksum + " (SGD)")
  }
}


object SGD_minibatch extends SpatialApp { // Regression (Dense) // Args: 40 64 0.0001


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
          Sequential.Foreach(tileSize by 1) {i => 
            val y_hat = Reg[TX]
            Reduce(y_hat)(D by 1 par ip){ j => x_tile(i,j) * sgdmodel(j).to[TX] }{_+_}
            y_err(i) = y_tile(i) - y_hat.value
          }
          Sequential.Foreach(D by 1) { i =>
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


    // (0 until E) foreach { i =>
    //   (0 until N) foreach { j =>
    //     val y_hat = sX.apply(j).zip(gold) {_*_}.reduce{_+_}
    //     val y_err = sY.apply(j) - y_hat
    //     val update = sX.apply(j).zip(gold){case (x,g) => g + x*A*y_err}
    //     (0 until D) foreach { q => gold(q) = update(q) }
    //   }
    // }

    val cksum = ideal_model.zip(result){ case (a,b) => abs(a - b) < margin }.reduce{_&&_}
    printArr(result, "result: ")
    printArr(ideal_model, "gold: ")
    println("PASS: " + cksum  + " (SGD_minibatch)")
  }
}

object SVRG extends SpatialApp { 

  type TM = FixPt[TRUE, _16, _48]
  // type TX = FixPt[TRUE, _16, _16]
  val modelSize = 16
  val margin = 1

  val loadPar = 1
  val storePar = 1

  val tileSize = 16 


  @virtualize
  def main() {
    val epochs = args(0).to[Int] // Epochs
    val len_epoch = args(1).to[Int] // Epoch Length
    val points = args(2).to[Int] // Total Points
    val alpha1 = args(3).to[TM] // Step size
    val alpha2 = args(4).to[TM] // Step size
    val D = modelSize
    val bump_epoch = args(5).to[Int]

    val noise_num = 2
    val noise_denom = 10
    // Generate some test data
    val sX = (0::points, 0::D){(i,j) => random[TM](3.to[TM]) + 1.to[TM]}
    val W_gold = Array.tabulate(D) { i => random[TM](3.to[TM]) / 2.to[TM]}
    val sY = Array.tabulate(points) { i => (random[TM](noise_num.to[TM]) / noise_denom - noise_num/2) + Array.tabulate(D){j => W_gold(j) * sX(i,j)}.reduce{_+_} }

    val E = ArgIn[Int]
    val N = ArgIn[Int]
    val T = ArgIn[Int]
    val DBG = ArgIn[Int]
    val BUMP_EPOCH = ArgIn[Int]
    val A1 = ArgIn[TM]
    val A2 = ArgIn[TM]

    setArg(E, epochs)
    setArg(N, points)
    setArg(T, len_epoch)
    setArg(A1, alpha1)
    setArg(A2, alpha2)
    setArg(BUMP_EPOCH, bump_epoch)


    val num_to_track = 512
    val init = Array.fill[TM](num_to_track)(0)

    val x = DRAM[TM](N, D)
    val y = DRAM[TM](N)
    val err = DRAM[TM](num_to_track)
    val result = DRAM[TM](D)

    setMem(x, sX)
    setMem(y, sY)
    setMem(err, init)


    Accel {
      val w_k = SRAM[TM](D)
      val g_k = SRAM[TM](D)
      val debug_err = SRAM[TM](num_to_track)
      Foreach(num_to_track by 1) {i => debug_err(i) = 0.to[TM]}
      Pipe(D by 1) { i => w_k(i) = 0.to[TM] }
      Sequential.Foreach(E by 1) { e =>
        val A = mux(e < BUMP_EPOCH, A1.value, A2.value)
      	// Compute g_k and w_k
      	val avg_err = Reg[TM](0.to[TM])
      	avg_err.reset
      	MemReduce(g_k)(N by tileSize){i => 
      	  val y_tile = SRAM[TM](tileSize)
      	  val x_tile = SRAM[TM](tileSize,D)
      	  y_tile load y(i::i + tileSize par loadPar)
      	  x_tile load x(i::i + tileSize, 0::D par loadPar)
      	  val g_k_partial = SRAM[TM](D)
      	  MemReduce(g_k_partial)(tileSize by 1){ii =>
      	  	val g_k_local = SRAM[TM](D)
      	  	val y_err = Reduce(Reg[TM](0.to[TM]))(D by 1){j => w_k(j) * x_tile(ii, j)}{_+_} - y_tile(ii)
      	  	avg_err := avg_err + (y_err * y_err / N.value.to[TM])
      	  	Foreach(D by 1){j => g_k_local(j) =  -A * y_err * x_tile(ii, j)}
      	  	g_k_local
      	  }{_+_}
      	}{(a,b) => a + b/tileSize.to[TM]}
      	if ((e == 0 && DBG.value == 1) || DBG.value == 0) {
      	  MemFold(w_k)(1 by 1){_ => g_k}{_+_}
      	  ()
      	}
      	debug_err(e) = avg_err.value
      	// Do SGD
      	val w_k_t = SRAM[TM](D)
      	Foreach(D by 1){i => w_k_t(i) = w_k(i)}
      	Foreach(T by 1){t => 
      	  val i = random[Int](1024) % N
      	  val x_point = SRAM[TM](D)
      	  val y_point = RegFile[TM](1) // Incredibly wasteful for now
      	  x_point load x(i, 0::D par loadPar)
      	  y_point load y(i::i+1 par loadPar)
      	  val y_err_t = Reduce(Reg[TM](0.to[TM]))(D by 1){j => w_k_t(j) * x_point(j)}{_+_} - y_point(0)
      	  val y_err_k = Reduce(Reg[TM](0.to[TM]))(D by 1){j => w_k(j) * x_point(j)}{_+_} - y_point(0)
      	  Foreach(D by 1){i => w_k_t(i) = w_k_t(i) - A * (y_err_t * x_point(i) + y_err_k * x_point(i) - g_k(i))}

      	  // Debug
      	  // if (e * T + t + (e+1)< num_to_track) debug_err(e*T + t + (e+1)) = y_err_t * y_err_t
      	}
      	// Copy w_k_t to w_k
      	Foreach(D by 1){i => w_k(i) = w_k_t(i)}
      }
      result(0 :: D par storePar) store w_k
      err(0 :: num_to_track par storePar) store debug_err
    }

    
    val w_result = getMem(result)
    val err_result = getMem(err)

    val cksum = W_gold.zip(w_result) { case (a, b) => (a - b) * (a - b) }.reduce{_+_} < margin
    printArray(err_result, "Error per update")
    printArray(w_result, "result: ")
    printArray(W_gold, "gold: ")
    println("Cartesian Distance From W_gold: " + { W_gold.zip(w_result) { case (a, b) => (a - b).to[TM] * (a - b).to[TM] }.reduce{_+_}.to[TM] } + " <? " + {margin.to[Int]})

    println("PASS: " + cksum + " (SVRG)")
  }
}
