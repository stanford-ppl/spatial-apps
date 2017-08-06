import spatial.dsl._
import org.virtualized._

object Kmeans extends SpatialApp { // Regression (Dense) // Args: 3 64

  type X = Int

  val numcents = 16
  val dim = 32
  val pts_per_ld = 1 // ???

  val ip = 16
  val op = 2

  val element_max = 10
  val margin = (element_max * 0.2).to[X]

  val MAXK = numcents
  val MAXD = dim

  @virtualize
  def kmeans[T:Type:Num](points_in: Array[T], cent_inits: Array[T], numPoints: Int, numCents: Int, numDims: Int, it: Int) = {
    bound(numPoints) = 960000
    bound(numCents) = MAXK
    bound(numDims) = MAXD

    val BN = pts_per_ld (96 -> 96 -> 9600)
    val BD = MAXD
    val PX = 1 (1 -> 1)
    val P0 = ip (32 -> 96 -> 192) // Dimensions loaded in parallel
    val P1 = op (1 -> 12)         // Sets of points calculated in parallel
    val P2 = ip (1 -> 4 -> 96)    // Dimensions accumulated in parallel (outer)
    val P3 = ip (1 -> 4 -> 16)    // Points calculated in parallel
    val PR = ip (1 -> 4 -> 96)
    val P4 = ip (1 -> 4 -> 96)

    val iters = ArgIn[Int]
    val N     = ArgIn[Int]
    val K     = numCents //ArgIn[Int]
    val D     = numDims //ArgIn[Int]

    setArg(iters, it)
    setArg(N, numPoints)
    // setArg(K, numCents)
    // setArg(D, numDims)

    val points = DRAM[T](N, D)    // Input points
    val centroids = DRAM[T](numcents*dim) // Output centroids
    // val init_cents = DRAM[T](K,D) // Output centroids
    setMem(points, points_in)
    // setMem(init_cents, cent_inits)


    Accel {
      val cts = SRAM[T](MAXK, MAXD)
      val newCents = SRAM[T](MAXK,MAXD)

      // Load initial centroids (from points)
      cts load points(0::K, 0::D par 16)

      // Initialize newCents
      // FPGA:
      // Foreach(K by 1, D by 1) {(i,j) => newCents(i,j) = cts(i,j)} 

      val DM1 = D - 1

      Sequential.Foreach(iters by 1){epoch =>

        // Flush centroid accumulator
        // FPGA:
        Foreach(K by 1, D par P0){(ct,d) =>
          newCents(ct,d) = 0.to[T]
        }

        // For each set of points
        Foreach(N by BN par PX){i =>
          val pts = SRAM[T](BN, BD)
          pts load points(i::i+BN, 0::BD par 16)

          // For each point in this set
          MemFold(newCents)(BN par PX){pt =>
            // Find the index of the closest centroid
            val accum = Reg[Tup2[Int,T]]( pack(0.to[Int], 100000.to[T]) )
            val minCent = Reduce(accum)(K par PX){ct =>
              val dist = Reg[T](0.to[T])
              Reduce(dist)(D par P2){d => (pts(pt,d) - cts(ct,d)) ** 2 }{_+_}
              pack(ct, dist.value)
            }{(a,b) =>
              mux(a._2 < b._2, a, b)
            }
            /*// PIR Version
            val minCent = Reg[Int]
            val minDist = Reg[T](100000.to[T])
            Foreach(K par PX){ct =>
              val dist = Reduce(Reg[T])(D par P2){d =>
                val cent = mux(epoch == 0, origCts(ct,d), cts(ct,d))
                (pts(pt,d) - cent) ** 2
              }{_+_}

              Pipe {
                val d = dist.value
                minDist := min(minDist.value, d)
                minCent := mux(minDist.value == d, ct, minCent.value)
              }
            }*/

            // Store this point to the set of accumulators
            val localCent = SRAM[T](MAXK,MAXD)
            Foreach(K by 1, D par P2){(ct,d) =>
              //val elem = mux(d == DM1, 1.to[T], pts(pt, d)) // fix for vanishing mux
              val elem = pts(pt,d)
              localCent(ct, d) = mux(ct == minCent.value._1, elem, 0.to[T])
            }
            localCent
          }{_+_} // Add the current point to the accumulators for this centroid
        }

        val centCount = SRAM[T](MAXK)
        Foreach(K by 1 par PX){ct => centCount(ct) = max(newCents(ct,DM1), 1.to[T]) } // Until diagonal banking is allowed

        // Average each new centroid
        // val centsOut = SRAM[T](MAXK, MAXD)
        Foreach(K by 1, D par P0){(ct,d) =>
//          val updateMux = mux(centCount(ct) == 0.to[T], 0.to[T], newCents(ct,d) / centCount(ct))
          cts(ct, d) = mux(centCount(ct) == 0.to[T], 0.to[T], newCents(ct,d) / centCount(ct)) //updateMux
        }
      }

      val flatCts = SRAM[T](MAXK * MAXD)
      Foreach(K by 1, D by 1) {(i,j) =>
        flatCts(i.to[Index]*D+j.to[Index]) = cts(i,j)
      }
      // Store the centroids out
      centroids(0::K*D par 16) store flatCts
    }

    getMem(centroids)
  }

  @virtualize
  def main() {
    val iters = args(0).to[Int]
    val N = args(1).to[Int]
    val K = numcents //args(2).to[SInt];
    val D = dim //args(3).to[SInt];

    val pts = Array.tabulate(N){i => Array.tabulate(D){d => if (d == D-1) 1.to[X] else random[X](element_max) + i }}
    val cnts = Array.tabulate(K){i => Array.tabulate(D){d => if (d == D-1) 1.to[X] else random[X](element_max) + (i*N/K) }}
    // val pts = Array.tabulate(N){i => Array.tabulate(D){d => if (d == D-1) 1.to[X] else if (d == 0) random[X](element_max) + i else 0.to[X]}}
    // val cnts = Array.tabulate(K){i => Array.tabulate(D){d => if (d == D-1) 1.to[X] else if (d == 0) random[X](element_max) + i else 0.to[X]}}
    // val pts = Array.tabulate(N){i => Array.tabulate(D){d => if (d == D-1) 1.to[X] else 5*i }}
    // val cnts = Array.tabulate(K){i => Array.tabulate(D){d => if (d == D-1) 1.to[X] else 5*i+1 }}

    // println("points: ")
    // for (i <- 0 until N) { println(i.mkString + ": " + pts(i).mkString(", ")) }

    val result = kmeans(pts.flatten, cnts.flatten, N, K, D, iters)

    val cts = Array.empty[Array[X]](K)
    for (k <- 0 until K) {
      cts(k) = Array.tabulate(D){i => pts(k).apply(i) }
    }
    val ii = Array.tabulate(K){i => i}

    for(epoch <- 0 until iters) {
      def dist[T:Type:Num](p1: Array[T], p2: Array[T]) = p1.zip(p2){(a,b) => (a - b)**2 }.reduce(_+_)

      // Make weighted points
      val map = pts.groupByReduce{pt =>
        val dists = cts.map{ct => dist(ct, pt) }
        dists.zip(ii){(a,b) => pack(a,b) }.reduce{(a,b) => if (a._1 < b._1) a else b}._2  // minIndex
      }{pt => pt}{(x,y) => x.zip(y){_+_} }

      // Average
      for (k <- 0 until K) {
        if (!map.contains(k)) {
          cts(k) = Array.tabulate(D){d => 0.to[X]}
        } else {
          val wp = map(k)
          val n  = wp(D - 1)
          cts(k) = Array.tabulate(D){d => if (d == D-1) 1.to[X] else wp(d)/n }
        }
      }
    }

    val gold = cts.flatten

    println("\n\nOriginal Centers:")
    (0 until K).foreach{ i => printArray(cnts(i))}
    println("\n\nOriginal Points:")
    (0 until N).foreach{ i => printArray(pts(i))}
    println("\n\nCorrect Centers:")
    (0 until K).foreach{ i => printArray(cts(i))}
    println("\n\nFPGA Centers:")
    (0 until K).foreach { i => 
      val resrow = Array.tabulate(D){j => result(i*D + j)}
      printArray(resrow)
    }

    // for (i <- 0 until result.length) {
    //   val diff = result(i) - gold(i)
    //   if (abs(diff) > margin)
    //     println("[" + i + "] gold: " + gold(i) + ", result: " + result(i) + ", diff: " + diff)
    // }

    val cksum = result.zip(gold){ case (o, g) => (g < (o + margin)) && g > (o - margin)}.reduce{_&&_}

    println("PASS: " + cksum + " (Kmeans)")
  }

}


object BFS extends SpatialApp { // DISABLED Regression (Sparse) // Args: 6 10
  val tileSize = 8000
  val edges_per_node = 6 // Will make this random later

  @virtualize
  def bfs(nodesIn: Array[Int], edgesIn: Array[Int], countsIn: Array[Int], idsIn: Array[Int], n: Int, e: Int, average_nodes_per_edge: Int) = {
    val edges = DRAM[Int](e)
    val counts = DRAM[Int](n)
    val ids = DRAM[Int](n)
    val result = DRAM[Int](n)

    setMem(edges, edgesIn)
    setMem(counts, countsIn)
    setMem(ids, idsIn)

    val depth = ArgIn[Int]
    val d = args(1).to[Int]
    setArg(depth, d)
    val anpe = ArgIn[Int]
    setArg(anpe, average_nodes_per_edge)

    Accel {
      val frontierNodes = SRAM[Int](tileSize)
      val frontierCounts = SRAM[Int](tileSize)
      val frontierIds = SRAM[Int](tileSize)
      val frontierLevels = SRAM[Int](tileSize)
      val currentNodes = SRAM[Int](tileSize)
      val pieceMem = SRAM[Int](tileSize)

      val concatReg = Reg[Int](0)
      val numEdges = Reg[Int](1)

      // Flush first few for scatter safety
      Foreach(anpe by 1){i =>  //dummy read of anpe
        Parallel{
          frontierNodes(i) = 0.to[Int]
          // frontierCounts(i) = 0.to[Int]
          frontierLevels(i) = 0.to[Int]
          currentNodes(i) = 0.to[Int]
          pieceMem(i) = 0.to[Int]
        }
      }
      Parallel {
        frontierIds load ids(0 :: tileSize)
        frontierCounts load counts(0 :: tileSize)
      }

      Sequential.Foreach(depth.value by 1) { i => /* Loop 1 */
        val nextLen = Reg[Int](1)
        val nextId = Reg[Int](1)
        Sequential.Foreach(numEdges by 1) { k => /* Loop 2 */
          // val lastLen = Reg[Int](1)

          val fetch = currentNodes(k)
          // val lastFetch = currentNodes(k - 1)
          nextId := frontierIds(fetch)
          nextLen := frontierCounts(fetch)
          // lastLen := frontierCounts(lastFetch)

          // pieceMem load edges(nextId :: nextId + nextLen)            
          pieceMem load edges(nextId :: nextId + nextLen)       

          // val frontierAddr = SRAM[Int](tileSize)
          // Foreach(nextLen by 1) { kk =>
          Foreach(nextLen by 1) { kk =>
            /* Since Loop 2 is a metapipe and we read concatReg before
               we write to it, this means iter0 and iter1 both read
               0 in concatReg.  I.e. we always see the previous iter's
               value of concatReg, so we should add nextLen to it here
               if we are not on the first iter (since concatReg is and
               should be 0)
            */
            // val plus = mux(k == 0, 0.to[Int], anpe.value) // Really adding in lastLen
            val frontierAddr = kk + concatReg.value
            frontierNodes(frontierAddr) = pieceMem(kk)
          }
          concatReg := min(tileSize.to[Int], concatReg + nextLen)
        }

        Foreach(concatReg by 1) { kk => currentNodes(kk) = frontierNodes(kk) }
        Foreach(concatReg by 1) { kk => frontierLevels(kk) = i + 1 }
        result(currentNodes, concatReg) scatter frontierLevels
        numEdges := concatReg
        concatReg := 0.to[Int]
      }
    }

    getMem(result)
  }

  @virtualize
  def main() {
    /* NEW VERSION FOR PERFORMANCE MEASUREMENTS */
    val E = 9600000
    val N = tileSize
    val average_nodes_per_edge = args(0).to[Int]
    val spacing = 3 
    val ed = E //args(1).to[SInt] // Set to roughly max_edges_per_node * N 

    val OCnodes = Array.tabulate(N) {i => 0.to[Int]}
    val OCedges = Array.tabulate(ed){ i => i*2 % N}
    val OCids = Array.tabulate(N)( i => average_nodes_per_edge*average_nodes_per_edge*i+1 % E)
    val OCcounts = Array.tabulate(N){ i => random[Int](average_nodes_per_edge-1)*2+1}

    val result = bfs(OCnodes, OCedges, OCcounts, OCids, N, E, average_nodes_per_edge)
    val gold = (6*1) + (16*2) + (22*3) + (5*4)
    // println("Cksum: " + gold + " == " + result.reduce{_+_})

    val cksum = gold == result.reduce{_+_}
    printArray(result, "result: ")
    println("Cksum = " + result.reduce{_+_})
    // println("PASS: " + cksum + " (BFS)")
  }

}


object BFS_FSM extends SpatialApp { // DISABLED Regression (Sparse) // Args: 6 10
  val tileSize = 8000

  @virtualize
  def bfs(nodesIn: Array[Int], edgesIn: Array[Int], lensIn: Array[Int], idsIn: Array[Int], n: Int, e: Int, average_nodes_per_edge: Int) = {
    val edges = DRAM[Int](e)
    val lens = DRAM[Int](n)
    val ids = DRAM[Int](n)
    val result = DRAM[Int](n)

    setMem(edges, edgesIn)
    setMem(lens, lensIn)
    setMem(ids, idsIn)

    val depth = ArgIn[Int]
    val d = args(1).to[Int]
    setArg(depth, d)
    // val anpe = ArgIn[Int]
    // setArg(anpe, average_nodes_per_edge)

    Accel {
      val init = 0
      val gatherEdgeInfo = 1
      val denseEdgeLoads = 2
      val scatterDepths = 3
      val done = 4

      val layer = Reg[Int](0)
      val depths = SRAM[Int](tileSize)
      val frontierStack = FILO[Int](tileSize) // TODO: Eventually allow scatters on stacks
      val idList = SRAM[Int](tileSize)
      val lenList = SRAM[Int](tileSize)
      val frontier = SRAM[Int](tileSize)
      val size = Reg[Int](1)
      FSM[Int]{state => state < done}{state =>
        if (state == init.to[Int]) {
          frontier(0) = 0.to[Int]
        } else if (state == gatherEdgeInfo.to[Int]) {
          // Increment layer
          layer :+= 1
          // Collect edge ids and lens
          idList gather ids(frontier par 1, size.value)
          lenList gather lens(frontier par 1, size.value)
        } else if (state == denseEdgeLoads.to[Int]) {
          // Accumulate frontier
          Foreach(size.value by 1) {i =>
            val start = idList(i)
            val end = lenList(i) + start
            frontierStack load edges(start::end par 1)
          }
        } else if (state == scatterDepths.to[Int]) {
          // Grab size of this scatter
          size := frontierStack.numel
          // Drain stack and set up scatter addrs + depth srams
          Foreach(frontierStack.numel by 1) { i => 
            depths(i) = layer.value
            frontier(i) = frontierStack.pop()
          }
          result(frontier, size) scatter depths
        }
      }{state => 

        mux(state == init.to[Int], gatherEdgeInfo, 
          mux(state == gatherEdgeInfo, denseEdgeLoads,
            mux(state == denseEdgeLoads, scatterDepths, 
              mux(state == scatterDepths && layer.value < depth, gatherEdgeInfo, done))))
        }

    }

    getMem(result)
  }

  @virtualize
  def main() {
    /* NEW VERSION FOR PERFORMANCE MEASUREMENTS */
    val E = 9600000
    val N = 96000
    val average_nodes_per_edge = args(0).to[Int]
    val spacing = 3 
    val ed = E //args(1).to[SInt] // Set to roughly max_edges_per_node * N 

    val OCnodes = Array.tabulate(N) {i => 0.to[Int]}
    val OCedges = Array.tabulate(ed){ i => i*2 % N}
    val OCids = Array.tabulate(N)( i => average_nodes_per_edge*average_nodes_per_edge*i+1 % E)
    val OCcounts = Array.tabulate(N){ i => random[Int](average_nodes_per_edge-1)*2+1}

    val result = bfs(OCnodes, OCedges, OCcounts, OCids, N, E, average_nodes_per_edge)
    val gold = 0
    // println("Cksum: " + gold + " == " + result.reduce{_+_})

    val cksum = gold == result.reduce{_+_}
    printArray(result, "result: ")
    println("Cksum = " + result.reduce{_+_})
    // println("PASS: " + cksum + " (BFS)")



  }

}

object BlackScholes extends SpatialApp {


  val margin = 0.5f // Validates true if within +/- margin
  val innerPar = 16
  val outerPar = 1
  val tileSize = 2000

  final val inv_sqrt_2xPI = 0.39894228040143270286f

  @virtualize
  def CNDF(x: Float) = {
    val ax = abs(x)

    val xNPrimeofX = exp((ax ** 2) * -0.05f) * inv_sqrt_2xPI
    val xK2 = 1.to[Float] / ((ax * 0.2316419f) + 1.0f)

    val xK2_2 = xK2 ** 2
    val xK2_3 = xK2_2 * xK2
    val xK2_4 = xK2_3 * xK2
    val xK2_5 = xK2_4 * xK2

    val xLocal_10 = xK2 * 0.319381530f
    val xLocal_20 = xK2_2 * -0.356563782f
    val xLocal_30 = xK2_3 * 1.781477937f
    val xLocal_31 = xK2_4 * -1.821255978f
    val xLocal_32 = xK2_5 * 1.330274429f

    val xLocal_21 = xLocal_20 + xLocal_30
    val xLocal_22 = xLocal_21 + xLocal_31
    val xLocal_23 = xLocal_22 + xLocal_32
    val xLocal_1 = xLocal_23 + xLocal_10

    val xLocal0 = xLocal_1 * xNPrimeofX
    val xLocal  = -xLocal0 + 1.0f

    mux(x < 0.0f, xLocal0, xLocal)
  }

  @virtualize
  def BlkSchlsEqEuroNoDiv(sptprice: Float, strike: Float, rate: Float,
    volatility: Float, time: Float, otype: Int) = {

    val xLogTerm = log( sptprice / strike )
    val xPowerTerm = (volatility ** 2) * 0.5f
    val xNum = (rate + xPowerTerm) * time + xLogTerm
    val xDen = volatility * sqrt(time)

    val xDiv = xNum / (xDen ** 2)
    val nofXd1 = CNDF(xDiv)
    val nofXd2 = CNDF(xDiv - xDen)

    val futureValueX = strike * exp(-rate * time)

    val negNofXd1 = -nofXd1 + 1.0f
    val negNofXd2 = -nofXd2 + 1.0f

    val optionPrice1 = (sptprice * nofXd1) - (futureValueX * nofXd2)
    val optionPrice2 = (futureValueX * negNofXd2) - (sptprice * negNofXd1)
    mux(otype == 0, optionPrice2, optionPrice1)
  }

  @virtualize
  def blackscholes(
    stypes:      Array[Int],
    sprices:     Array[Float],
    sstrike:     Array[Float],
    srate:       Array[Float],
    svolatility: Array[Float],
    stimes:      Array[Float]
  ): Array[Float] = {
    val B  = tileSize (96 -> 96 -> 19200)
    val OP = outerPar (1 -> 2)
    val IP = innerPar (1 -> 96)

    val size = stypes.length; bound(size) = 9995328

    val N = ArgIn[Int]
    setArg(N, size)

    val types    = DRAM[Int](N)
    val prices   = DRAM[Float](N)
    val strike   = DRAM[Float](N)
    val rate     = DRAM[Float](N)
    val vol      = DRAM[Float](N)
    val times    = DRAM[Float](N)
    val optprice = DRAM[Float](N)
    setMem(types, stypes)
    setMem(prices, sprices)
    setMem(strike, sstrike)
    setMem(rate, srate)
    setMem(vol, svolatility)
    setMem(times, stimes)

    Accel {
      Foreach(N by B par OP) { i =>
        val typeBlk   = SRAM[Int](B)
        val priceBlk  = SRAM[Float](B)
        val strikeBlk = SRAM[Float](B)
        val rateBlk   = SRAM[Float](B)
        val volBlk    = SRAM[Float](B)
        val timeBlk   = SRAM[Float](B)
        val optpriceBlk = SRAM[Float](B)

        Parallel {
          typeBlk   load types(i::i+B par IP)
          priceBlk  load prices(i::i+B par IP)
          strikeBlk load strike(i::i+B par IP)
          rateBlk   load rate(i::i+B par IP)
          volBlk    load vol(i::i+B par IP)
          timeBlk   load times(i::i+B par IP)
        }

        Foreach(B par IP){ j =>
          val price = BlkSchlsEqEuroNoDiv(priceBlk(j), strikeBlk(j), rateBlk(j), volBlk(j), timeBlk(j), typeBlk(j))
          optpriceBlk(j) = price
        }
        optprice(i::i+B par IP) store optpriceBlk
      }
    }
    getMem(optprice)
  }

  @virtualize
  def main(): Unit = {
    val N = args(0).to[Int]

    val types  = Array.fill(N)(random[Int](2))
    val prices = Array.fill(N)(random[Float])
    val strike = Array.fill(N)(random[Float])
    val rate   = Array.fill(N)(random[Float])
    val vol    = Array.fill(N)(random[Float])
    val time   = Array.fill(N)(random[Float])

    val out = blackscholes(types, prices, strike, rate, vol, time)

    printArray(out, "result: ")

    //val cksum = out.zip(gold){ case (o, g) => (g < (o + margin)) && g > (o - margin)}.reduce{_&&_}
    //println("PASS: " + cksum + " (BlackSholes)")


  }
}

object Convolution_FPGA extends SpatialApp { // Regression (Dense) // Args: none


  val Kh = 3
  val Kw = 3
  val Cmax = 16

  @virtualize
  def convolve[T:Type:Num](image: Matrix[T]): Matrix[T] = {
    val B = 16 (1 -> 1 -> 16)

    val R = ArgIn[Int]
    val C = ArgIn[Int]
    setArg(R, image.rows)
    setArg(C, image.cols)
    val lb_par = 8

    val img = DRAM[T](R, C)
    val imgOut = DRAM[T](R, C)

    setMem(img, image)

    Accel {
      val lb = LineBuffer[T](Kh, Cmax)

      val kh = LUT[T](3,3)(1.to[T], 0.to[T], -1.to[T],
                           2.to[T], 0.to[T], -2.to[T],
                           1.to[T], 0.to[T], -1.to[T])
      val kv = LUT[T](3,3)(1.to[T],  2.to[T],  1.to[T],
                           0.to[T],  0.to[T],  0.to[T],
                          -1.to[T], -2.to[T], -1.to[T])

      val sr = RegFile[T](Kh, Kw)
      val lineOut = SRAM[T](Cmax)

      Foreach(0 until R) { r =>
        lb load img(r, 0::C par lb_par)

        /*println("Row " + r)
        Foreach(0 until Kh) { i =>
          Foreach(0 until C) { c => print("" + lb(i,c) + "\t") }
          println("")
        }*/

        Foreach(0 until C) { c =>
          Pipe{sr.reset(c == 0)}

          Foreach(0 until Kh par Kh){i => sr(i, *) <<= lb(i, c) }
          
          val horz = Reduce(Reg[T])(Kh by 1, Kw by 1){ (i,j) => 
            // val number = mux((r < 2) || (c < 2) , 0.to[T], sr(i,j))
            // number * kh(i,j) 
            sr(i,j) * kh(i,j)
          }{_+_}
          val vert = Reduce(Reg[T])(Kh by 1, Kw by 1){ (i,j) => 
            // val number = mux((r < 2) || (c < 2) , 0.to[T], sr(i,j))
            // number * kv(i,j) 
            sr(i,j) * kv(i,j)
          }{_+_}

          lineOut(c) = mux( r < 2, 0.to[T], abs(horz.value) + abs(vert.value)) // Technically should be sqrt(horz**2 + vert**2)
        }

        imgOut(r, 0::C par 16) store lineOut
      }

    }

    getMatrix(imgOut)

  }

  @virtualize
  def main() {
    val R = 16
    val C = 16
    val border = 3
    // val image = (0::R, 0::C){(i,j) => if (j > 3 && i > 3 && j < 11 && i < 11) 256 else 0 }
    val image = (0::R, 0::C){(i,j) => if (j > border && j < C-border && i > border && i < C - border) i*16 else 0}
    val ids = (0::R, 0::C){(i,j) => if (i < 2) 0 else 1}

    val kh = List((List(1,2,1), List(0,0,0), List(-1,-2,-1)))
    val kv = List((List(1,0,-1), List(2,0,-2), List(1,0,-1)))

    val output = convolve(image)

    /*
      Filters: 
      1   2   1 
      0   0   0 
     -1  -2  -1

      1   0  -1 
      2   0  -2 
      1   0  -1

    */
    val gold = (0::R, 0::C){(i,j) => 
      // Shift result down by 2 and over by 2 because of the way accel is written
      val px00 = if ((j-2) > border && (j-2) < C-border && (i-2) > border && (i-2) < C - border) (i-2)*16 else 0
      val px01 = if ((j-1) > border && (j-1) < C-border && (i-2) > border && (i-2) < C - border) (i-2)*16 else 0
      val px02 = if ((j+0) > border && (j+0) < C-border && (i-2) > border && (i-2) < C - border) (i-2)*16 else 0
      val px10 = if ((j-2) > border && (j-2) < C-border && (i-1) > border && (i-1) < C - border) (i-1)*16 else 0
      val px11 = if ((j-1) > border && (j-1) < C-border && (i-1) > border && (i-1) < C - border) (i-1)*16 else 0
      val px12 = if ((j+0) > border && (j+0) < C-border && (i-1) > border && (i-1) < C - border) (i-1)*16 else 0
      val px20 = if ((j-2) > border && (j-2) < C-border && (i+0) > border && (i+0) < C - border) (i+0)*16 else 0
      val px21 = if ((j-1) > border && (j-1) < C-border && (i+0) > border && (i+0) < C - border) (i+0)*16 else 0
      val px22 = if ((j+0) > border && (j+0) < C-border && (i+0) > border && (i+0) < C - border) (i+0)*16 else 0
      abs(px00 * 1 + px01 * 2 + px02 * 1 - px20 * 1 - px21 * 2 - px22 * 1) + abs(px00 * 1 - px02 * 1 + px10 * 2 - px12 * 2 + px20 * 1 - px22 * 1)
    };

    // // This contains the "weird scheduling bug"
    printMatrix(image, "Image")
    printMatrix(gold, "Gold")
    printMatrix(output, "Output")

    val gold_sum = gold.map{g => g}.reduce{_+_} 
    val output_sum = output.zip(ids){case (o,i) => i * o}.reduce{_+_}
    println("gold " + gold_sum + " =?= output " + output_sum)
    val cksum = gold_sum == output_sum
    // val cksum = gold.zip(output){(g, o) => g == o}.reduce{_&&_}
    println("PASS: " + cksum + " (Convolution_FPGA)")



  }
}

object EdgeDetector extends SpatialApp { // Regression (Dense) // Args: none

  type T = FixPt[TRUE,_16,_16]

  @virtualize
  def main() {
    type T = FixPt[TRUE,_16,_16]
    val rowtile = 16
    val coltile = 64
    val data = loadCSV2D[T]("/remote/regression/data/slacsample2d.csv", ",", "\n")
    val memrows = ArgIn[Int]
    val memcols = ArgIn[Int]
    setArg(memrows, data.rows.to[Int])
    setArg(memcols, data.cols.to[Int])
    val srcmem = DRAM[T](memrows, memcols)
    setMem(srcmem, data)
    val risingEdges = DRAM[Int](memrows)
    // val fallingEdges = DRAM[Int](memrows)

    val window = 16

    Accel {
      val sr = RegFile[T](1,window)
      val rawdata = SRAM[T](coltile)
      val results = SRAM[Int](rowtile)
      // Work on each row
      Sequential.Foreach(memrows by rowtile) { r => 
        Sequential.Foreach(rowtile by 1) { rr => 
          // Work on each tile of a row
          val globalMax = Reduce(Reg[Tuple2[Int,T]](pack(0.to[Int], -1000.to[T])))(memcols by coltile) { c =>
            // Load tile from row
            rawdata load srcmem(r + rr, c::c+coltile)
            // Scan through tile to get deriv
            val localMax = Reduce(Reg[Tuple2[Int,T]](pack(0.to[Int], -1000.to[T])))(coltile by 1) { j =>
              sr(0,*) <<= rawdata(j)
              val mean_right = Reduce(Reg[T](0.to[T]))(window/2 by 1) { k => sr(0,k) }{_+_} / window.to[T]
              val mean_left = Reduce(Reg[T](0.to[T]))(window/2 by 1) { k => sr(0,k+window/2) }{_+_} / window.to[T]
              val slope = (mean_right - mean_left) / (window/2).to[T]
              val idx = j + c
              mux(idx < window, pack(idx, 0.to[T]), pack(idx,slope))
            }{(a,b) => mux(a._2 > b._2, a, b)}
            localMax
          }{(a,b) => mux(a._2 > b._2, a, b)}
          results(rr) = globalMax._1
        }
        risingEdges(r::r+rowtile) store results
      }
    }


    // Extract results from accelerator
    val results = getMem(risingEdges)
    val gold = loadCSV1D[Int]("/remote/regression/data/edge_gold.csv", ",")
    val margin = 2.to[Int]

    // Create validation checks and debug code
    printArray(results, "Results:")

    val cksum = results.zip(gold) {case (a,b) => a < b + margin && a > b - margin}.reduce{_&&_}
    println("PASS: " + cksum + " (EdgeDetector)")
  }
}

object Differentiator extends SpatialApp { // Regression (Dense) // Args: none

  type T = FixPt[TRUE,_16,_16]

  @virtualize
  def main() {
    type T = FixPt[TRUE,_16,_16]
    val coltile = 64
    val data = loadCSV1D[T]("/remote/regression/data/slacsample1d.csv", ",")
    val memcols = ArgIn[Int]
    setArg(memcols, data.length.to[Int])
    val srcmem = DRAM[T](memcols)
    setMem(srcmem, data)
    val dstmem = DRAM[T](memcols)

    val window = 16

    Accel {
      val sr = RegFile[T](1,window)
      val rawdata = SRAM[T](coltile)
      val results = SRAM[T](coltile)
      Foreach(window by 1) { i => results(i) = 0} // Temporary fix to regression screwing up write to address 1 only
      // Work on each tile of a row 
      Foreach(memcols by coltile) { c =>
        rawdata load srcmem(c::c+coltile)
        // Scan through tile to get deriv
        Foreach(coltile by 1) { j => 
          // Pipe{sr.reset(j == 0)}
          sr(0,*) <<= rawdata(j)
          val mean_right = Reduce(Reg[T](0.to[T]))(window/2 by 1) { k => sr(0,k) }{_+_} / window.to[T]
          val mean_left = Reduce(Reg[T](0.to[T]))(window/2 by 1) { k => sr(0,k+window/2) }{_+_} / window.to[T]
          val slope = (mean_right - mean_left) / (window/2).to[T]
          val idx = j + c
          results(j) = mux(idx < window, 0.to[T], slope)
        }
        dstmem(c::c+coltile) store results
      }
    }


    // Extract results from accelerator
    val results = getMem(dstmem)

    // // Write answer for first time
    // writeCSV1D(results, "/remote/regression/data/deriv_gold.csv", ",")
    // Read answer
    val gold = loadCSV1D[T]("/remote/regression/data/deriv_gold.csv", ",")

    // Create validation checks and debug code
    printArray(results, "Results:")
    val margin = 0.5.to[T]

    val cksum = gold.zip(results){case (a,b) => abs(a-b) < margin}.reduce{_&&_}
    println("PASS: " + cksum + " (Differentiator) * Look into issue with addr 1 screwing up with --retiming on and --naming off but only with waveforms off in dc6db05")
  }
}

object GDA extends SpatialApp { // Regression (Dense) // Args: 64


  type X = Float

  val MAXC = 96
  val C = MAXC
  val margin = 1

  val innerPar = 16
  val outerPar = 4

  val tileSize = 20

  @virtualize
  def gda[T: Type : Num](xCPU: Array[T], yCPU: Array[Int], mu0CPU: Array[T], mu1CPU: Array[T]) = {
    val rTileSize = tileSize(96 -> 19200)
    val op = outerPar(1 -> 8)
    val ip = innerPar(1 -> 12)
    val subLoopPar = innerPar(1 -> 16)
    val prodLoopPar = innerPar(1 -> 96)
    val outerAccumPar = innerPar(1 -> 1)

    val rows = yCPU.length;
    bound(rows) = 360000
    val cols = mu0CPU.length;
    bound(cols) = MAXC

    val R = ArgIn[Int]
    // val C = ArgIn[SInt]
    // setArg(C, cols)
    setArg(R, rows)

    val x = DRAM[T](R, C)
    val y = DRAM[Int](R)
    val mu0 = DRAM[T](C)
    val mu1 = DRAM[T](C)
    val sigma = DRAM[T](C, C)

    setMem(x, xCPU)
    setMem(y, yCPU)
    setMem(mu0, mu0CPU)
    setMem(mu1, mu1CPU)

    Accel {
      val mu0Tile = SRAM[T](MAXC)
      val mu1Tile = SRAM[T](MAXC)
      Parallel {
        mu0Tile load mu0(0 :: C par 16) // Load mu0
        mu1Tile load mu1(0 :: C par 16) // Load mu1
      }

      val sigmaOut = SRAM[T](MAXC, MAXC)

      MemReduce(sigmaOut)(R by rTileSize par op){ r =>
        val gdaYtile = SRAM[Int](rTileSize)
        val gdaXtile = SRAM[T](rTileSize, MAXC)
        val blk = Reg[Int]
        Parallel {
          gdaYtile load y(r :: r + rTileSize par 16)
          gdaXtile load x(r :: r + rTileSize, 0 :: C par 16) // Load tile of x
          Pipe {
            blk := min(R.value - r, rTileSize)
          }
        }

        val sigmaBlk = SRAM[T](MAXC, MAXC)

        MemReduce(sigmaBlk)(blk par param(1)) { rr =>
          val subTile = SRAM[T](MAXC)
          val sigmaTile = SRAM[T](MAXC, MAXC)
          Foreach(C par subLoopPar) { cc =>
            subTile(cc) = gdaXtile(rr, cc) - mux(gdaYtile(rr) == 1, mu1Tile(cc), mu0Tile(cc))
          }
          Foreach(C by 1, C par ip) { (ii, jj) =>
            sigmaTile(ii, jj) = subTile(ii) * subTile(jj)
          }
          sigmaTile
        }{_+_}
      }{_+_}

      sigma(0 :: C, 0 :: C par 16) store sigmaOut
    }

    getMem(sigma)
  }


  def printArr(a: Array[Int], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  @virtualize
  def main() {
    val R = args(0).to[Int]
    // val C = args(0).to[SInt] // TODO: Should be selectable up to maximum

    // val x  = Array.fill(R){ Array.fill(C){ random[X](10) }}
    // val ys = Array.fill(R){ random[Int](1) }
    // val mu0 = Array.fill(C){ random[X](10) }
    // val mu1 = Array.fill(C){ random[X](10) }

    val x = Array.tabulate(R) { i => Array.tabulate(C) { j => (i * C + j) % 256 } }
    val ys = Array.tabulate(R) { i => i % 256 }
    val mu0 = Array.tabulate(C) { i => i % 2 }
    val mu1 = Array.tabulate(C) { i => i % 2 }

    val result = gda(x.flatten, ys, mu0, mu1)

    val gold = x.zip(ys) { (row, y) =>
      val sub = if (y == 1) row.zip(mu1){_-_} else row.zip(mu0) {_-_}
      Array.tabulate(C) { i => Array.tabulate(C) { j => sub(i) * sub(j) } }.flatten
    }.reduce { (a, b) => a.zip(b) {_+_} }

    printArr(gold, "gold: ")
    printArr(result, "result: ")

    val cksum = gold.zip(result){ case (a,b) => a < b + margin && a > b - margin }.reduce{_&&_}
    println("PASS: " + cksum  + " (GDA)")

    // // println("actual: " + gold.mkString(", "))
    // //println("result: " + result.mkString(", "))
    // // println("Sum of differences: " + gold.zip(result){_-_}.reduce{_+_})
    // printArr(gold, "gold: ")
    // printArr(result, "result: ")

    // val cksum = gold.zip(result){ case (a,b) => a < b + margin && a > b - margin }.reduce{_&&_}
    // println("PASS: " + cksum  + " (GDA)")

    // assert( result == gold )
  }

}


object Gibbs_Ising2D extends SpatialApp { // Regression (Dense) // Args: 25 0.3 2
  /*
  Implementation based on http://cs.stanford.edu/people/karpathy/visml/ising_example.html
   pi(x) = exp(J* 𝚺x_j*x_i + J_b * 𝚺b_i*x_i)        
   let x' = x with one entry flipped
   Prob(accept x') ~ min(1, pi(x')/pi(x)) = exp(-2*J*𝚺x_j*x_i)*exp(-2*J_b*𝚺b_i*x_i)
  Use args 100 0.4 0 to get a nice looking lava lamp pattern, or 0.8 for scala

                           
          _________________________________________
         |                                         |
         | update --->                             |
x_par=4  |       --->       X                XX    |
         |      --->                       XXXX    |
         |     --->     .------------.X   X XXX    |
         |          X   .BIAS REGION .  XX   X     |
         |            XX.            .     XX      |
         |              .     X XX   .             |
         |X             .      XXXXX .             |
         |              .      XXXX  .             |
         |      X XXX   .------------.             |
         |     X XXX        XX         X           |
         |                                         |
         |                                  X      |
         |_________________________________________|

  */
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

    val par_load = 16
    val par_store = 16
    val x_par = 4 (1 -> 1 -> 16)

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
      exp_sram load exp_lut
      grid_sram load grid_dram(0::ROWS, 0::COLS par par_load)
      // Issue #187
      val bias_sram = SRAM[Int](ROWS,COLS)
      bias_sram load bias_dram(0::ROWS, 0::COLS par par_load)


      Foreach(iters by 1) { iter =>
        Foreach(ROWS by 1 par x_par) { i => 
          // Update each point in active row
          val this_body = i % x_par
          Sequential.Foreach(-this_body until COLS by 1) { j => 
            // val col = j - this_body
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
            if (j >= 0 && j < COLS) {
              grid_sram(i,j) = mux(flip == 1.to[T], -self, self)
            }
          }
        }
      }
      grid_dram(0::ROWS, 0::COLS par par_store) store grid_sram
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

object LogReg extends SpatialApp {


  type X = Float //FixPt[TRUE,_16,_16]

  val margin = 5
  val dim = 192
  val D = dim
  val A = 1

  val innerPar = 16
  val outerPar = 10

  val tileSize = 64

  def sigmoid[T:Type:Num](t:T) = 1.to[T]/(exp(-t) + 1.to[T])

  @virtualize
  def logreg[T:Type:Num](xIn: Array[T], yIn: Array[T], tt: Array[T], n: Int, it: Int) = {
    val iters = ArgIn[Int]
    val N     = ArgIn[Int]
    setArg(iters, it)
    setArg(N, n)

    val BN = tileSize (96 -> 96 -> 9600)
    val PX = 1 (1 -> 1)
    val P1 = innerPar (1 -> 2)
    val P2 = innerPar (1 -> 96)
    val P3 = outerPar (1 -> 96)

    val x = DRAM[T](N, D)
    val y = DRAM[T](N)
    val theta = DRAM[T](D)

    setMem(x, xIn)
    setMem(y, yIn)
    setMem(theta, tt)

    Accel {
      val btheta = SRAM[T](D)

      Sequential.Foreach(iters by 1) { epoch =>

        Sequential.MemReduce(btheta)(1 by 1){ xx =>
          val gradAcc = SRAM[T](D)
          Foreach(N by BN){ i =>
            val logregX = SRAM[T](BN, D)
            val logregY = SRAM[T](BN)
            Parallel {
              logregX load x(i::i+BN, 0::D par P2)
              logregY load y(i::i+BN par P2)
            }
            MemReduce(gradAcc)(BN par P3){ ii =>
              val pipe2Res = Reg[T]
              val subRam   = SRAM[T](D)

              val dotAccum = Reduce(Reg[T])(D par P2){j => logregX(ii,j) * btheta(j) }{_+_}  // read
              Pipe { pipe2Res := (logregY(ii) - sigmoid(dotAccum.value)) }
              Foreach(D par P2) {j => subRam(j) = logregX(ii,j) - pipe2Res.value }
              subRam
            }{_+_}
          }
          gradAcc
        }{(b,g) => b+g*A.to[T]}

        // Flush gradAcc
        //Pipe(D by 1 par P2) { i => gradAcc(i) = 0.to[T]}
      }
      theta(0::D par P2) store btheta // read
    }
    getMem(theta)
  }

  @virtualize
  def main() {
    val iters = args(0).to[Int]
    val N = args(1).to[Int]

    val sX = Array.fill(N){ Array.fill(D){ random[X](10.to[X])} }
    val sY = Array.tabulate(N){ i => i.to[X]} //fill(N)( random[T](10.0) )
    val theta = Array.fill(D) {random[X](1.to[X]) }

    val result = logreg(sX.flatten,sY, theta, N, iters)

    val gold = Array.empty[X](D)
    val ids = Array.tabulate(D){i => i}
    for (i <- 0 until D) {
      gold(i) = theta(i)
    }
    for (i <- 0 until iters) {
      val next = sX.zip(sY) {case (row, y) =>
        // println("sigmoid for " + y + " is " + sigmoid(row.zip(gold){_*_}.reduce{_+_}))
        val sub = y - sigmoid(row.zip(gold){(a,b) =>
          // println("doing " + a + " * " + b + " on row " + y)
          a*b}.reduce{_+_})
        row.map{a =>
          // println("subtraction for " + y + " is " + (a - sub))
          a - sub}
      }.reduce{(a,b) => a.zip(b){_+_}}
      for (i <- 0 until D) {
        gold(i) = gold(i) + next(i)
      }
      // printArr(gold, "gold now")
    }


    printArray(gold, "gold: ")
    printArray(result, "result: ")

    val cksum = result.zip(gold){ (a,b) => a > b-margin && a < b+margin}.reduce{_&&_}
    // println("max err: " + result.zip(gold){(a,b) => (a-b)*(a-b)}.reduce{Math.max(_,_)})
    // println("mean err: " + result.zip(gold){(a,b) => (a-b)*(a-b)}.reduce{_+_} / D)
    println("PASS: " + cksum  + " (LogReg)")



    /* OptiMl version
    val w = untilconverged(theta, maxIter = 30) { (cur, iter) =>
      val gradient = ((0::x.numRows) { i =>
        x(i)*(y(i) - sigmoid(cur *:* x(i)))
      }).sum
      val z = cur + gradient*alpha
      z
    }
    */

  }

}

object PageRank extends SpatialApp { // Regression (Sparse) // Args: 50 0.125

  type Elem = FixPt[TRUE,_16,_16] // Float
  type X = FixPt[TRUE,_16,_16] // Float

  /*
    Currently testing with DIMACS10 Chesapeake dataset from UF Sparse Matrix collection

  */
  val margin = 0.3f

  @virtualize
  def main() {
    val tileSize = 16
    val sparse_data = loadCSV2D[Int]("/remote/regression/data/machsuite/pagerank_chesapeake.csv", " ", "\n").transpose
    val rows = sparse_data(0,0)
    val node1_list = Array.tabulate(sparse_data.cols - 1){i => sparse_data(0, i+1)-1} // Every page is 1-indexed...
    val node2_list = Array.tabulate(sparse_data.cols - 1){i => sparse_data(1, i+1)-1} // Every page is 1-indexed...
    // Preprocess to get frontier sizes.  We can do this on chip also if we wanted
    println("Matrix has " + rows + " rows")
    val edgeLens = Array.tabulate(rows){i => Array.tabulate(node1_list.length){j => 
      if (node1_list(j) == i) 1 else 0
    }.reduce{_+_}}
    val edgeIds = Array.tabulate(rows){i => 
      var id = -1
      Array.tabulate(node1_list.length){j => 
        if (id == -1 && node1_list(j) == i) {
          id = j
          j
        } else { 0 }
    }.reduce{_+_}}

    // printArray(node1_list, "node1_list:")
    // printArray(node2_list, "node2_list:")
    printArray(edgeLens, "edgeLens: ")
    printArray(edgeIds, "edgeIds: ")

    val par_load = 16
    val par_store = 16
    val tile_par = 2 (1 -> 1 -> 12)
    val page_par = 2 (1 -> 1 -> tileSize)

    // Arguments
    val itersIN = args(0).to[Int]
    val dampIN = args(1).to[X]

    val iters = ArgIn[Int]
    val NP    = ArgIn[Int]
    val damp  = ArgIn[X]
    val NE    = ArgIn[Int]
    setArg(iters, itersIN)
    setArg(NP, rows)
    setArg(damp, dampIN)
    setArg(NE, node2_list.length)

    val OCpages    = DRAM[X](NP)
    val OCedges    = DRAM[Int](NE)    // srcs of edges
    val OCedgeLens   = DRAM[Int](NP)    // counts for each edge
    val OCedgeIds   = DRAM[Int](NP) // Start index of edges

    val pagesInit = Array.tabulate(NP){i => 4.to[X]}

    setMem(OCpages, pagesInit)
    setMem(OCedges, node2_list)
    setMem(OCedgeLens, edgeLens)
    setMem(OCedgeIds, edgeIds)

    Accel {
      Sequential.Foreach(iters by 1){iter => 
        // Step through each tile
        Foreach(NP by tileSize par tile_par){page => 
          val local_pages = SRAM.buffer[X](tileSize)
          val local_edgeIds = SRAM[Int](tileSize)
          val local_edgeLens = SRAM[Int](tileSize)
          val pages_left = min(tileSize.to[Int], NP-page)
          local_pages load OCpages(page::page+pages_left par par_load)
          local_edgeLens load OCedgeLens(page::page+pages_left par par_load)
          local_edgeIds load OCedgeIds(page::page+pages_left par par_load)
          // Process each page in local tile
          Sequential.Foreach(pages_left by 1 par page_par){local_page => 
            // Fetch edge list for this page
            val edgeList = FIFO[Int](128)
            val id = local_edgeIds(local_page)
            val len = local_edgeLens(local_page)
            edgeList load OCedges(id::id+len)
            // Triage between edges that exist in local tiles and off chip
            val nearPages = FIFO[Int](128)
            val farPages1 = FIFO[Int](128)
            val farPages2 = FIFO[Int](128)
            Foreach(edgeList.numel by 1){i => 
              val tmp = edgeList.deq()
              if (tmp >= page && tmp < page+pages_left) {
                nearPages.enq(tmp - page)
              } else {
                farPages1.enq(tmp)
                farPages2.enq(tmp)
              }
            }
            // Fetch off chip info
            val local_farPages = FIFO[X](128)
            val local_farEdgeLens = FIFO[Int](128)
            local_farPages gather OCpages(farPages1) // Need fifos 1 and 2 because fifo is consumed during gather
            local_farEdgeLens gather OCedgeLens(farPages2)

            // Do math to find new rank
            val pagerank = Pipe(ii=7).Reduce(Reg[X](0))(len by 1){i => 
              if (nearPages.empty) {
                println("page: " + page + ", local_page: " + local_page + " deq from far")
                local_farPages.deq() / local_farEdgeLens.deq().to[X]
              } else {
                val addr = nearPages.deq()
                println("page: " + page + ", local_page: " + local_page + " deq from near addr " + addr)
                local_pages(addr) / local_edgeLens(addr).to[X]
              }
            }{_+_}

            // Write new rank
            local_pages(local_page) = pagerank * damp + (1.to[X] - damp)
          }
          OCpages(page::page+pages_left par par_store) store local_pages
        }
      }
    }

    val result = getMem(OCpages)

    val gold = Array.empty[X](NP)
    // Init
    for (i <- 0 until NP) {
      gold(i) = pagesInit(i)
    }

    // Really bad imperative version
    for (ep <- 0 until iters) {
      // println("Iter " + ep)
      for (i <- 0 until NP) {
        val numEdges = edgeLens(i)
        val startId = edgeIds(i)
        val iterator = Array.tabulate(numEdges){kk => startId + kk}
        val these_edges = iterator.map{j => node2_list(j)}
        val these_pages = these_edges.map{j => gold(j)}
        val these_counts = these_edges.map{j => edgeLens(j)}
        val pr = these_pages.zip(these_counts){ (p,c) =>
          // println("page " + i + " doing " + p + " / " + c)
          p/c.to[X]
        }.reduce{_+_}
        // println("new pr for " + i + " is " + pr)
        gold(i) = pr*dampIN + (1.to[X]-dampIN)
      }
    }

    println("PageRank on DIMACS10 Chesapeake dataset downloaded from UF Sparse Matrix collection")
    printArray(gold, "gold: ")
    printArray(result, "result: ")
    val cksum = result.zip(gold){ case (o, g) => (g < (o + margin.to[X])) && g > (o - margin.to[X])}.reduce{_&&_}
    println("PASS: " + cksum + " (PageRank)")

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

object SPMV_DumbPack extends SpatialApp {  // Regression (Sparse) // Args: 1536


  type T = Int //FixPt[Signed,B16,B16]

  val pp = 3840
  val NNZ = 60

  val ip = 1
  val op = 2

  val tileSize = 384

  val margin = 1

  @virtualize
  def main() = {
    val nn = args(0).to[Int]
    val P = pp

    val AC = Array.tabulate(nn){ i => Array.tabulate(NNZ) { j => (j * 3).to[Int]}}
    val AD = Array.tabulate(nn){ i => Array.fill(NNZ) {random[Int](5) }}
    val S = Array.tabulate(nn){ i => NNZ.to[Int] }
    val V = Array.tabulate(P){ i => i.to[Int] }

    val N = ArgIn[Int]
    setArg(N,nn)

    val aC = DRAM[Int](pp,NNZ)
    val aD = DRAM[Int](pp,NNZ)
    val sizes = DRAM[Int](pp)
    val v = DRAM[Int](pp)
    val out = DRAM[Int](N)

    //val op = op (1 -> 6)
    //val ip = ip (1 -> 96)
    val stPar    = ip (1 -> 1)

    setMem(aC, AC.flatten)
    setMem(aD, AD.flatten)
    setMem(sizes, S)
    setMem(v, V)

    Accel {
      Foreach(N by tileSize par op){ rowchunk =>
        val smvresult = SRAM[Int](tileSize)
        val smvtileSizes = SRAM[Int](tileSize)
        smvtileSizes load sizes(rowchunk :: rowchunk+tileSize par ip)
        Foreach(tileSize by 1){row =>
          val csrCols = SRAM[Int](tileSize)
          val csrData = SRAM[Int](tileSize)
          val vecGathered = SRAM[Int](tileSize)

          // Load dense csr piece
          val len = smvtileSizes(row)
          val OCROW = (rowchunk+row) // TODO: Issue #47
          Parallel{
            csrCols load aC(OCROW, 0 :: len par ip)
            csrData load aD(OCROW, 0 :: len par ip)
          }
          vecGathered gather v(csrCols, len)

          val acc = Reduce(Reg[Int](0.to[Int]))(len by 1 par ip) { i =>
            csrData(i) * vecGathered(i)
          }{_+_}

          smvresult(row) = acc.value
        }
        out(rowchunk::rowchunk+tileSize par stPar) store smvresult
      }
    }
    val smvresult = getMem(out)



    val gold = AC.zip(AD) { (col, data) => col.zip(data) {(c, d) =>
      d*V(c)
    }.reduce{_+_}}

    printArray(gold, "gold: ")
    printArray(smvresult, "smvresult: ")

    val cksum = smvresult.zip(gold){(a,b) => a - margin < b && a + margin > b}.reduce{_&&_}
    println("PASS: " + cksum + " (SMV)")

  }


}



/*

optiQL
trait TPCHQ6Trait extends TPCHBaseTrait {
  val queryName = "Q6"

  def query() = {
    val lineItems = loadLineItems()
    tic(lineItems.size)

    //FIXME: infix_&& fails to resolve automatically
    val q = lineItems Where (l => infix_&&(l.l_shipdate >= Date("1994-01-01"), infix_&&(l.l_shipdate < Date("1995-01-01"), infix_&&(l.l_discount >= 0.05, infix_&&(l.l_discount <= 0.07, l.l_quantity < 24)))))
    val revenue = q.Sum(l => l.l_extendedprice * l.l_discount)

    toc(revenue)
    println(revenue)
  }
}


SQL:
SELECT
    sum(l_extendedprice * l_discount) as revenue
FROM
    lineitem
WHERE
    l_shipdate >= date '1994-01-01'
    AND l_shipdate < date '1994-01-01' + interval '1' year
    AND l_discount between 0.06 - 0.01 AND 0.06 + 0.01
    AND l_quantity < 24;

*/

object TPCHQ6 extends SpatialApp { // Regression (Dense) // Args: 3840


  type FT = Int

  val MIN_DATE = 0
  val MAX_DATE = 9999
  val MIN_DISC = 0
  val MAX_DISC = 9999
  val margin = 1

  val innerPar = 16
  val outerPar = 2

  val tileSize = 384

  @virtualize
  def tpchq6[T:Type:Num](datesIn: Array[Int], quantsIn: Array[Int], disctsIn: Array[T], pricesIn: Array[T]): T = {
    val dataSize = ArgIn[Int]
    setArg(dataSize, datesIn.length)

    val dates  = DRAM[Int](dataSize)
    val quants = DRAM[Int](dataSize)
    val discts = DRAM[T](dataSize)
    val prices = DRAM[T](dataSize)
    val minDateIn = MIN_DATE
    val maxDateIn = MAX_DATE
    val out = ArgOut[T]

    val ts = tileSize (96 -> 96 -> 192000)
    val op = outerPar (1 -> 6)
    val ip = innerPar (1 -> 384)
    val lp = 16 (1 -> 384)

    setMem(dates, datesIn)
    setMem(quants, quantsIn)
    setMem(discts, disctsIn)
    setMem(prices, pricesIn)

    Accel {
      val minDate = minDateIn
      val maxDate = maxDateIn

      val acc = Reg[T]
      Reduce(acc)(dataSize by ts par op){ i =>
        val datesTile  = SRAM[Int](ts)
        val quantsTile = SRAM[Int](ts)
        val disctsTile = SRAM[T](ts)
        val pricesTile = SRAM[T](ts)
        Parallel {
          datesTile  load dates(i::i+ts par lp)
          quantsTile load quants(i::i+ts par lp)
          disctsTile load discts(i::i+ts par lp)
          pricesTile load prices(i::i+ts par lp)
        }
        Reduce(Reg[T])(ts par ip){ j =>
          val date  = datesTile(j)
          val disct = disctsTile(j)
          val quant = quantsTile(j)
          val price = pricesTile(j)
          val valid = date > minDate && date < maxDate && disct >= MIN_DISC.to[T] && disct <= MAX_DISC.to[T] && quant < 24
          mux(valid, price * disct, 0.to[T])
        }{_+_}
      }{_+_}

      out := acc
    }
    getArg(out)
  }

  @virtualize
  def main() {
    val N = args(0).to[Int]

    // val dates  = Array.fill(N){random[Int](20) + 65}
    // val quants = Array.fill(N){random[Int](25) }
    // // val discts = Array.fill(N){random[FT] * 0.05f + 0.02f}
    // // val prices = Array.fill(N){random[FT] * 1000f}
    // val discts = Array.fill(N){random[FT] /*/ 100000*/}
    // val prices = Array.fill(N){random[FT] /*/ 100000*/}

    val dates  = Array.tabulate[Int](N){i => i % 256 } // Standard array
    val quants = Array.tabulate[Int](N){i => i % 256 } // Standard array
    val discts = Array.tabulate[FT](N){i => i % 256 } // Standard array
    val prices = Array.tabulate[FT](N){i => i % 256 } // Standard array

    val result = tpchq6(dates, quants, discts, prices)

    // --- software version
    val conds = Array.tabulate(N){i => dates(i) > MIN_DATE && dates(i) < MAX_DATE  &&
                                       quants(i) < 24 && discts(i) >= MIN_DISC  && discts(i) <= MAX_DISC}
    // printArr(conds, "conds: ")

    val gold = Array.tabulate(N){i => if (conds(i)) prices(i) * discts(i) else 0.to[FT] }.reduce{_+_}

    println("expected " + gold)
    println("result " + result)

    val cksum = (gold < result + margin && gold > result - margin)
    println("PASS: " + cksum + " (TPCHQ6)")
  }
}

object BTC extends SpatialApp { // Regression (Dense) // Args: 0100000081cd02ab7e569e8bcd9317e2fe99f2de44d49ab2b8851ba4a308000000000000e320b6c2fffc8d750423db8b1eb942ae710e951ed797f7affc8892b0f1fc122bc7f5d74df2b9441a42a14695
  /*
    According to https://en.bitcoin.it/wiki/Block_hashing_algorithm
    Proof of Work = SHA256(SHA256(HEADER))

  */

  type ULong = FixPt[FALSE, _32, _0]
  type UInt8 = FixPt[FALSE, _8, _0]
  @virtualize
  def main() = {
    // Setup off-chip data

    val raw_text = args(0).to[MString]// loadCSV1D[String]("/remote/regression/data/machsuite/sha_txt.csv", "\n").apply(0)
    val data_text_int = argon.lang.String.string2num(raw_text)
    val data_text = Array.tabulate(data_text_int.length){i => data_text_int(i).to[UInt8]}
    val len = HostIO[Int]
    setArg(len, data_text.length)
    val text_dram = DRAM[UInt8](1024)
    val hash_dram = DRAM[UInt8](32)//(5)

    println("Hashing: " + raw_text + " (len: " + data_text.length + ")")
    setMem(text_dram, data_text)

    Accel{

      // Init
      val datalen = Reg[Int](0)
      val bitlen = RegFile[ULong](2, List(0.to[ULong],0.to[ULong]))
      val state = RegFile[ULong](8, List(0x6a09e667L.to[ULong],0xbb67ae85L.to[ULong],0x3c6ef372L.to[ULong],0xa54ff53aL.to[ULong],
                                       0x510e527fL.to[ULong],0x9b05688cL.to[ULong],0x1f83d9abL.to[ULong],0x5be0cd19L.to[ULong])
                              )
      val hash = SRAM[UInt8](32)
      val K_LUT = LUT[ULong](64)(
            0x428a2f98L.to[ULong],0x71374491L.to[ULong],0xb5c0fbcfL.to[ULong],0xe9b5dba5L.to[ULong],0x3956c25bL.to[ULong],0x59f111f1L.to[ULong],0x923f82a4L.to[ULong],0xab1c5ed5L.to[ULong],
            0xd807aa98L.to[ULong],0x12835b01L.to[ULong],0x243185beL.to[ULong],0x550c7dc3L.to[ULong],0x72be5d74L.to[ULong],0x80deb1feL.to[ULong],0x9bdc06a7L.to[ULong],0xc19bf174L.to[ULong],
            0xe49b69c1L.to[ULong],0xefbe4786L.to[ULong],0x0fc19dc6L.to[ULong],0x240ca1ccL.to[ULong],0x2de92c6fL.to[ULong],0x4a7484aaL.to[ULong],0x5cb0a9dcL.to[ULong],0x76f988daL.to[ULong],
            0x983e5152L.to[ULong],0xa831c66dL.to[ULong],0xb00327c8L.to[ULong],0xbf597fc7L.to[ULong],0xc6e00bf3L.to[ULong],0xd5a79147L.to[ULong],0x06ca6351L.to[ULong],0x14292967L.to[ULong],
            0x27b70a85L.to[ULong],0x2e1b2138L.to[ULong],0x4d2c6dfcL.to[ULong],0x53380d13L.to[ULong],0x650a7354L.to[ULong],0x766a0abbL.to[ULong],0x81c2c92eL.to[ULong],0x92722c85L.to[ULong],
            0xa2bfe8a1L.to[ULong],0xa81a664bL.to[ULong],0xc24b8b70L.to[ULong],0xc76c51a3L.to[ULong],0xd192e819L.to[ULong],0xd6990624L.to[ULong],0xf40e3585L.to[ULong],0x106aa070L.to[ULong],
            0x19a4c116L.to[ULong],0x1e376c08L.to[ULong],0x2748774cL.to[ULong],0x34b0bcb5L.to[ULong],0x391c0cb3L.to[ULong],0x4ed8aa4aL.to[ULong],0x5b9cca4fL.to[ULong],0x682e6ff3L.to[ULong],
            0x748f82eeL.to[ULong],0x78a5636fL.to[ULong],0x84c87814L.to[ULong],0x8cc70208L.to[ULong],0x90befffaL.to[ULong],0xa4506cebL.to[ULong],0xbef9a3f7L.to[ULong],0xc67178f2L.to[ULong]
          )

      val data = SRAM[UInt8](64)
      
      def SHFR(x: ULong, y: Int): ULong = {
        val tmp = Reg[ULong](0)
        tmp := x
        Foreach(y by 1){_ => tmp := tmp >> 1}
        tmp.value
      }

      // DBL_INT_ADD treats two unsigned ints a and b as one 64-bit integer and adds c to it
      def DBL_INT_ADD(c:ULong): Unit = {
        if (bitlen(0) > 0xffffffffL.to[ULong] - c) {bitlen(1) = bitlen(1) + 1}
        bitlen(0) = bitlen(0) + c
      }

      def SIG0(x:ULong): ULong = {
        // (ROTRIGHT(x,7) ^ ROTRIGHT(x,18) ^ ((x) >> 3))
        ( x >> 7 | x << (32-7) ) ^ ( x >> 18 | x << (32-18) ) ^ x >> 3
      }

      def SIG1(x:ULong): ULong = {
        // (ROTRIGHT(x,17) ^ ROTRIGHT(x,19) ^ ((x) >> 10))
        ( x >> 17 | x << (32-17) ) ^ ( x >> 19 | x << (32-19) ) ^ x >> 10
      }

      def CH(x:ULong, y:ULong, z:ULong): ULong = {
        // (((x) & (y)) ^ (~(x) & (z)))
        (x & y) ^ (~x & z)
      }

      def MAJ(x:ULong, y:ULong, z:ULong): ULong = {
        // (((x) & (y)) ^ ((x) & (z)) ^ ((y) & (z)))
        (x & y) ^ (x & z) ^ (y & z)
      }

      def EP0(x: ULong): ULong = {
        // (ROTRIGHT(x,2) ^ ROTRIGHT(x,13) ^ ROTRIGHT(x,22))
        ( x >> 2 | x << (32-2) ) ^ ( x >> 13 | x << (32-13) ) ^ ( x >> 22 | x << (32-22) )
      }

      def EP1(x: ULong): ULong = {
        // (ROTRIGHT(x,6) ^ ROTRIGHT(x,11) ^ ROTRIGHT(x,25))
        ( x >> 6 | x << (32-6) ) ^ ( x >> 11 | x << (32-11) ) ^ ( x >> 25 | x << (32-25) )
      }

      def sha_transform(): Unit = {
        val m = SRAM[ULong](64)
        Foreach(0 until 64 by 1){i => 
          if ( i.to[Index] < 16 ) {
            val j = 4*i.to[Index]
            // println(" m(" + i + ") = " + {(data(j).as[ULong] << 24) | (data(j+1).as[ULong] << 16) | (data(j+2).as[ULong] << 8) | (data(j+3).as[ULong])})
            m(i) = (data(j).as[ULong] << 24) | (data(j+1).as[ULong] << 16) | (data(j+2).as[ULong] << 8) | (data(j+3).as[ULong])
          } else {
            // println(" m(" + i + ") = " + SIG1(m(i-2)) + " " + m(i-7) + " " + SIG0(m(i-15)) + " " + m(i-16))
            m(i) = SIG1(m(i-2)) + m(i-7) + SIG0(m(i-15)) + m(i-16)
          } 
          // val j = 4*i.to[Index]
          // m(i) = if (i.to[Index] < 16) {(data(j).as[ULong] << 24) | (data(j+1).as[ULong] << 16) | (data(j+2).as[ULong] << 8) | (data(j+3).as[ULong])}
          //        else {SIG1(m(i-2)) + m(i-7) + SIG0(m(i-15)) + m(i-16)}
        }
        val A = Reg[ULong] 
        val B = Reg[ULong] 
        val C = Reg[ULong] 
        val D = Reg[ULong] 
        val E = Reg[ULong] 
        val F = Reg[ULong] 
        val G = Reg[ULong] 
        val H = Reg[ULong] 

        A := state(0)
        B := state(1)
        C := state(2)
        D := state(3)
        E := state(4)
        F := state(5)
        G := state(6)
        H := state(7)

        Foreach(64 by 1){ i => 
          val tmp1 = H + EP1(E) + CH(E,F,G) + K_LUT(i) + m(i)
          val tmp2 = EP0(A) + MAJ(A,B,C)
          // println(" " + i + " : " + A.value + " " + B.value + " " + 
          //   C.value + " " + D.value + " " + E.value + " " + F.value + " " + G.value + " " + H.value)
          // println("    " + H.value + " " + EP1(E) + " " + CH(E,F,G) + " " + K_LUT(i) + " " + m(i))
          H := G; G := F; F := E; E := D + tmp1; D := C; C := B; B := A; A := tmp1 + tmp2
        }

        Foreach(8 by 1 par 8){i => 
          state(i) = state(i) + mux(i.to[Index] == 0, A, mux(i.to[Index] == 1, B, mux(i.to[Index] == 2, C, mux(i.to[Index] == 3, D, 
            mux(i.to[Index] == 4, E, mux(i.to[Index] == 5, F, mux(i.to[Index] == 6, G, H)))))))
        }

      }

      def SHA256(): Unit = {
        // Init 
        Pipe{
          bitlen.reset
          state.reset
        }

        // Update
        Sequential.Foreach(0 until len.value by 64 par 1) { i => 
          datalen := min(len.value - i, 64)
          // println(" datalen " + datalen.value + " and i " + i + " and len " + len.value)
          data load text_dram(i::i+datalen.value)
          if (datalen.value == 64.to[Int]) {
            // println("doing this " + datalen.value)
            sha_transform()
            DBL_INT_ADD(512); 
          }
        }

        // Final
        val pad_stop = if (datalen.value < 56) 56 else 64
        Foreach(datalen until pad_stop by 1){i => data(i) = if (i.to[Index] == datalen) 0x80.to[UInt8] else 0.to[UInt8]}
        if (datalen.value >= 56) {
          sha_transform()
          Foreach(56 by 1){i => data(i) = 0}
        }

        DBL_INT_ADD(datalen.value.to[ULong] * 8.to[ULong])
        Pipe{data(63) = (bitlen(0)).to[UInt8]}
        Pipe{data(62) = (bitlen(0) >> 8).to[UInt8]}
        Pipe{data(61) = (bitlen(0) >> 16).to[UInt8]}
        Pipe{data(60) = (bitlen(0) >> 24).to[UInt8]}
        Pipe{data(59) = (bitlen(1)).to[UInt8]}
        Pipe{data(58) = (bitlen(1) >> 8).to[UInt8]}
        Pipe{data(57) = (bitlen(1) >> 16).to[UInt8]}
        Pipe{data(56) = (bitlen(1) >> 24).to[UInt8]}
        sha_transform()

        // Foreach(8 by 1){i => println(" " + state(i))}

        Sequential.Foreach(4 by 1){ i => 
          hash(i)    = (SHFR(state(0), (24-i.to[Index]*8))).apply(7::0).as[UInt8]
          hash(i+4)  = (SHFR(state(1), (24-i.to[Index]*8))).apply(7::0).as[UInt8]
          hash(i+8)  = (SHFR(state(2), (24-i.to[Index]*8))).apply(7::0).as[UInt8]
          hash(i+12) = (SHFR(state(3), (24-i.to[Index]*8))).apply(7::0).as[UInt8]
          hash(i+16) = (SHFR(state(4), (24-i.to[Index]*8))).apply(7::0).as[UInt8]
          hash(i+20) = (SHFR(state(5), (24-i.to[Index]*8))).apply(7::0).as[UInt8]
          hash(i+24) = (SHFR(state(6), (24-i.to[Index]*8))).apply(7::0).as[UInt8]
          hash(i+28) = (SHFR(state(7), (24-i.to[Index]*8))).apply(7::0).as[UInt8]
        }

      }

      Sequential.Foreach(2 by 1){i => 
        Pipe{SHA256()}
        if (i.to[Index] == 0) {
          text_dram(0::32) store hash
          len := 32
        }
      }
      
      hash_dram store hash
    }  

    val hashed_result = getMem(hash_dram)
    val hashed_gold = Array[UInt8](101.to[UInt8],0.to[UInt8],241.to[UInt8],59.to[UInt8],194.to[UInt8],84.to[UInt8],197.to[UInt8],158.to[UInt8],159.to[UInt8],61.to[UInt8],119.to[UInt8],189.to[UInt8],11.to[UInt8],25.to[UInt8],153.to[UInt8],230.to[UInt8],134.to[UInt8],250.to[UInt8],223.to[UInt8],119.to[UInt8],101.to[UInt8],174.to[UInt8],43.to[UInt8],89.to[UInt8],38.to[UInt8],109.to[UInt8],29.to[UInt8],131.to[UInt8],91.to[UInt8],134.to[UInt8],144.to[UInt8],131.to[UInt8])
    printArray(hashed_gold, "Expected: ")
    printArray(hashed_result, "Got: ")

    val cksum = hashed_gold.zip(hashed_result){_==_}.reduce{_&&_}
    println("PASS: " + cksum + " (BTC)")

  }
}
