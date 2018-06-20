import spatial.dsl._
import spatial.targets._
import virtualized._

object Kmeans extends SpatialApp { // Regression (Dense) // Args: 3 64
  override val target = AWS_F1

  type X = Int

  val I = 1 // param
  val N = 10240  // param
  val K = 16 // param
  val D = 32 // param
  val ts = 1024 // param

  val op = 1 // param (1, <N> / <ts>, 1)
  val mp = 1 // param (1, 5, min(1, <ts>))
  val ip = 16
  val PX = 1

  val element_max = 10
  val margin = (element_max * 0.2).to[X]

  @virtualize
  def kmeans[T:Type:Num](points_in: Array[T], cent_inits: Array[T], numPoints: Int, numCents: Int, numDims: Int, it: Int) = {
    bound(numPoints) = Kmeans.N
    bound(numCents) = Kmeans.K
    bound(numDims) = Kmeans.D

    val par_load = ip
    val par_store = ip

    val iters = ArgIn[Int]
    val N     = ArgIn[Int]
    val K     = ArgIn[Int]
    val D     = ArgIn[Int]

    setArg(iters, it)
    setArg(N, numPoints)
    setArg(K, numCents)
    setArg(D, numDims)

    val points = DRAM[T](numPoints, numDims)    // Input points
    val centroids = DRAM[T](numCents, numDims) // Output centroids
    // val init_cents = DRAM[T](K,D) // Output centroids
    setMem(points, points_in)
    // setMem(init_cents, cent_inits)


    Accel {
      val cts = SRAM[T](numCents, numDims)
      val newCents = SRAM[T](numCents,numDims)

      // Load initial centroids (from points)
      cts load points(0::K, 0::D par par_load)

      val DM1 = D - 1

      Sequential.Foreach(iters by 1){epoch =>
        // For each set of points
        Foreach(N by ts par op){i =>
          val pts = SRAM[T](ts, numDims)
          pts load points(i::i+ts, 0::D par par_load)

          // For each point in this set
          MemFold(newCents par ip)(ts par mp){pt =>
            // Find the index of the closest centroid
            val accum = Reg[Tup2[Int,T]]( pack(0.to[Int], 100000.to[T]) )
            val minCent = Reduce(accum)(K par PX){ct =>
              val dist = Reg[T](0.to[T])
              Reduce(dist)(D par ip){d => (pts(pt,d) - cts(ct,d)) ** 2 }{_+_}
              pack(ct, dist.value)
            }{(a,b) =>
              mux(a._2 < b._2, a, b)
            }

            // Store this point to the set of accumulators
            val localCent = SRAM[T](numCents,numDims)
            Foreach(K by 1, D par ip){(ct,d) =>
              //val elem = mux(d == DM1, 1.to[T], pts(pt, d)) // fix for vanishing mux
              val elem = pts(pt,d)
              localCent(ct, d) = mux(ct == minCent.value._1, elem, 0.to[T])
            }
            localCent
          }{_+_} // Add the current point to the accumulators for this centroid
        }

        val centCount = SRAM[T](numCents)
        Foreach(K by 1 par PX){ct => centCount(ct) = max(newCents(ct,DM1), 1.to[T]) } 

        // Average each new centroid
        // val centsOut = SRAM[T](K, D)
        Foreach(K by 1, D par ip){(ct,d) =>
          cts(ct, d) = mux(centCount(ct) == 0.to[T], 0.to[T], newCents(ct,d) / centCount(ct)) //updateMux
        }
      }

      centroids(0::K, 0::D par par_store) store cts
    }

    getMem(centroids)
  }

  @virtualize
  def main() {
    val pts = Array.tabulate(N){i => Array.tabulate(D){d => if (d == D-1) 1.to[X] else random[X](element_max) + i }}
    val cnts = Array.tabulate(K){i => Array.tabulate(D){d => if (d == D-1) 1.to[X] else random[X](element_max) + (i*N/K) }}

    val result = kmeans(pts.flatten, cnts.flatten, N, K, D, I)

    val cts = Array.empty[Array[X]](K)
    for (k <- 0 until K) {
      cts(k) = Array.tabulate(D){i => pts(k).apply(i) }
    }
    val ii = Array.tabulate(K){i => i}

    for(epoch <- 0 until I) {
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

    val cksum = result.zip(gold){ case (o, g) => (g < (o + margin)) && g > (o - margin)}.reduce{_&&_}

    println("PASS: " + cksum + " (Kmeans)")
  }
}

