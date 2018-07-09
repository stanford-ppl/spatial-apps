import spatial.dsl._
import spatial.targets._
import virtualized._

object Kmeans extends SpatialApp { // Regression (Dense) // Args: 3 64
  override val target = targets.Default
  type X = Int

  val I = 2
  val N = 64
  val K = 16 
  val D = 32
  val ts = 16
  val ip = 16
  val op = 1
  val mp1 = 2
  val mp2 = 1
  val mp3 = 1

  val DM1 = D - 1
  val element_max = 10
  val margin = (element_max * 0.2).to[X]

  @virtualize
  def kmeans[T:Type:Num](points_in: Array[T], cent_inits: Array[T]) = {

    val iters = ArgIn[Int]
    val N     = ArgIn[Int]

    setArg(iters, I)
    setArg(N, Kmeans.N)

    val points = DRAM[T](N, D)    // Input points
    val centroids = DRAM[T](K, D) // Output centroids
    setMem(points, points_in)


    Accel {
      val cts = SRAM[T](K, D)

      // Load initial centroids (from points)
      cts load points(0::K, 0::D par ip)

      Sequential.Foreach(iters by 1){epoch =>
        // For each set of points
        val newCents = MemReduce(SRAM[T](K,D) par ip)(N by ts par op){i =>
          val pts = SRAM[T](ts, D)
          pts load points(i::i+ts, 0::D par ip)

          // For each point in this set
          MemReduce(SRAM[T](K,D) par ip)(ts par mp1){pt =>
            // Find the index of the closest centroid
            val dists = SRAM[T](K)
            Foreach(K by 1 par mp2) { ct =>
              val dist = Reduce(Reg[T])(D par ip){d => (pts(pt,d) - cts(ct,d)) ** 2 }{_+_}
              dists(ct) = dist.value
            }
            val minDist = Reduce(Reg[T])(K by 1 par ip) { ct => dists(ct) } { (a,b) => min(a, b) }
            val minCent = Reduce(Reg[Index])(K by 1 par ip) { ct => 
              mux(dists(ct) == minDist.value, ct, -1)
            } { (a,b) => max(a,b) }

            // Store this point to the set of accumulators
            val localCent = SRAM[T](K,D)
            Foreach(K by 1, D par ip){(ct,d) =>
              localCent(ct, d) = mux(ct == minCent.value, pts(pt,d), 0.to[T])
            }
            localCent
          }{_+_} // Add the current point to the accumulators for this centroid
        }{_+_}

        // Average each new centroid
        Foreach(K by 1 par mp3){ ct =>
          val centCount = Reg[T](0.to[T])
          Pipe {
            centCount := max(newCents(ct,DM1), 1.to[T])
          }
          Foreach(D par ip){ d =>
            cts(ct, d) = mux(centCount == 0.to[T], 0.to[T], newCents(ct,d) / centCount) //updateMux
          }
        }
      }

      // Store the centroids out
      centroids(0::K, D par ip) store cts
    }

    getMem(centroids)
  }

  @virtualize
  def main() {

    val pts = Array.tabulate(N){i => Array.tabulate(D){d => if (d == D-1) 1.to[X] else random[X](element_max) + i }}
    val cnts = Array.tabulate(K){i => Array.tabulate(D){d => if (d == D-1) 1.to[X] else random[X](element_max) + (i*N/K) }}

    val result = kmeans(pts.flatten, cnts.flatten)

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
