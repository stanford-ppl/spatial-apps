import spatial.dsl._
import virtualized._
import spatial.targets._

object Kmeans23 extends SpatialApp { // Regression (Dense) // Args: 3 64
  override val target = AWS_F1

  type X = Int

  val numcents = 16
  val dim = 32
  val pts_per_ld = 1 // ???

  val element_max = 10
  val margin = (element_max * 0.2).to[X]

  val MAXK = numcents
  val MAXD = dim

  @virtualize
  def kmeans[T:Type:Num](points_in: Array[T], cent_inits: Array[T], numPoints: Int, numCents: Int, numDims: Int, it: Int) = {
    bound(numPoints) = 960000
    bound(numCents) = MAXK
    bound(numDims) = MAXD

    val BN = 4 (96 -> 96 -> 9600)
    val BD = MAXD
    val par_load = 16
    val par_store = 16
    val PX = 1//1 (1 -> 1)
    val P0 = 8//16 (1 -> 2 -> dim)
    val P1 = 8//16 (1 -> 2 -> dim)
    val P2 = 8//8 (1 -> 2 -> dim)

    val P3 = 6 
    val P4 = 4
    val P5 = 4

    val iters = ArgIn[Int]
    val N     = ArgIn[Int]
    val K     = numCents //ArgIn[Int]
    val D     = numDims //ArgIn[Int]

    setArg(iters, it)
    setArg(N, numPoints)
    // setArg(K, numCents)
    // setArg(D, numDims)

    val points = DRAM[T](N, D)    // Input points
    val centroids = DRAM[T](numcents,dim) // Output centroids
    // val init_cents = DRAM[T](K,D) // Output centroids
    setMem(points, points_in)
    // setMem(init_cents, cent_inits)


    Accel {
      val cts = SRAM[T](MAXK, MAXD)

      // Load initial centroids (from points)
      cts load points(0::K, 0::D par par_load)

      // Initialize newCents
      // FPGA:

      Sequential.Foreach(iters by 1){epoch =>

        val newCents = List.fill(P3)(SRAM[T](MAXK,MAXD))
        val centCounts = List.fill(P3)(SRAM[T](MAXK))
  
        Foreach(K by 1 par P0) {i => Parallel{centCounts.map(_(i) = 0.to[T])}}
        // Foreach(K by 1, D by 1 par P0){(i,j) => newCents(i,j) = 0.to[T]}

        // For each set of batch

        Parallel{
          List.tabulate(P3){p => 
            Foreach(N by P3 par PX){i =>
              val accum = Reg[Tup2[Int,T]]( pack(0.to[Int], 100000.to[T]) )
              val pts = SRAM[T](BD)
              pts load points(i+p, 0::BD par par_load)

              // Find the index of the closest centroid
              Reduce(accum)(K par PX){ct =>
                val dist = Reg[T](0.to[T])
                Reduce(dist)(D par P2){d => (pts(d) - cts(ct,d)) ** 2 }{_+_}
                pack(ct, dist.value)
              }{(a,b) =>
                mux(a._2 < b._2, a, b)
              }

              // println("mincent on " + i + " = idx " + accum.value._1 + " dist " + accum.value._2)

              // Store this point to the set of accumulators
              Sequential{ // Sequential avoids centCounts being buffered
                // println("pt " + {i + p} + " -> " + accum.value._1)
                Foreach(D by 1 par P5){ d => newCents(p)(accum.value._1, d) = pts(d) + mux(centCounts(p)(accum.value._1) == 0.to[T], 0.to[T], newCents(p)(accum.value._1, d)) }
                centCounts(p)(accum.value._1) = centCounts(p)(accum.value._1) + 1.to[T]
              }
            }
          }
          ()
        }

        // Average each new centroid
        // val centsOut = SRAM[T](MAXK, MAXD)
        Foreach(K by 1 par P4){ct =>
          val count = centCounts.map(_(ct)).reduce{_+_}
          Foreach(D by 1 par P1){d => 
            val data = List.tabulate(P3){i => mux(centCounts(i)(ct) == 0.to[T], 0.to[T], newCents(i)(ct,d))}.reduce{_+_}
            cts(ct, d) = mux(d == D-1, 1.to[T], mux(count == 0.to[T], 0.to[T], data/count))
          }
        }
      }

      // Store the centroids out
      centroids(0::K, 0::D par par_store) store cts
    }

    getMatrix(centroids)
  }

  @virtualize
  def main() {
    val iters = args(0).to[Int]
    val N = args(1).to[Int]
    val K = numcents //args(2).to[SInt];
    val D = dim //args(3).to[SInt];

    val pts = Array.tabulate(N){i => Array.tabulate(D){d => if (d == D-1) 1.to[X] else random[X](element_max) + i }}
    val cnts = Array.tabulate(K){i => Array.tabulate(D){d => if (d == D-1) 1.to[X] else random[X](element_max) + (i*N/K) }}

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
        val ans = dists.zip(ii){(a,b) => pack(a,b) }.reduce{(a,b) => if (a._1 < b._1) a else b}._2  // minIndex
        ans
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
    (0 until K).foreach{ i => 
      (0 until D).foreach{ j => print(result(i,j) + " ")}
      print("\n")
    }

    val cksum = result.flatten.zip(gold){ case (o, g) => (g < (o + margin)) && g > (o - margin)}.reduce{_&&_}

    println("PASS: " + cksum + " (Kmeans)")
  }
}
