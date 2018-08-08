import spatial.dsl._
import virtualized._

// The goal is to create a fused matmult for gate computes, and make sure that the SRAMs are stored within the Foreach loop so that I can par it...
// Also need to reduce the # of streams. The original ones take too much memstreams

object ParWriteSRAMTransfer extends SpatialApp {
  // stage 1: Foreach controller wrapping SRAM. That way I can par. write to the SRAM
  // stage 2: Get the SRAM and do some work with it, then flush the result.
  type X = FixPt[TRUE, _3, _5]

  @virtualize
  def main() {
    val M = 64
    val P = 32
    val N = 32
    val bm = 8
    val bn = 8
    val bp = 4

    val outerPar = 2
    val midPar = 2
    val innerPar = 2
    val elePar = 2
    val reducePar = 2

    val a = Array.tabulate(M){ j => Array.tabulate(P){ i => ((i + j * P) % 8).to[X] } }.flatten // Standard array
    val b = Array.tabulate(P){ j => Array.tabulate(N){ i => ((i + j * N) % 8).to[X] } }.flatten // Standard array

    val A = DRAM[X](M, P)
    val B = DRAM[X](P, N)
    val C = DRAM[X](M, N)
    setMem(A, a)
    setMem(B, b)

    Accel {
      // Top-level data feeder
      val resultMem = SRAM[X](M,N)
      Foreach (M by bm, N by bn par outerPar) { (ii, jj) =>
        Foreach (P by bp par midPar) { kk =>
          val tileA = SRAM[X](bm, bp)
          val tileB = SRAM[X](bp, bn)
          Parallel {
            tileA load A(ii::ii+bm, kk::kk+bp)
            tileB load B(kk::kk+bp, jj::jj+bn)
          }

          Foreach(bm by 1, bn by 1 par innerPar) { (iii, jjj) =>
            val prod = Reduce(Reg[X])(bp by 1 par reducePar) { kkk => tileA(iii, kkk) * tileB(kkk, jjj)}{_+_}
            val prev = mux(kk == 0, 0.to[X], resultMem(ii*bm+iii, jj*bm+jjj))
            resultMem(ii*bm+iii, jj*bn+jjj) = 0.to[X]// + prev
          }
        }
      }

      // Mid-level: do something on the result SRAM and flush it back
      Foreach (M by 1, N by 1 par elePar) { (ii, jj) =>
        resultMem(ii, jj) = resultMem(ii, jj) + 1.to[X]
      }
      C(0::M, 0::N) store resultMem
    }

    val result = getMem(C)
    printArray(result, "result mem = ")
  }
}

// I may need to do explicit parallelism here instead of using the meta programming method...


// This guy should calculate [x, hidden] * kernel + bias, where bias, cat(x, feature) are SRAM, and kernel is tiled...
object gateOps extends SpatialApp { // Regression (Dense) // Args: 32 128 128
  type X = FixPt[TRUE,_16,_16]
  val batchSize = 1
  val featureSize = 32
  val hiddenSize = 32
  val reduceSize = featureSize + hiddenSize
  val outputSize = 4 * hiddenSize

  val M = batchSize.to[Int]
  val N = outputSize.to[Int]
  val P = reduceSize.to[Int]

  val innerPar = 16
  val midPar = 2
  val outerPar = 2

  val tsm = 16
  val tsn = 64
  val tsp = 64

  @virtualize
  def MatMult_outer[T:Type:Num](A: Array[T], B: Array[T], C_init: Array[T], mm: Int, nn: Int, pp: Int) = {
    val a = DRAM[T](M, P)
    val b = DRAM[T](P, N)
    val c = DRAM[T](M, N)

    val op = outerPar
    val mp = midPar
    val ip = innerPar
    val px = 1

    val bm = tsm
    val bn = tsn
    val bp = tsp

    setMem(a, A)
    setMem(b, B)
    setMem(c, C_init)

    Accel {
      val tileC = SRAM[T](M,N)
      tileC load c(0::M, 0::N)
      Sequential.Foreach(M by bm, N by bn par op) { (i,j) =>
        MemFold(tileC)(P by bp) { k =>
          val tileA = SRAM[T](bm, bp)
          val tileB = SRAM[T](bp, bn)
          val accum = SRAM[T](bm, bn)
          Parallel {
            tileA load a(i::i+bm, k::k+bp par ip)
            tileB load b(k::k+bp, j::j+bn par ip)
          }

          MemReduce(accum)(bp by 1 par mp){ kk =>
            val tileC_partial = SRAM[T](bm,bn)
            Foreach(bm by 1, bn by 1 par ip){ (ii,jj) =>
              tileC_partial(ii,jj) = tileA(ii,kk) * tileB(kk,jj)
            }
            tileC_partial
          }{_+_}

        }{_+_}
      }

      c(0::M, 0::N par ip) store tileC
    }
    getMem(c)
  }


  @virtualize
  def main() = {
    val a = Array.tabulate(M){ j => Array.tabulate(P){ i => ((i + j * P) % 8).to[X] } } // Standard array
    val b = Array.tabulate(P){ j => Array.tabulate(N){ i => ((i + j * N) % 8).to[X] } } // Standard array
    val c_init = Array.fill(M){ Array.fill(N){ 0.to[X] } }

    val result = MatMult_outer(a.flatten, b.flatten, c_init.flatten, M, N, P)

    val gold = Array.tabulate(M){i =>
      val aRow = a(i)
      Array.tabulate(N){j =>
        val bCol = b.map{row => row(j)}
        aRow.zip(bCol){_*_}.reduce{_+_}
      }
    }.flatten

    println("expected cksum: " + gold.map(a => a).reduce{_+_})
    println("result cksum: " + result.map(a => a).reduce{_+_})
    printArray(gold, "Gold: ")
    printArray(result, "Result: ")

    val cksum = result.zip(gold){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (MatMult_outer)")
  }
}
