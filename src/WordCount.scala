import spatial.dsl._
import virtualized._

object WordCount extends SpatialApp { // Regression (Dense) // Args: 640

  type T = Int

  // Final params
  val N = 4
  val numMap = 2
  val B = 4
  val C = 8
  val ip = 1

  @virtualize
  def wordCount(wordInits:scala.Array[Array[T]]) = {
    val mergedDram = DRAM[T](C)
    val wordDrams = scala.Array.tabulate(numMap) { i => 
      val dram = DRAM[T](N)
      setMem(dram, wordInits(i))
      dram
    }

    Accel {
      val counts = scala.Array.tabulate(numMap) { k =>
        val word = SRAM[T](B)
        val count = SRAM[T](C)
        word load wordDrams(k)(0::B par ip)
        Foreach(C by 1 par ip) { i =>
          count(i) = 0
        }
        Sequential.Foreach(B by 1) { i =>
          val idx = word(i)
          count(idx) = count(idx) + 1
        }
        count
      }

      val merged = SRAM[T](C)
      Foreach(C par ip) { i =>
        merged(i) = counts.map { count =>
          count(i)
        }.reduce[T] { case (a,b) => a + b }
      }

      mergedDram(0::B par ip) store merged
    }
    getMem(mergedDram)
  }

  @virtualize
  def main() {
    val wordInits = scala.Array(
      Array[T](1,2,3,3),
      Array[T](3,2,3,2)
    )
    val result = wordCount(wordInits).mkString(",")
    println(result)
  }
}


