import spatial.dsl._
import virtualized._

object WordCount2 extends SpatialApp { // Regression (Dense) // Args: 640

  type T = Int

  // Final params
  val B = 4 // Blocking size of word sram
  val C = 8 // Max size of the word count. assuming fit on chip
  val numMap = 2  // Number of parallel mapper
  val ip = 1 // SIMD vectorization

  @virtualize
  def wordCount(wordInit:Array[T]) = {
    val N = wordInit.length // Total size of the word dram
    val size = ArgIn[Int]
    setArg(size, N)
    val mergedDram = DRAM[T](C)
    val wordDram = DRAM[T](size)
    setMem(wordDram, wordInit)
    Accel {
      val merged = SRAM[T](C)
      // With MemReduce, reducer will be parallelized automatically when mapper are parallelized
      // With 8 mappers, there will be 4 + 2 + 1 reducers folled by a accumulator 
      MemReduce(merged)(size by B par numMap) { k => 
        val word = SRAM[T](B)
        word load wordDram(k::k+B par ip)
        val count = SRAM[T](C)
        Pipe { // Sequentialize the two stages below
          Foreach(C by 1 par ip) { i =>
            count(i) = 0
          }
          Sequential.Foreach(B by 1) { i =>
            val idx = word(i)
            count(idx) = count(idx) + 1
          }
        }
        count
      }{_+_}

      mergedDram(0::B par ip) store merged
    }
    getMem(mergedDram)
  }

  @virtualize
  def main() {
    val wordInit = Array[T](1,2,3,3,3,2,3,2)
    val result = wordCount(wordInit).mkString(",")
    println(result)
  }
}


