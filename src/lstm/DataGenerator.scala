import spatial.dsl._
import virtualized._

trait DataGenerator extends SpatialApp {
  /**
  * Dummy data generation
  * TODO: order probably matters in this case. 
  */

  type sInt = scala.Int
  type sDouble = scala.Double
  
  val defaultRandomMax = 0.1

  @virtualize
  def genDRAMData1D[T:Type:Num](dim0: sInt, max: sDouble = defaultRandomMax): DRAM1[T] = {
    val k = DRAM[T](dim0)
    setMem(k, Array.fill (dim0) { 
      random[T](max.to[T])
    })
    k
  }

  @virtualize
  def genDRAMData2D[T:Type:Num] (dim0: sInt, dim1: sInt, max: sDouble = defaultRandomMax): DRAM2[T] = {
    val k = DRAM[T](dim0, dim1)
    setMem(k, Array.fill(dim0) {
      Array.fill(dim1) {
        random[T](max.to[T])
      }
    }.flatten)
    k
  }

  @virtualize
  def genDRAMData3D[T:Type:Num] (dim0: sInt, dim1: sInt, dim2: sInt,
    max: sDouble = defaultRandomMax): DRAM3[T] = {
    val k = DRAM[T](dim0, dim1, dim2)
    setMem(k, Array.fill(dim0) {
      Array.fill(dim1) {
        Array.fill(dim2) {
          random[T](max.to[T])
        }
      }.flatten
    }.flatten)
    k
  }

  @virtualize
  def genDRAMData4D[T:Type:Num] (dim0: sInt, dim1: sInt, dim2: sInt,
    dim3: sInt, max: sDouble = defaultRandomMax): DRAM4[T] = {
    val k = DRAM[T](dim0, dim1, dim2, dim3)
    setMem(k, Array.fill(dim0) {
      Array.fill(dim1) {
        Array.fill(dim2) {
          Array.fill(dim3) {
            random[T](max.to[T])
          }
        }.flatten
      }.flatten
    }.flatten)
    k
  }

  @virtualize
  def genDRAMData5D[T:Type:Num] (dim0: sInt, dim1: sInt, dim2: sInt, dim3: sInt, 
    dim4: sInt, max: sDouble = defaultRandomMax): DRAM5[T] = {
    val k = DRAM[T](dim0, dim1, dim2, dim3, dim4)
    setMem(k, Array.fill(dim0) {
      Array.fill(dim1) {
        Array.fill(dim2) {
          Array.fill(dim3) {
            Array.fill(dim4) {
              random[T](max.to[T])
            }
          }.flatten
        }.flatten
      }.flatten
    }.flatten)
    k 
  }
}
