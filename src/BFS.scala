import spatial.dsl._
import spatial.targets._
import virtualized._

object BFS extends SpatialApp { // DISABLED Regression (Sparse) // Args: 6 10

  val E = 9600000 // param
  val ts = 8000 // param

  val N = ts
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
      val frontierNodes = SRAM[Int](ts)
      val frontierCounts = SRAM[Int](ts)
      val frontierIds = SRAM[Int](ts)
      val frontierLevels = SRAM[Int](ts)
      val currentNodes = SRAM[Int](ts)
      val pieceMem = SRAM[Int](ts)

      val concatReg = Reg[Int](0)
      val numEdges = Reg[Int](1)

      // Flush first few for scatter safety
      Foreach(anpe by 1){i =>  //dummy read of anpe
        frontierNodes(i) = 0
        // frontierCounts(i) = 0.to[Int]
        frontierLevels(i) = 0
        currentNodes(i) = 0
        pieceMem(i) = 0
      }
      Parallel {
        frontierIds load ids(0 :: ts)
        frontierCounts load counts(0 :: ts)
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

          // val frontierAddr = SRAM[Int](ts)
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
          concatReg := min(ts, concatReg + nextLen)
        }

        Foreach(concatReg by 1) { kk => currentNodes(kk) = frontierNodes(kk) }
        Foreach(concatReg by 1) { kk => frontierLevels(kk) = i + 1 }
        result(currentNodes, concatReg) scatter frontierLevels
        numEdges := concatReg
        concatReg := 0
      }
    }

    getMem(result)
  }

  @virtualize
  def main() {
    /* NEW VERSION FOR PERFORMANCE MEASUREMENTS */
    val average_nodes_per_edge = args(0).to[Int]
    val spacing = 3 

    val OCnodes = Array.tabulate(N) {i => 0.to[Int]}
    val OCedges = Array.tabulate(E){ i => i*2 % N}
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
