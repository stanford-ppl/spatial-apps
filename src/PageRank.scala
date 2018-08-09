import spatial.dsl._
import spatial.targets._
import virtualized._

object PageRank extends SpatialApp { // Regression (Sparse) // Args: 50 0.125
  override val target = targets.Default

  val ts = 16 // param
  val page_par = 2 // param (1, min(<ts>, 3), 1)

  val tile_par = 1 // Do not change
  val par_load = 16
  val par_store = 16
  type Elem = Float // FixPt[TRUE,_16,_16]
  type X = Float // FixPt[TRUE,_16,_16]

  val I = 1 // param

  val damp = 0.125f

  /*
    Currently testing with DIMACS10 Chesapeake dataset from UF Sparse Matrix collection

  */
  val margin = 0.5f

  @virtualize
  def main() {
    val sparse_data = loadCSV2D[Int](sys.env("SPATIAL_HOME") + "/apps/data/pagerank/pagerank_chesapeake.csv", " ", "\n").transpose
    val rows = sparse_data(0,0)
    val node1_list = Array.tabulate(sparse_data.cols - 1){i => sparse_data(0, i+1)-1} // Every page is 1-indexed...
    val node2_list = Array.tabulate(sparse_data.cols - 1){i => sparse_data(1, i+1)-1} // Every page is 1-indexed...
    // Preprocess to get frontier sizes.  We can do this on chip also if we wanted
    println("Matrix has " + rows + " rows")
    println("node2_list.length " + node2_list.length)
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

    // Arguments
    val iters = ArgIn[Int]
    val NP    = ArgIn[Int]
    val NE    = ArgIn[Int]
    setArg(iters, I)
    setArg(NP, rows); bound(rows) = 39
    setArg(NE, node2_list.length); bound(node2_list.length) = 340

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
        Foreach(NP by ts par tile_par){page => 
          val local_pages = SRAM.buffer[X](ts)
          val local_edgeIds = SRAM[Int](ts)
          val local_edgeLens = SRAM[Int](ts)
          local_pages load OCpages(page::page+ts par par_load)
          local_edgeLens load OCedgeLens(page::page+ts par par_load)
          local_edgeIds load OCedgeIds(page::page+ts par par_load)
          // Process each page in local tile
          Sequential.Foreach(ts by 1 par page_par){local_page => 
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
              if (tmp >= page && tmp < page+ts) {
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
            val pagerank = Pipe.Reduce(Reg[X](0))(len by 1){i => 
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
            local_pages(local_page) = pagerank * damp + (1f - damp).to[X]
          }
          OCpages(page::page+ts par par_store) store local_pages
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
        gold(i) = pr * damp + (1f - damp).to[X]
      }
    }

    println("PageRank on DIMACS10 Chesapeake dataset downloaded from UF Sparse Matrix collection")
    printArray(gold, "gold: ")
    printArray(result, "result: ")
    val cksum = result.zip(gold){ case (o, g) => (g < (o + margin.to[X])) && g > (o - margin.to[X])}.reduce{_&&_}
    println("PASS: " + cksum + " (PageRank)")

  }

}

