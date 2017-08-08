import java.util.concurrent.{BlockingQueue, Executors, LinkedBlockingQueue, TimeUnit}

import org.virtualized._
import spatial.SpatialCompiler
import argon.core._
import spatial.aliases._

import scala.sys.process._

object BandwidthTests extends SpatialCompiler {

  sealed abstract class TxType
  case object Load extends TxType
  case object Store extends TxType
  case object Mixed extends TxType

  lazy val SPATIAL_HOME: String = sys.env.getOrElse("SPATIAL_HOME", {
    error("SPATIAL_HOME was not set!")
    error("Set top directory of spatial using: ")
    error("export SPATIAL_HOME=/path/to/spatial")
    sys.exit()
  })

  def synthesize(name: String): Unit = {
    try {
      val output = Seq("python", s"$SPATIAL_HOME/bin/scrape.py", s"${Config.cwd}/gen/$name").!!

      if (output.contains("failed") || output.contains("error") || output.contains("aborting")) {
        println(s"$name: FAIL (Synthesis)")
      }
      else println(s"$name: DONE")
    }
    catch { case e: Throwable =>
      println(s"$name: FAIL (Synthesis)")
    }
  }

  class Synthesis(id: Int, queue: BlockingQueue[String]) extends Runnable {
    var isAlive = true

    def run(): Unit = {
      println(s"[T$id] Started")

      while(isAlive) {
        val name = queue.take()
        if (!name.isEmpty) synthesize(name)
        else {
          isAlive = false
          println(s"[T#$id] Received kill signal")
        }
      }

      println(s"[T$id] Ended")
    }
  }

  // Total data (bits) = rows * cols * N * 100000 * nbits[T]
  @virtualize def transfer1D[T:Type:Bits](p: Int): Unit = {
    import spatial.dsl._
    val N_MAX = 16

    val drams = List.fill(N_MAX){ DRAM[T](1000000) }
    val N      = ArgIn[Int]
    val offset = ArgIn[Int]
    val store  = ArgIn[Int]
    val width  = ArgIn[Int]

    if (args.length < 4) {
      println("Args: #Txs isAlign isStore #Words")
      println("  isAlign: 0 = unaligned, 1 = aligned ")
      println("  isStore: 0 = loads, 1 = stores, 2 = mixed")
    }

    val ofs = if (args(1).to[Int] == 1.to[Int]) { 0.to[Int] } else { random[Int](16) + 5 }

    setArg(N, args(0).to[Int])
    setArg(offset, ofs)
    setArg(store, args(2).to[Int])
    setArg(width, args(3).to[Int])

    Accel {
      val n: Int = N.value
      val tx: Int = store.value
      val ofs: Int = offset.value
      val cmdWidth: Int = width.value
      val srams = List.fill(N_MAX){ SRAM[T](96) }  // Won't actually get all the writes but that's ok

      Foreach(100000 by 1){ i =>
        Parallel {
          srams.zip(drams).zipWithIndex.foreach { case ((sram, dram), j) =>
            Pipe {
              if (j.to[Int] < n) {
                if (tx == 0.to[Int] || (tx == 2.to[Int] && j % 2 == 0)) {
                  sram load dram(ofs :: cmdWidth+ofs par p)
                }
                if (tx == 1.to[Int] || (tx == 2.to[Int] && j % 2 == 1)) {
                  dram(ofs :: cmdWidth+ofs par p) store sram
                }
              }
            }
          }
        }
      }
    }
  }

  // Total data (bits) = rows * cols * N * 100000 * nbits[T]
  @virtualize def transfer2D[T:Type:Bits](p: Int): Unit = {
    import spatial.dsl._
    val N_MAX = 16

    val drams = List.fill(N_MAX){ DRAM[T](100,1000000) }
    val N      = ArgIn[Int]
    val offset = ArgIn[Int]
    val store  = ArgIn[Int]
    val rows   = ArgIn[Int]
    val cols   = ArgIn[Int]

    if (args.length < 5) {
      println("Args: #Txs isAlign isStore #Rows #Cols")
      println("  isAlign: 0 = unaligned, 1 = aligned")
      println("  isStore: 0 = loads, 1 = stores, 2 = mixed")
    }

    val ofs = if (args(1).to[Int] == 1.to[Int]) { 0.to[Int] } else { random[Int](16) + 5 }

    setArg(N, args(0).to[Int])
    setArg(offset, ofs)
    setArg(store, args(2).to[Int])
    setArg(rows, args(3).to[Int])
    setArg(cols, args(4).to[Int])

    Accel {
      val n: Int = N.value
      val tx: Int = store.value
      val ofs: Int = offset.value
      val numCmds: Int = rows.value
      val cmdWidth: Int = cols.value
      val srams = List.fill(N_MAX){ SRAM[T](4,96) }  // Won't actually get all the writes but that's ok

      Foreach(100000 by 1){ i =>
        Parallel {
          srams.zip(drams).zipWithIndex.foreach { case ((sram, dram), j) =>
            Pipe {
              if (j.to[Int] < n) {
                if (tx == 0.to[Int] || (tx == 2.to[Int] && j % 2 == 0)) {
                  sram load dram(0 :: rows, ofs :: cmdWidth+ofs par p)
                }
                if (tx == 1.to[Int] || (tx == 2.to[Int] && j % 2 == 1)) {
                  dram(0 :: rows, ofs :: cmdWidth+ofs par p) store sram
                }
              }
            }
          }
        }
      }
    }
  }

  case class Program[T<:MetaAny[T]](p: Int, rank: Int)(implicit tp: Type[T], bt: Bits[T]) {
    def name: String = s"rank=${rank}_p=$p"
    def go(): () => Unit = () => {
      if (rank == 1) transfer1D[T](p)
      else           transfer2D[T](p)
    }
  }

  def createPrograms(): Seq[Program[_]] = {
    import spatial.dsl._

    val ranks = List(1, 2).reverse
    val pars  = List(1, 2, 4, 8, 16)

    ranks.flatMap { r =>
      pars.map { p =>
        Program[Int](p = p, rank = r)
      }
    }
  }

  override val stagingArgs = Array("--synth")

  def main(args: Array[String]): Unit = {
    print("Threads: ")
    val threads = scala.io.StdIn.readLine().toInt
    val programs = createPrograms()

    println(s"Number of programs: ${programs.length}")
    println(s"SPATIAL_HOME: $SPATIAL_HOME")

    val pool = Executors.newFixedThreadPool(threads)
    val workQueue = new LinkedBlockingQueue[String](programs.length)
    val workers = List.tabulate(threads){id => new Synthesis(id, workQueue) }
    workers.foreach{worker => pool.submit(worker) }

    programs.zipWithIndex.foreach{case (program,i) =>
      val name = program.name
      initConfig(stagingArgs)

      Config.name = name
      Config.genDir = s"${Config.cwd}/gen/$name"
      Config.logDir = s"${Config.cwd}/logs/$name"
      resetState()

      //println(s"Compiling #$i: " + name + "...")
      try {
        compileProgram { program.go() }
        //synthesize(program.name)
        workQueue.put(name)
      }
      catch {case e: Throwable =>
        println(s"$name: FAIL (Compilation)")
        e.printStackTrace()
      }
    }

    (0 until threads).foreach{_ => workQueue.put("") }

    pool.shutdown()
    pool.awaitTermination(14L, TimeUnit.DAYS)
    println("COMPLETED")
  }
}