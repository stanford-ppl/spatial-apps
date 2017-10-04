import java.io.PrintStream
import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.concurrent.{BlockingQueue, Executors, LinkedBlockingQueue, TimeUnit}

import sys.process._
import spatial.SpatialApp

object Regression {
  private final val NoArgs = Array[Any]()

  lazy val tests = {
    var dense = List[(SpatialApp, Array[Any])]()
    dense ::= (DotProduct, Array(640))
    dense ::= (OuterProduct, Array(640, 640))
    dense ::= (SimpleRowStridedConv, NoArgs)
    dense ::= (Convolutions, Array(16))
    dense ::= (BasicBLAS, Array(0.2, 0.8, 64, 128, 96))
    dense ::= (TRSM, NoArgs)
    dense ::= (SYRK_col, Array(64))
    dense ::= (MatMult_inner, Array(32, 128, 128))
    dense ::= (MatMult_outer, Array(32, 128, 128))
    dense ::= (JPEG_Decompress, NoArgs)
    dense ::= (JPEG_Markers, NoArgs)
    dense ::= (SHA1, NoArgs)
    dense ::= (Sobel, Array(200, 160))
    dense ::= (SW, Array("tcgacgaaataggatgacagcacgttctcgtattagagggccgcggtacaaaccaaatgctgcggcgtacagggcacggggcgctgttcgggagatcgggggaatcgtggcgtgggtgattcgccggc ttcgagggcgcgtgtcgcggtccatcgacatgcccggtcggtgggacgtgggcgcctgatatagaggaatgcgattggaaggtcggacgggtcggcgagttgggcccggtgaatctgccatggtcgat"))
    dense ::= (BTC, Array("0100000081cd02ab7e569e8bcd9317e2fe99f2de44d49ab2b8851ba4a308000000000000e320b6c2fffc8d750423db8b1eb942ae710e951ed797f7affc8892b0f1fc122bc7f5d74df2b9441a42a14695"))
    dense ::= (TPCHQ6, Array(3840))
    dense ::= (SGD_minibatch, Array(40, 64, 0.0001))
    dense ::= (SGD, Array(40, 64, 0.0001))
    dense ::= (Gibbs_Ising2D, Array(25, 0.3, 2))
    dense ::= (GDA, Array(64))
    dense ::= (Differentiator, NoArgs)
    dense ::= (EdgeDetector, NoArgs)
    dense ::= (Convolution_FPGA, NoArgs)
    dense ::= (Kmeans, Array(3, 64))
    dense ::= (FFT_Transpose, NoArgs)
    dense ::= (FFT_Strided, NoArgs)
    dense ::= (Backprop, Array(15))
    dense ::= (Sort_Radix, NoArgs)
    dense ::= (GEMM_Blocked, Array(128))
    dense ::= (GEMM_NCubed, NoArgs)
    dense ::= (KMP, Array("the"))
    dense ::= (MD_Grid, NoArgs)
    dense ::= (MD_KNN, NoArgs)
    dense ::= (NW, Array("tcgacgaaataggatgacagcacgttctcgtattagagggccgcggtacaaaccaaatgctgcggcgtacagggcacggggcgctgttcgggagatcgggggaatcgtggcgtgggtgattcgccggc ttcgagggcgcgtgtcgcggtccatcgacatgcccggtcggtgggacgtgggcgcctgatatagaggaatgcgattggaaggtcggacgggtcggcgagttgggcccggtgaatctgccatggtcgat"))
    dense ::= (Stencil3D, NoArgs)
    dense ::= (Stencil2D, NoArgs)
    dense ::= (Viterbi, NoArgs)
    dense ::= (Sort_Merge, NoArgs)
    dense ::= (AES, Array(50))

    var sparse = List[(SpatialApp, Array[Any])]()
    sparse ::= (ScatterGather, Array(160))
    sparse ::= (GatherStore, NoArgs)
    sparse ::= (PageRank_Bulk, Array(50, 0.125))
    sparse ::= (SPMV_DumbPack, Array(1536))
    sparse ::= (PageRank, Array(50, 0.125))
    sparse ::= (BFS_Queue, NoArgs)
    sparse ::= (BFS_Bulk, NoArgs)
    sparse ::= (SPMV_ELL, NoArgs)
    sparse ::= (SPMV_CRS, NoArgs)

    var unit = List[(SpatialApp, Array[Any])]()
    unit ::= (ArbitraryLambda, Array(8))
    unit ::= (BubbledWriteTest, NoArgs)
    unit ::= (MultiplexedWriteTest, NoArgs)
    unit ::= (MixedIOTest, NoArgs)
    unit ::= (LUTTest, Array(2))
    unit ::= (SSV2D, NoArgs)
    unit ::= (SSV1D, NoArgs)
    unit ::= (MultiWriteBuffer, NoArgs)
    unit ::= (DiagBanking, NoArgs)
    unit ::= (SpecialMath, Array(0.125, 5.625, 14, 1.875, -3.4375, -5))
    unit ::= (FixPtMem, Array(5.25, 2.125))
    unit ::= (MaskedWrite, Array(5))
    unit ::= (FixPtInOutArg, Array(-1.5))
    unit ::= (LaneMaskPar, Array(13))
    unit ::= (FifoStackFSM, NoArgs)
    unit ::= (CtrlEnable, Array(9))
    unit ::= (DotProductFSM, NoArgs)
    unit ::= (BasicCondFSM, NoArgs)
    unit ::= (BasicFSM, NoArgs)
    unit ::= (FifoPushPop, Array(384))
    unit ::= (ChangingCtrMax, NoArgs)
    unit ::= (BlockReduce2D, Array(192, 384))
    unit ::= (Tensor3D, Array(32, 4, 4))
    unit ::= (UnalignedLd, Array(100, 9))
    unit ::= (BlockReduce1D, Array(1920))
    unit ::= (UniqueParallelLoad, NoArgs)
    unit ::= (Memcpy2D, NoArgs)
    unit ::= (SimpleFold, Array(1920))
    unit ::= (SimpleMemReduce, NoArgs)
    unit ::= (SimpleReduce, Array(7))
    unit ::= (StackLoadStore, NoArgs)
    unit ::= (FifoLoadStore, NoArgs)
    unit ::= (ParFifoLoad, Array(384))
    unit ::= (FloatBasics, Array(3.2752, -283.70))
    unit ::= (CompactingFifo, Array(640))
    unit ::= (UnalignedFifoLoad, Array(400))
    unit ::= (OHM, Array(400))
    unit ::= (SimpleTileLoadStore, Array(100))
    unit ::= (DeviceMemcpy, Array(50))
    unit ::= (SimpleSequential, Array(5, 8))
    unit ::= (FifoLoadSRAMStore, Array(192))
    unit ::= (MemTest2D, Array(7))
    unit ::= (MemTest1D, Array(7))
    unit ::= (Niter, Array(100))
    unit ::= (InOutArg, Array(32))
    unit ::= (Tensor5D, Array(32, 4, 4, 4, 4))
    unit ::= (Tensor4D, Array(32, 4, 4, 4))
    unit ::= (SequentialWrites, Array(7))

    Array("Dense" -> dense, "Sparse" -> sparse, "Unit" -> unit)
  }

  case class Backend(name: String, stagingArgs: Array[String], make: String => String, run: (String,Array[String]) => String) {
    override def toString: String = name
  }
  case class Test(backend: Backend, category: String, app: SpatialApp, runtimeArgs: Array[Any], compile: Boolean) {
    def isValid: Boolean = !((backend eq null) || category == "" || (app eq null) || (runtimeArgs eq null))
  }
  object Test {
    def empty: Test = Test(null, "", null, null, true)
  }


  case class Worker(id: Int, queue: BlockingQueue[Test], results: BlockingQueue[String]) extends Runnable {
    var isAlive = true

    def compileTest(test: Test): Unit = {
      val Test(backend, cat, app, targs, _) = test
      val name = app.name
      app.__stagingArgs = backend.stagingArgs   // just in case the app refers to this
      try {
        app.init(backend.stagingArgs)
        app.IR.config.verbosity = 0
        app.IR.config.genDir = s"${app.IR.config.cwd}/gen/$backend/$cat/$name/"
        app.IR.config.logDir = s"${app.IR.config.cwd}/logs/$backend/$cat/$name/"
        val consoleLog = argon.core.createLog(s"${app.IR.config.logDir}", "console.log")
        argon.core.withLog(consoleLog){
          app.compileProgram{ () => app.main() }
        }(app.IR)
      }
      catch {case e: Throwable =>
        results.put(s"$backend.$cat.$name: Fail [Spatial Compile]\n  Cause: $e\n    ${e.getMessage}")
        throw e
      }
    }

    def makeTest(test: Test): Unit = {
      val Test(backend, cat, app, targs, _) = test
      val name = app.name
      try {
        //val makeLog = s"${app.IR.config.cwd}/logs/$backend/$cat/$name/make.log"
        //s"rm -f $makeLog".!
        //s"touch $makeLog".!
        backend.make(app.IR.config.genDir)
      }
      catch {case e: Throwable =>
        results.put(s"$backend.$cat.$name: Fail [Backend Compile]\n  Cause: $e\n    ${e.getMessage}")
        throw e
      }
    }

    def runTest(test: Test): String = {
      val Test(backend, cat, app, targs, _) = test
      val name = app.name
      try {
        //val runLog = s"${app.IR.config.cwd}/logs/$backend/$cat/$name/run.log"
        //s"rm -f $runLog".!
        //s"touch $runLog".!
        backend.run(app.IR.config.genDir, targs.map(_.toString))
      }
      catch {case e: Throwable =>
        results.put(s"$backend.$cat.$name: Fail [Execution]\n  Cause: $e\n    ${e.getMessage}")
        throw e
      }
    }

    def validateTest(result: String, test: Test): Unit = {
      val Test(backend, cat, app, targs, _) = test
      val name = app.name
      //val runLog = s"${app.IR.config.cwd}/logs/$backend/$cat/$name/run.log"
      //val src = Source.fromFile(runLog)
      val lines = result.split("\n")

      var passed = false
      var failed = false
      lines.foreach{line =>
        passed = passed || line.contains("PASS: 1") || line.contains("PASS: true")
        failed = failed || line.contains("PASS: 0") || line.contains("PASS: false")
      }

      if (failed)      results.put(s"$backend.$cat.$name: Fail [Validation]")
      else if (passed) results.put(s"$backend.$cat.$name: Pass")
      else             results.put(s"$backend.$cat.$name: Indeterminate")

      //src.close()
    }


    def run(): Unit = {
      println(s"[T$id] Started")

      while(isAlive) {
        val test = queue.take()
        if (test.isValid) {
          try {
            compileTest(test)
            makeTest(test)
            val result = runTest(test)
            validateTest(result, test)
          }
          catch{ case t: Throwable =>
            import argon.core._
            implicit val IR: State = test.app.IR
            config.verbosity = 10
            withLog(config.logDir, config.name + "_exception.log") {
              if (t.getMessage != null) { dbg(t.getMessage); dbg("") }
              if (t.getCause != null) { dbg(t.getCause); dbg("") }
              t.getStackTrace.foreach{elem => dbg(elem.toString) }
            }
          }
        }
        else {
          isAlive = false
          println(s"[T#$id] Received kill signal")
        }
      }

      println(s"[T$id] Ended")
    }
  }

  private var backends = List[Backend]()
  backends ::= Backend(
    name = "Scala",
    stagingArgs = Array("--sim"),
    make = genDir => Process(Seq("make"), new java.io.File(genDir)).!!,
    run = (genDir, args) => Process(Seq("bash","run.sh") ++ args.toSeq, new java.io.File(genDir)).!!
  )
  backends ::= Backend(
    name = "Chisel",
    stagingArgs = Array("--synth"),
    make = genDir => Process(Seq("make","vcs"), new java.io.File(genDir)).!!,
    run  = (genDir,args) => Process(Seq("bash", "run.sh") ++ args.toSeq, new java.io.File(genDir)).!!
  )

  def main(args: Array[String]): Unit = {
    val now = Calendar.getInstance().getTime
    val fmt = new SimpleDateFormat("dd_MM_yyyy_hh_mm_aa")
    val timestamp = fmt.format(now)

    val threads: Int = try { args(0).toInt } catch { case _:Throwable => 8 }
    val nPrograms = tests.map(_._2.length).sum

    val testTests = Array("Dense" -> List(tests(0)._2.head))

    val regressionLog = new PrintStream(s"regression_$timestamp.log")

    // TODO: This could use some optimization
    // Can potentially overlap next backend with current as long as the same app is never compiling
    // Could also print out results to file on the fly
    backends.zipWithIndex.foreach{case (backend,i) =>
      val pool = Executors.newFixedThreadPool(threads)
      val workQueue = new LinkedBlockingQueue[Test](nPrograms)
      val resultQueue = new LinkedBlockingQueue[String](nPrograms)
      (0 until threads).foreach{id => pool.submit(Worker(id, workQueue, resultQueue)) }

      //testTests.foreach{case (cat,apps) =>
      tests.foreach{case (cat,apps) =>
        apps.foreach{case (app,targs) =>
          workQueue.put(Test(backend, cat, app, targs, true))
        }
      }

      (0 until threads).foreach{_ => workQueue.put(Test.empty) }
      pool.shutdown()
      pool.awaitTermination(14L, TimeUnit.DAYS)

      val iter = resultQueue.iterator()
      while (iter.hasNext) {
        val item = iter.next()
        regressionLog.println(item)
      }
    }

    regressionLog.close()
  }
}
