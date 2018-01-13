import java.io.PrintStream
import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.concurrent._
import scala.concurrent._
import ExecutionContext.Implicits.global   // implicit execution context for Futures

import sys.process._
import spatial.SpatialApp

import scala.concurrent.{Await, Future, TimeoutException}

// Usage: <threads> <branch> <type [Scala, Chisel]>
object Regression {
  // Times to wait for compilation and running, in seconds
  var MAKE_TIMEOUT = 2000
  var RUN_TIMEOUT = 2000
  var ZYNQ_TIMEOUT = 11000
  var AWS_TIMEOUT = 32400

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
    // dense ::= (Backprop, Array(5))
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
    // sparse ::= (SPMV_DumbPack, Array(1536))
    sparse ::= (PageRank, Array(50, 0.125))
    sparse ::= (BFS_Queue, NoArgs)
    sparse ::= (BFS_Bulk, NoArgs)
    sparse ::= (SPMV_ELL, NoArgs)
    sparse ::= (SPMV_CRS, NoArgs)

    var unit = List[(SpatialApp, Array[Any])]()
    unit ::= (Breakpoint, NoArgs)
    unit ::= (ArbitraryLambda, Array(8))
    unit ::= (BubbledWriteTest, NoArgs)
    unit ::= (MultiplexedWriteTest, NoArgs)
    unit ::= (MixedIOTest, NoArgs)
    unit ::= (LUTTest, Array(2, 3))
    unit ::= (RetimedFifoBranch, Array(13,25))
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
    unit ::= (StridedLoad, NoArgs)
    unit ::= (InOutArg, Array(32))
    unit ::= (Tensor5D, Array(32, 4, 4, 4, 4))
    unit ::= (Tensor4D, Array(32, 4, 4, 4))
    unit ::= (IndirectLoad, NoArgs)
    unit ::= (SequentialWrites, Array(7))

    var fixme = List[(SpatialApp, Array[Any])]()
    // fixme ::= (KMP, Array("the"))
    fixme ::= (SPMV_DumbPack, Array(1536))
    fixme ::= (Backprop, Array(5))




    Array("Dense" -> dense, "Sparse" -> sparse, "Unit" -> unit, "Fixme" -> fixme)
  }

  case class Backend(name: String, stagingArgs: Array[String], make: String => ProcessBuilder, run: (String,String) => ProcessBuilder) {
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

    def logExcept(test: Test, t: Throwable): Unit = {
      import argon.core._
      implicit val IR: State = test.app.IR
      config.verbosity = 10
      withLog(config.logDir, config.name + "_exception.log") {
        if (t.getMessage != null) { dbg(t.getMessage); dbg("") }
        if (t.getCause != null) { dbg(t.getCause); dbg("") }
        t.getStackTrace.foreach{elem => dbg(elem.toString) }
      }
    }

    def compileTest(test: Test): Boolean = {
      val Test(backend, cat, app, _, _) = test
      val name = app.name
      // val env = sys.env.get("PATH")
      // results.put(s"$env")

      app.__stagingArgs = backend.stagingArgs   // just in case the app refers to this
      try {
        app.init(backend.stagingArgs)
        app.IR.config.verbosity = -2      // Won't see any of this output anyway
        app.IR.config.exitOnBug = false   // Never exit, even on errors
        app.IR.config.genDir = s"${app.IR.config.cwd}/gen/$backend/$cat/$name/"
        app.IR.config.logDir = s"${app.IR.config.cwd}/logs/$backend/$cat/$name/"
        val consoleLog = argon.core.createLog(s"${app.IR.config.logDir}", "console.log")
        val f = Future(blocking{
          argon.core.withLog(consoleLog){
            app.compileProgram{ () => app.main() }
          }(app.IR)
        })
        Await.result(f, duration.Duration(MAKE_TIMEOUT, "sec"))
        if (app.IR.hadErrors) {
          results.put(s"$backend.$cat.$name: Fail [Spatial Compile][newline]&nbsp;&nbsp;Cause: Error(s) during Spatial compilation")
        }
        !app.IR.hadErrors
      }
      catch {
        case e: TimeoutException =>
          results.put(s"$backend.$cat.$name: Fail [Spatial Compile][newline]&nbsp;&nbsp;ause: Spatial compile timed out after $MAKE_TIMEOUT seconds")
          false
        case e: Throwable =>
          results.put(s"$backend.$cat.$name: Fail [Spatial Compile][newline]&nbsp;&nbsp;Cause: $e[newline]&nbsp;&nbsp;&nbsp;&nbsp;${e.getMessage}")
          logExcept(test, e)
          false
      }
    }

    def makeTest(test: Test): Boolean = {
      val Test(backend, cat, app, targs, _) = test
      val name = app.name
      val makeLog = new PrintStream(app.IR.config.logDir + "/" + "make.log")
      val cmd = backend.make(app.IR.config.genDir)
      var cause = ""
      def log(line: String): Unit = {
        // Couple of really dumb heuristics for finding reported errors at runtime
        if (line.contains("Placer could not place all instances") && cause == "") cause = line
        else if ("ERROR.*Value '[0-9]+' is out of the range".r.findFirstIn(line).isDefined && cause == "") cause = line
        makeLog.println(line)
      }
      val logger = ProcessLogger(log,log)
      val p = cmd.run(logger)

      try {
        val f = Future(blocking(p.exitValue()))
        val code = Await.result(f, duration.Duration(MAKE_TIMEOUT, "sec"))

        if (code != 0)   {
          val expl = if (cause == "") s"Non-zero exit code[newline]&nbsp;&nbsp;&nbsp;&nbsp;See ${app.IR.config.logDir}make.log" else cause
          results.put(s"$backend.$cat.$name: Fail [Execution][newline]&nbsp;&nbsp;Cause: $expl")
        }

        code == 0
      }
      catch {
        case e: TimeoutException =>
          results.put(s"$backend.$cat.$name: Fail [Backend Compile][newline]&nbsp;&nbsp;Cause: Backend compile timed out after $MAKE_TIMEOUT seconds")
          p.destroy()
          //p.exitValue()
          false
        case e: Throwable =>
          results.put(s"$backend.$cat.$name: Fail [Backend Compile][newline]&nbsp;&nbsp;Cause: $e[newline]&nbsp;&nbsp;&nbsp;&nbsp;${e.getMessage}")
          logExcept(test, e)
          false
      }
      finally {
        makeLog.close()
      }
    }

    def runTest(test: Test): Boolean = {
      val Test(backend, cat, app, targs, _) = test
      var passed = false
      var failed = false
      var prev = ""
      var cause = ""
      val name = app.name
      val runLog = new PrintStream(app.IR.config.logDir + "/" + "run.log")
      val args = targs.map(_.toString).mkString(" ")
      val cmd = backend.run(app.IR.config.genDir, args)
      def log(line: String): Unit = {
        // Couple of really dumb heuristics for finding reported errors at runtime
        if (line.trim.startsWith("at") && cause == "") cause = prev     // Exceptions in Scala
        if (line.trim.endsWith("failed.") && cause == "") cause = line // Assertion failures in VCS
        passed = passed || line.contains("PASS: 1") || line.contains("PASS: true")
        failed = failed || line.contains("PASS: 0") || line.contains("PASS: false")
        runLog.println(line)
        prev = line
      }

      val logger = ProcessLogger(log,log)
      val p = cmd.run(logger)

      try {
        val f = Future(blocking(p.exitValue()))
        val code = Await.result(f, duration.Duration(RUN_TIMEOUT, "sec"))

        if (code != 0)   {
          val expl = if (cause == "") s"Non-zero exit code[newline]&nbsp;&nbsp;&nbsp;&nbsp;See ${app.IR.config.logDir}run.log" else cause
          results.put(s"$backend.$cat.$name: Fail [Execution][newline]&nbsp;&nbsp;Cause: $expl")
        }
        else if (cause != "") results.put(s"$backend.$cat.$name: Fail [Execution][newline]&nbsp;&nbsp;Cause: $cause")
        else if (failed) results.put(s"$backend.$cat.$name: Fail [Validation][newline]&nbsp;&nbsp;Cause: Application reported that it did not pass validation.")
        else if (passed) results.put(s"$backend.$cat.$name: Pass")
        else             results.put(s"$backend.$cat.$name: Indeterminate[newline]&nbsp;&nbsp;Cause: Application did not report validation result.")
        code == 0 && cause == ""
      }
      catch {
        case e: TimeoutException =>
          results.put(s"$backend.$cat.$name: Fail [Execution][newline]&nbsp;&nbsp;Cause: Execution timed out after $RUN_TIMEOUT seconds")
          p.destroy()
          //p.exitValue() // Block waiting for kill
          false

        case e: Throwable =>
          results.put(s"$backend.$cat.$name: Fail [Execution][newline]&nbsp;&nbsp;Cause: $e[newline]&nbsp;&nbsp;&nbsp;&nbsp;${e.getMessage}")
          logExcept(test, e)
          false
      }
      finally {
        runLog.close()
      }
    }

    def run(): Unit = {
      println(s"[T$id] Started")

      while(isAlive) {
        val test = queue.take()
        if (test.isValid) {
          try {
            val compiled = compileTest(test)
            val made = compiled && makeTest(test)
            val success = made && runTest(test)
            val msg = if (success) "PASS" else "FAIL"
            println(s"[T$id] ${test.backend}.${test.category}.${test.app.name}: $msg")
          }
          catch{ case t: Throwable =>
            println(s"[T$id] ${test.backend}.${test.category}.${test.app.name}: UNCAUGHT EXCEPTION[newline]&nbsp;&nbsp;Cause: $t[newline]&nbsp;&nbsp; ${t.getMessage}")
          }
        }
        else {
          isAlive = false
          println(s"[T$id] Received kill signal")
        }
      }

      println(s"[T$id] Ended")
    }
  }

  case class Printer(log: PrintStream, queue: BlockingQueue[String]) extends Runnable {
    var isAlive = true
    def run(): Unit = {
      while (isAlive) {
        val result = queue.take()
        if (result != "") {
          log.println(result)
        }
        else isAlive = false
      }
    }
  }


  def main(args: Array[String]): Unit = {
    val now = Calendar.getInstance().getTime
    val fmt = new SimpleDateFormat("dd_MM_yyyy_hh_mm_aa")
    val timestamp = fmt.format(now)
    var backends = List[Backend]()

    val threads: Int = try { args(0).toInt } catch { case _:Throwable => 8 }
    val branch: String = try { args(1) } catch { case _:Throwable => "nobranch" }
    var flags = Array[String]()
    if (branch.contains("retim")) flags = flags :+ "--retiming"
    if (branch.contains("syncMem")) flags = flags :+ "--syncMem"
    if (branch.contains("syncMem")) flags = flags :+ "--multifile=4" else flags = flags :+ "--multifile=5"
    if (branch.contains("pre-master")) flags = flags :+ "--instrument"

    backends ::= Backend(
      name = "Scala",
      stagingArgs = Array("--sim"),
      make = genDir => Process(Seq("make"), new java.io.File(genDir)),
      run = (genDir, args) => Process(Seq("bash","run.sh", args), new java.io.File(genDir))
    )
    backends ::= Backend(
      name = "Chisel",
      stagingArgs = flags :+ "--synth",
      make = genDir => Process(Seq("make","vcs"), new java.io.File(genDir)),
      run  = (genDir,args) => Process(Seq("bash", "scripts/regression_run.sh", branch, args), new java.io.File(genDir))
    )
    backends ::= Backend(
      name = "Zynq",
      stagingArgs = flags :+ "--synth" :+ "--retime",
      make = genDir => Process(Seq("make","zynq"), new java.io.File(genDir)),
      run  = (genDir,args) => Process(Seq("bash", "scripts/scrape.sh", "Zynq", args), new java.io.File(genDir))
    )
    backends ::= Backend(
      name = "AWS",
      stagingArgs = flags :+ "--synth" :+ "--retime",
      make = genDir => Process(Seq("make","aws-F1-afi"), new java.io.File(genDir)),
      run  = (genDir,args) => Process(Seq("bash", "scripts/scrape.sh", "AWS"), new java.io.File(genDir))
    )
    backends ::= Backend(
      name = "Stats",
      stagingArgs = flags :+ "--synth" :+ "--retime",
      make = genDir => Process(Seq("make","null"), new java.io.File(genDir)),
      run  = (genDir,args) => Process(Seq("bash", "scripts/stats.sh"), new java.io.File(genDir))
    )

    var testBackends = backends.filter{b => args.contains(b.name) }
    if (args.contains("Zynq")) MAKE_TIMEOUT = ZYNQ_TIMEOUT
    else if (args.contains("AWS")) MAKE_TIMEOUT = AWS_TIMEOUT
    if (testBackends.isEmpty) testBackends = backends
    var testDomains = tests.filter{t => args.contains(t._1) }
    if (testDomains.isEmpty) testDomains = tests

    val nPrograms = testDomains.map(_._2.length).sum
    println(s"$nPrograms total tests to run:")
    testDomains.foreach{case (cat, apps) =>
      apps.foreach{case (app,_) => println(s"  $cat.${app.name}") }
    }

    val regressionLog = new PrintStream(s"regression_$timestamp.log")
    val flagsLog = new PrintStream(s"flags_$timestamp.log")
    flagsLog.println(s"Flags: ${testBackends.map{x => s"${x.stagingArgs.toList.mkString(" ")}"}.mkString(", ")}")
    // TODO: Good way to get the make target? (vcs, xsim, etc)
    // flagsLog.println(s"Make: ${testBackends.map{x => s"${x.make}"}.mkString(", ")}")

    // TODO: This could use some optimization
    // Can potentially overlap next backend with current as long as the same app is never compiling
    // TODO: Actually, can we run two compilations of the same app at the same time?
    testBackends.zipWithIndex.foreach{case (backend,i) =>
      val pool = Executors.newFixedThreadPool(threads)
      val workQueue = new LinkedBlockingQueue[Test](threads)
      val resultQueue = new LinkedBlockingQueue[String](threads)
      (0 until threads).foreach{id => pool.submit(Worker(id, workQueue, resultQueue)) }

      val printerPool = Executors.newFixedThreadPool(1)
      printerPool.submit(Printer(regressionLog, resultQueue))

      var testApps = testDomains.map{case (cat,apps) =>
        cat -> apps.filter{case (app,targs) => args.contains(app.name) }
      }
      if (testApps.forall(_._2.isEmpty)) testApps = testDomains

      testApps.foreach{case (cat,apps) =>
        apps.foreach{case (app,targs) =>
          workQueue.put(Test(backend, cat, app, targs, true))
        }
      }

      (0 until threads).foreach{_ => workQueue.put(Test.empty) }
      pool.shutdown()
      pool.awaitTermination(MAKE_TIMEOUT + RUN_TIMEOUT, TimeUnit.SECONDS)

      resultQueue.put("")
      printerPool.shutdown()
      printerPool.awaitTermination(MAKE_TIMEOUT + RUN_TIMEOUT, TimeUnit.SECONDS)
    }

    flagsLog.close()
    regressionLog.close()
  }
}
