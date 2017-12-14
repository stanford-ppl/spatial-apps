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
object Benchmarks {
  // Times to wait for compilation and running, in seconds
  var MAKE_TIMEOUT = 11000 //1800
  var RUN_TIMEOUT = 1800
  var ZYNQ_TIMEOUT = 11000
  var AWS_TIMEOUT = 32400

  private final val NoArgs = Array[Any]()

  final val N = 1000000

  lazy val tests = {
    var tests = List[(SpatialApp, Array[Any])]()
    tests ::= (SimplePrimitiveTestReuseNone, Array(N, 32, 16))
    tests ::= (SimplePrimitiveTestReuseSome, Array(N, 32, 16))
    tests ::= (SimplePrimitiveTestReuseAll,  Array(N, 32, 16))

    tests ::= (ComplexPrimitiveTestReuseNone, Array(N, 1, 32, 16, 15, 4, 1))
    tests ::= (ComplexPrimitiveTestReuseSome, Array(N, 1, 32, 16, 15, 4, 1))
    tests ::= (ComplexPrimitiveTestReuseAll, Array(N, 1, 32, 16, 15, 4, 1))

    tests ::= (SimpleLoopTestReuseNone, Array(N, 1000))
    tests ::= (SimpleLoopTestReuseSome, Array(N, 1000))
    tests ::= (SimpleLoopTestReuseAll, Array(N, 1000))

    tests ::= (Nested3TestReuseNone, Array(N, 1, 32, 16, 15, 4, 1))
    tests ::= (Nested3TestReuseSome, Array(N, 1, 32, 16, 15, 4, 1))
    tests ::= (Nested3TestReuseAll,  Array(N, 1, 32, 16, 15, 4, 1))

    Array("Tests" -> tests)
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
        app.IR.config.verbosity = -2  // Won't see any of this output anyway
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
        else if (line.contains("errors")) cause = "Chisel build errors"
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
    /*backends ::= Backend(
      name = "Chisel",
      stagingArgs = flags :+ "--synth",
      make = genDir => Process(Seq("make","vcs"), new java.io.File(genDir)),
      run  = (genDir,args) => Process(Seq("bash", "run.sh", args), new java.io.File(genDir))
    )*/
    backends ::= Backend(
      name = "Zynq",
      stagingArgs = flags :+ "--synth" :+ "--retime",
      make = genDir => Process(Seq("make","zynq"), new java.io.File(genDir)),
      run  = (genDir,args) => Process(Seq("bash", "scrape.sh", "Zynq", args), new java.io.File(genDir))
    )

    backends ::= Backend(
      name = "Baseline",
      stagingArgs = flags :+ "--synth" :+ "--retime" :+ "--inline",
      make = genDir => Process(Seq("make","zynq"), new java.io.File(genDir)),
      run  = (genDir,args) => Process(Seq("bash", "scrape.sh", "Zynq", args), new java.io.File(genDir))
    )
    /*backends ::= Backend(
      name = "AWS",
      stagingArgs = flags :+ "--synth" :+ "--retime",
      make = genDir => Process(Seq("make","aws-F1-afi"), new java.io.File(genDir)),
      run  = (genDir,args) => Process(Seq("bash", "scrape.sh", "AWS"), new java.io.File(genDir))
    )*/
    /*backends ::= Backend(
      name = "Stats",
      stagingArgs = flags :+ "--synth" :+ "--retime",
      make = genDir => Process(Seq("make","null"), new java.io.File(genDir)),
      run  = (genDir,args) => Process(Seq("bash", "stats.sh"), new java.io.File(genDir))
    )*/

    var testBackends = backends.filter{b => args.contains(b.name) }
    if (args.contains("Zynq")) MAKE_TIMEOUT = ZYNQ_TIMEOUT
    else if (args.contains("AWS")) MAKE_TIMEOUT = AWS_TIMEOUT
    if (testBackends.isEmpty) testBackends = backends
    var testDomains = tests.filter{t => args.contains(t._1) }
    if (testDomains.isEmpty) testDomains = tests

    val nPrograms = testDomains.map(_._2.length).sum

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

