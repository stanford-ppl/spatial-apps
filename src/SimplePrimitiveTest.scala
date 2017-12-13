import spatial.dsl._
import org.virtualized._

trait SimplePrimitiveTest extends SpatialApp {

  def body(i: Int, x: Int, y: Int, out1: Reg[Int], out2: Reg[Int]): Unit

  @module
  def primitive_function(arg1: Int, arg2: Int): Int = {
    arg1 * arg2
  }
  
  
  @virtualize
  def main() {
    // Declare SW-HW interface vals
    val n = ArgIn[Int]
    val x = ArgIn[Int]
    val y = ArgIn[Int]
    val w = ArgOut[Int]
    val z = ArgOut[Int]

    val MAX = args(0).to[Int]
    val N0 = args(1).to[Int]
    val N1 = args(2).to[Int]

    // Connect SW vals to HW vals
    setArg(x, N0)
    setArg(y, N1)
    setArg(n, MAX)

    // Create HW accelerator
    Accel {
      val max = n.value
      val out1 = Reg[Int](0)
      val out2 = Reg[Int](0)

      Foreach(0 until max){ i => body(i,x,y,out1,out2) }

      w := out1
      z := out2
    }


    // Extract results from accelerator
    val result1 = getArg(w)
    val result2 = getArg(z)

    // Create validation checks and debug code
    val gold = N0 * N1
    println("expected: " + gold)
    println("result1: " + result1)
    println("result2: " + result2)

    val cksum1 = gold == result1
    val cksum2 = result1 == result2
    val cksum = cksum1 && cksum2


    println("PASS: " + cksum + " (SimplePrimitiveTestReuseAll)")
  }
}

object SimplePrimitiveTestReuseAll extends SimplePrimitiveTest {
  def body(i: Int, x: Int, y: Int, out1: Reg[Int], out2: Reg[Int]): Unit = {
    Sequential {
      Pipe { out1 := primitive_function(x, y) }
      Pipe { out2 := primitive_function(x, y) }
    }
  }
}

object SimplePrimitiveTestReuseSome extends SimplePrimitiveTest {
  def body(i: Int, x: Int, y: Int, out1: Reg[Int], out2: Reg[Int]): Unit = {
    Sequential {
      Pipe { out1 := primitive_function(x, y) }

      Parallel {
        Pipe { out1 := primitive_function(x, y) }
        Pipe { out2 := primitive_function(x, y) }
      }

      Pipe { out2 := primitive_function(x, y) }
    }
  }
}

object SimplePrimitiveTestReuseNone extends SimplePrimitiveTest {
  def body(i: Int, x: Int, y: Int, out1: Reg[Int], out2: Reg[Int]): Unit = {
    Parallel {
      Pipe { out1 := primitive_function(x, y) }
      Pipe { out2 := primitive_function(x, y) }
    }
  }
}

