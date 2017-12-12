import spatial.dsl._
import org.virtualized._
import spatial.stdlib._
import spatial.targets._


object SimplePrimitiveTestReuseSome extends SpatialApp {
  
  @module
  def primitive_function(arg1: Int, arg2: Int): Int = {
    arg1 * arg2
  }
  
  
  @virtualize
  def main() {
    // Declare SW-HW interface vals
    val x = ArgIn[Int]
    val y = ArgIn[Int]
    val w = ArgOut[Int]
    val z = ArgOut[Int]

    val N0 = args(0).to[Int]
    val N1 = args(1).to[Int]

    // Connect SW vals to HW vals
    setArg(x, N0)
    setArg(y, N1)

    // Create HW accelerator
    Accel {

      val max = 100000
      val out1 = Reg[Int](0)
      val out2 = Reg[Int](0)

      Foreach(0 until max){ i=>

        Sequential {

          Pipe { out1 := primitive_function(x, y) }
          
          Parallel {
            Pipe { out1 := primitive_function(x, y) }
            Pipe { out2 := primitive_function(x, y) }
          }

          Pipe { out2 := primitive_function(x, y) }
        }

      }

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


    println("PASS: " + cksum + " (SimplePrimitiveTestReuseSome)")
  }
}

