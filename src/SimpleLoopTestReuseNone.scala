import spatial.dsl._
import org.virtualized._
import spatial.stdlib._
import spatial.targets._


object SimpleLoopTestReuseNone extends SpatialApp {
  
  //@module
  def loop_function(loop_max: Int): Int = {
    Reduce(Reg[Int](0))(0 until loop_max by 1){ i =>
      2*i
    } {_+_}
  }
  
  
  @virtualize
  def main() {
    // Declare SW-HW interface vals
    val x = ArgIn[Int]
    val y = ArgOut[Int]
    val z = ArgOut[Int]

    val N0 = args(0).to[Int]
    val N1 = args(1).to[Int]

    // Connect SW vals to HW vals
    setArg(x, N0)

    // Create HW accelerator
    Accel {

      val max = 100000
      val out1 = Reg[Int](0)
      val out2 = Reg[Int](0)

      Foreach(0 until max){ i=>
        Parallel {
          Pipe { out1 := loop_function(x) }
          Pipe { out2 := loop_function(x) }
        }
      }

      y := out1
      z := out2

    }


    // Extract results from accelerator
    val result1 = getArg(y)
    val result2 = getArg(z)

    // Create validation checks and debug code
    var gold = 0
    for (i <- 0 until N0){
      gold = gold + 2*i
    }
    println("expected: " + gold)
    println("result1: " + result1)
    println("result2: " + result2)

    val cksum1 = gold == result1
    val cksum2 = result1 == result2
    val cksum = cksum1 && cksum2


    println("PASS: " + cksum + " (SimpleLoopTestReuseNone)")
  }
}

