
import spatial.dsl._
import org.virtualized._

object BankingTests extends SpatialApp {
  val R = 32; val C = 16
  val P = 1;  val Q = 4

  @virtualize
  def main() {
    val dram = DRAM[Int](R,C)

    Accel {
      val x = SRAM[Int](R,C)

      Foreach(0 until R, 0 until C par Q){(i,j) =>
        x(i,j) = i + j
      }
      dram store x
    }

    val data = getMatrix(dram)
    printMatrix(data, "data")
  }
}

object SliceTest extends SpatialApp {
  val R = 12; val C = 12
  val P = 1;  val Q = 4

  @virtualize
  def main() {
    val dram = DRAM[Int](R,C)
    val out = DRAM[Int](R,C)

    val input = (0::R,0::C){(i,j) => i*C + j }
    setMem(dram,input)

    Accel {
      val lb = LineBuffer[Int](3,C)
      val sr = RegFile[Int](3,6)
      val results = SRAM[Int](R,C)

      Foreach(0 until R){i =>
        // Load a line
        lb load dram(i,0::C)

        println("Line Buffer contents: ")
        Foreach(0 until 3){x =>
          Foreach(0 until C){y =>
            print(lb(x, y) + " ")
          }
          println("")
        }

        Foreach(0 until C by 3){j =>
          // Load in a 3x3 window
          Foreach(0 until 3 par 3){i => sr(i, *) <<= lb(i, j::j+3) }

          println("Shift Register contents: ")
          Foreach(0 until 3){ i =>
            Foreach(0 until 6){ j =>
              print(sr(i, j) + " ")
            }
            println("")
          }

          // The result element is the sum of the 3x6 window
          val res = Reduce(0)(0 until 3, 0 until 6){(k,l) => sr(k,l) }{_+_}
          results(i,j) = res
        }
      }
      out store results
    }

    val data = getMatrix(dram)
    printMatrix(data, "data")
    val output = getMatrix(out)
    printMatrix(output, "output")

    val expect = (0::R,0::C){(i,j) =>
      if (j % 3 == 0 && i > 2 && j < C-3)
        Array.tabulate(3){ii =>
          Array.tabulate(6){jj => input(i-ii,j+jj-3) }.reduce{_+_}
        }.reduce{_+_}
      else
        0.to[Int]
    }
    printMatrix(expect, "expect")
  }
}