import spatial.dsl._
import org.virtualized._


object NMT extends SpatialApp with Activations {
  type T = FixPt[TRUE, _8, _8]

  @virtualize
  def main() {
    val x = ArgIn[T]
    val y = ArgOut[T]
    val N = args(0).to[T]

    setArg(x, N)
    Accel {
      y := tanh_(x.value)
    }

    val yre = getArg(y)
    println(yre)
  }
}