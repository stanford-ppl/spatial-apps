import spatial.dsl._
import virtualized._


// This File includes implementation for the following attentions
// Luong Attention
// Bahdanau Attention
// Scaled DotProduct Attention
// Multi-Headed Attention


object TestSqrtApprox extends SpatialApp {
  type T = FixPt[TRUE, _16, _16]
  type F = Float 

  val len = 32
  val high = 256
  val low = 100

  @virtualize
  def main() {
    val fixPtArray = loadCSV1D[T]("/home/tianzhao/data/softmax/norm_in.csv", ",")
    val floatPtArray = loadCSV1D[F]("/home/tianzhao/data/softmax/norm_in.csv", ",")

    val fixPtMem = DRAM[T](len)
    val floatPtMem = DRAM[F](len)

    val normErrorAccumulated = ArgOut[F]

    setMem(fixPtMem, fixPtArray)
    setMem(floatPtMem, floatPtArray)

    Accel {
      val fixReTile = SRAM[T](len)
      val floatReTile = SRAM[F](len)
      fixReTile load fixPtMem(0::len)
      floatReTile load floatPtMem(0::len)

      val fixPtNorm = Sequential.Reduce(Reg[T](0))(len by 1){ i =>
        fixReTile(i) * fixReTile(i)
      }{_+_}

      val floatPtNorm = Sequential.Reduce(Reg[F](0))(len by 1){ i =>
        floatReTile(i) * floatReTile(i)
      }{_+_}


      print("fixPtNormValue = " + fixPtNorm.value)
      print("floatPtNormValue = " + floatPtNorm.value)

      val fixPtNormSqrt = sqrt_approx(fixPtNorm.value)
      val floatPtNormSqrt = sqrt(floatPtNorm.value)

      println("fixPtNormSqrt = " + fixPtNormSqrt)
      println("floatPtNormSqrt = " + floatPtNormSqrt)

      normErrorAccumulated := abs(fixPtNormSqrt.to[F] - floatPtNormSqrt)
    }

    val error = getArg(normErrorAccumulated)
    println("error = " + error)
  }
}

object TestExp extends SpatialApp {
  type ST = FixPt[TRUE, _16, _16]
  type GT = Float

  val exp_order = 16

  @virtualize
  def exp_high_order(x: ST) = {
    // TODO: this guy seems fuzzy. Need to figure out a better implementation when x is large.
    // Maybe a mux based on range of x
    // lim_{n -> \inf}(1 + x/n)^n. Let n = 4. This way we form a reduction tree.
    println("x = " + x)
    println("x >> 4 = " + x)
    val tmp_re = 1.to[ST] + x / 16.to[ST]
    println("tmp_re = " + tmp_re)
    val re = Reduce(Reg[ST])(exp_order by 1) { _ =>
      tmp_re
    } {_*_}

    println("re.value = " + re.value)
    re.value
  }

  @virtualize
  def exp_gold(x: GT) = {
    exp(x)
  }

  @virtualize
  def main() {
    val fix_in = args(0).to[ST]
    val float_in = args(0).to[GT]

    val fix_in_arg = ArgIn[ST]
    val float_in_arg = ArgIn[GT]

    setArg(fix_in_arg, fix_in)
    setArg(float_in_arg, float_in)

    val diff_arg = ArgOut[GT]
    val fix_re_arg = ArgOut[GT]
    val float_re_arg = ArgOut[GT]
    Accel {
      val fix = exp_high_order(fix_in_arg.value)
      val float = exp_gold(float_in_arg.value)
      val diff = abs(fix.to[GT] - float)
      diff_arg := diff
      fix_re_arg := fix.to[GT]
      float_re_arg := float
    }

    val error = getArg(diff_arg)
    val fix_re = getArg(fix_re_arg)
    val float_re = getArg(float_re_arg)
    println("error: "+error)
    println("fix re: "+fix_re)
    println("float re: "+float_re)
  }
}


trait SoftMaxLib extends SpatialApp {
  val fIn = "/home/tianzhao/data/softmax/softmax_in.csv"
  val fGold = "/home/tianzhao/data/softmax/softmax_gold.csv"
  val fStore = "/home/tianzhao/data/softmax/softmax_spatial.csv"

  type T = Float
  val batchSize = 128
  val featureSize = 128
  val nPE = 4
  val nFeatureSum = 2

  @virtualize
  def exp_sm[T:Type:Num](x: T) = {
    // TODO: what would be a good way to implement EXP? Piece-wise?
    exp(x)
  }


  /**
  * Implements y = softmax(x, dim=1)
  * x,y should be a DRAM of shape (batchSize, featureSize)
  * cur error: ~ 1e-05
  */
  @virtualize
  def softmax[T:Type:Num](x: DRAM2[T], y: DRAM2[T]) {
    Foreach(batchSize by 1 par nPE){ idxPE =>
      val sramFeature = SRAM[T](featureSize)
      val sramNormalized = SRAM[T](featureSize)

      sramFeature load x(idxPE, 0::featureSize)
      val featureSum = Reduce(Reg[T])(featureSize by 1 par nFeatureSum){ idxFeature => exp_sm(sramFeature(idxFeature)) }{_+_}
      val featureSumVal = featureSum.value
      Foreach(featureSize by 1 par nFeatureSum){ idxFeature =>
        sramNormalized(idxFeature) = exp_sm(sramFeature(idxFeature)) / featureSumVal
      }

      y(idxPE, 0::featureSize) store sramNormalized
    }
  }
}


/**
* Tests the correctness of SoftMax in Spatial.
*/
object TestSoftMax extends SpatialApp with SoftMaxLib { // PASS

  @virtualize
  def main() {
    val smIn = loadCSV2D[T](fIn, ",", "\n")
    val x = DRAM[T](batchSize, featureSize)
    val y = DRAM[T](batchSize, featureSize)

    setMem(x, smIn)
    Accel {
      softmax(x, y)
    }

    val yRe = getMatrix(y)
    writeCSV2D(yRe, fStore)
  }
}



