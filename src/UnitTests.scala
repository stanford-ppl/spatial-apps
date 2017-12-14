import spatial.dsl._
import org.virtualized._
import spatial.stdlib._
import spatial.targets._


object InOutArg extends SpatialApp { // Regression (Unit) // Args: 32
  @virtualize
  def main() {
    // Declare SW-HW interface vals
    val x = ArgIn[Int]
    val y = ArgOut[Int]
    val N = args(0).to[Int]

    // Connect SW vals to HW vals
    setArg(x, N)

    // Create HW accelerator
    Accel {
      println("hi")
      Pipe { y := x + 4 }
    }


    // Extract results from accelerator
    val result = getArg(y)

    // Create validation checks and debug code
    val gold = N + 4
    println("expected: " + gold)
    println("result: " + result)

    val cksum = gold == result
    println("PASS: " + cksum + " (InOutArg)")
  }
}

object StreamInOut extends SpatialApp {
  import spatial.targets.DE1

  @virtualize def main(): Unit = {
    val in  = StreamIn[Int](DE1.GPInput1)
    val out = StreamOut[Int](DE1.GPOutput1)
    Accel(*) {
      out := in
    }
  }
}

// Example of user syntax for explicitly setting II of a pipeline
object ExplicitIITest extends SpatialApp {
  @virtualize def main(): Unit = {
    val y = ArgIn[Int]

    Accel {
      val x = SRAM[Int](32)
      Pipe(ii=1).Foreach(0 until 32) { i =>
        x(i) = (x(i) * 32) / y.value
      }
    }
  }
}

object FriendlyTest extends SpatialApp {
  @virtualize def main(): Unit = {
    val y = ArgIn[Int]
    val z = ArgOut[Int]
    y := 3
    Accel {
      setArg(z, getArg(y) + 2)
    }
    println("z: " + z.value)
  }
}

object InlineSwitchTest extends SpatialApp {
  @virtualize def main(): Unit = {
    val y = ArgIn[Int]
    val x = ArgOut[Int]
    y := args(0).to[Int]
    Accel {
      val z = if (y == 3.to[Int]) 0 else if (y == 5.to[Int]) 1 else if (y == 7.to[Int]) 2 else 3
      x := z
    }
    println(getArg(x))
  }
}

object ModRewriteTest extends SpatialApp {
  @virtualize def main(): Unit = {
    val y = ArgIn[Int]
    val o = ArgOut[Int]

    setArg(y, 14)

    Accel {
      o := y % 8
    }

    println(getArg(o))
  }
}

object SRAMChar extends SpatialApp {
  val N = 16
  val len = 8192
  val depth = 2
  val p = 2
  type T = Int

  @virtualize def main(): Unit = {
    val outs = List.fill(N){ ArgOut[T] }

    Accel {
      val rfs = List.fill(N){ SRAM.buffer[T](len) }

      Foreach(0 until 1000) { _ =>
        List.tabulate(depth) { d =>
          Foreach(0 until 100 par p) { i =>
            rfs.zip(outs).foreach{case (rf,out) => if (d > 0) rf.update(i, i.to[T]) else out := rf(i) }
          }
        }
        ()
      }
    }
  }
}


object FloatBasics extends SpatialApp { // Regression (Unit) // Args: 3.2752 -283.70
  type T = Float //FixPt[TRUE,_16,_16]

  @virtualize
  def main() {

    val in1 = ArgIn[T]
    val in2 = ArgIn[T]
    val ffadd_out = ArgOut[T]
    val ffmul_out = ArgOut[T]
    val ffdiv_out = ArgOut[T]
    val ffsqrt_out = ArgOut[T]
    val ffsub_out = ArgOut[T]
    val fflt_out = ArgOut[Boolean]
    val ffgt_out = ArgOut[Boolean]
    val ffeq_out = ArgOut[Boolean]

    // val dram1 = DRAM[T](16)
    // val dram2 = DRAM[T](16)

    // val array1 = Array.tabulate[T](16){i => i.to[T]}

    // setMem(dram1, array1)
    setArg(in1, args(0).to[T])
    setArg(in2, args(1).to[T])
    Accel{
      // val sram1 = SRAM[T](16)
      ffadd_out := in1 + in2
      ffmul_out := in1 * in2
      ffdiv_out := in1 / in2
      ffsqrt_out := sqrt(in1)
      ffsub_out := in1 - in2
      fflt_out := in1 < in2
      ffgt_out := in1 > in2
      ffeq_out := in1.value == in2.value
    }

    val ffadd_result = getArg(ffadd_out)
    val ffmul_result = getArg(ffmul_out)
    val ffsub_result = getArg(ffsub_out)
    val ffdiv_result = getArg(ffdiv_out)
    val ffsqrt_result = getArg(ffsqrt_out)
    val fflt_result = getArg(fflt_out)
    val ffgt_result = getArg(ffgt_out)
    val ffeq_result = getArg(ffeq_out)

    val ffadd_gold = args(0).to[T] + args(1).to[T]
    val ffmul_gold = args(0).to[T] * args(1).to[T]
    val ffsub_gold = args(0).to[T] - args(1).to[T]
    val ffdiv_gold = args(0).to[T] / args(1).to[T]
    val ffsqrt_gold = sqrt(args(0).to[T])
    val ffgt_gold = args(0).to[T] > args(1).to[T]
    val fflt_gold = args(0).to[T] < args(1).to[T]
    val ffeq_gold = args(0).to[T] == args(1).to[T]

    println("sum: " + ffadd_result + " == " + ffadd_gold)
    println("prod: " + ffmul_result + " == " + ffmul_gold)
    println("sub: " + ffsub_result + " == " + ffsub_gold)
    println("div: " + ffdiv_result + " == " + ffdiv_gold)
    println("sqrt: " + ffsqrt_result + " == " + ffsqrt_gold)
    println("gt: " + ffgt_result + " == " + ffgt_gold)
    println("lt: " + fflt_result + " == " + fflt_gold)
    println("eq: " + ffeq_result + " == " + ffeq_gold)
    val cksum = /*ffsqrt_result == ffsqrt_gold && ffdiv_result == ffdiv_gold && */ffadd_result == ffadd_gold && ffmul_result == ffmul_gold && ffsub_result == ffsub_gold && fflt_result == fflt_gold && ffgt_result == ffgt_gold && ffeq_result == ffeq_gold
    println("PASS: " + cksum + " (FloatBasics) * Fix sqrt and div")
  }
}

object Tensor3D extends SpatialApp { // Regression (Unit) // Args: 32 4 4
  @virtualize
  def main() {
    // For 3D
    val tsP = 2
    val tsR = 2
    val tsC = 16
    val P = ArgIn[Int]
    val R = ArgIn[Int]
    val C = ArgIn[Int]
    val c = args(0).to[Int]
    val r = args(1).to[Int]
    val p = args(2).to[Int]
    setArg(P, p)
    setArg(R, r)
    setArg(C, c)
    val srcDRAM3 = DRAM[Int](P,R,C)
    val dstDRAM3 = DRAM[Int](P,R,C)
    val data3 = (0::p, 0::r, 0::c){(p,r,c) => r+c+p /*random[Int](5)*/}
    setMem(srcDRAM3, data3)

    Accel {
      val sram3 = SRAM[Int](tsP,tsR,tsC)
      Foreach(P by tsP, R by tsR, C by tsC) { (i,j,k) => 
        sram3 load srcDRAM3(i::i+tsP, j::j+tsR, k::k+tsC)
        dstDRAM3(i::i+tsP, j::j+tsR, k::k+tsC) store sram3
      }
    }

    // Extract results from accelerator
    val result3 = getTensor3(dstDRAM3)
    printTensor3(result3, "got: ")
    printTensor3(data3, "wanted; ")
    println("")
    val cksum = result3.zip(data3){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (Tensor3D)")
  }
}

object Tensor4D extends SpatialApp { // Regression (Unit) // Args: 32 4 4 4
  @virtualize
  def main() {
    val tsP = 2
    val tsR = 2
    val tsC = 16
    val tsX = 2
    val P = ArgIn[Int]
    val R = ArgIn[Int]
    val C = ArgIn[Int]
    val X = ArgIn[Int]
    val c = args(0).to[Int]
    val r = args(1).to[Int]
    val p = args(2).to[Int]
    val x = args(3).to[Int]
    setArg(P, p)
    setArg(R, r)
    setArg(C, c)
    setArg(X, x)

    val srcDRAM4 = DRAM[Int](X,P,R,C)
    val dstDRAM4 = DRAM[Int](X,P,R,C)
    val data4 = (0::x, 0::p, 0::r, 0::c){(x,p,r,c) => x+r+c+p /*random[Int](5)*/}
    setMem(srcDRAM4, data4)

    Accel {
      val sram4 = SRAM[Int](tsX,tsP,tsR,tsC)
      Foreach(X by tsX, P by tsP, R by tsR, C by tsC) { case List(h,i,j,k) => 
        sram4 load srcDRAM4(h::h+tsX, i::i+tsP, j::j+tsR, k::k+tsC)
        dstDRAM4(h::h+tsX, i::i+tsP, j::j+tsR, k::k+tsC) store sram4
      }
    }

    // Extract results from accelerator
    val result4 = getTensor4(dstDRAM4)
    printTensor4(result4, "got: ")
    printTensor4(data4, "wanted: ")
    println("")
    val cksum = result4.zip(data4){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (Tensor4D)")
  }
}

object Tensor5D extends SpatialApp { // Regression (Unit) // Args: 32 4 4 4 4
  @virtualize
  def main() {
    // For 3D
    val tsP = 2
    val tsR = 2
    val tsC = 16
    val tsX = 2
    val tsY = 2
    val P = ArgIn[Int]
    val R = ArgIn[Int]
    val C = ArgIn[Int]
    val X = ArgIn[Int]
    val Y = ArgIn[Int]
    val c = args(0).to[Int]
    val r = args(1).to[Int]
    val p = args(2).to[Int]
    val x = args(3).to[Int]
    val y = args(4).to[Int]
    setArg(P, p)
    setArg(R, r)
    setArg(C, c)
    setArg(X, x)
    setArg(Y, y)

    val srcDRAM5 = DRAM[Int](Y,X,P,R,C)
    val dstDRAM5 = DRAM[Int](Y,X,P,R,C)
    val data5 = (0::y, 0::x, 0::p, 0::r, 0::c){(y,x,p,r,c) => y+x+r+c+p /*random[Int](5)*/}
    setMem(srcDRAM5, data5)

    Accel {
      val sram5 = SRAM[Int](tsY,tsX,tsP,tsR,tsC)
      Foreach(Y by tsY, X by tsX, P by tsP, R by tsR, C by tsC) { case List(g,h,i,j,k) => 
        sram5 load srcDRAM5(g::g+tsY, h::h+tsX, i::i+tsP, j::j+tsR, k::k+tsC)
        dstDRAM5(g::g+tsY, h::h+tsX, i::i+tsP, j::j+tsR, k::k+tsC) store sram5
      }
    }


    // Extract results from accelerator
    val result5 = getTensor5(dstDRAM5)
    printTensor5(result5, "got: ")
    printTensor5(data5, "wanted; ")
    println("")

    val cksum = result5.zip(data5){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (Tensor5D)")
  }
}

object LUTTest extends SpatialApp { // Regression (Unit) // Args: 2


  type T = FixPt[TRUE,_32,_32]
  @virtualize
  def main() {
    // Declare SW-HW interface vals
    val i = ArgIn[Int]
    val y = ArgOut[T]
    val ii = args(0).to[Int]

    // Connect SW vals to HW vals
    setArg(i, ii)

    // Create HW accelerator
    Accel {
      val lut = LUT[T](4, 4)(
         0,  (1*1E0).to[T],  2,  3,
         4,  -5,  6,  7,
         8,  9, -10, 11,
        12, 13, 14, -15
      )
      val red = Reduce(Reg[T](0))(3 by 1 par 3) {q =>
        lut(q,q)
      }{_^_}
      y := lut(1, 3) ^ lut(3, 3) ^ red ^ lut(i,0)
    }


    // Extract results from accelerator
    val result = getArg(y)

    // Create validation checks and debug code
    val gold = (-15 ^ 7 ^ -0 ^ -5 ^ -10 ^ 4*ii).to[T]
    println("expected: " + gold)
    println("result: " + result)

    val cksum = gold == result
    println("PASS: " + cksum + " (InOutArg)")
  }
}

object MixedIOTest extends SpatialApp { // Regression (Unit) // Args: none


  @virtualize 
  def main(): Unit = { 
    val cst1 = 32
    val cst2 = 23;
    val cst3 = 11
    val cst4 = 7
    val io1 = HostIO[Int]
    val io2 = HostIO[Int]
    val io_unused = HostIO[Int]
    val x1 = ArgIn[Int]
    val x2 = ArgIn[Int]
    val x_unused = ArgIn[Int]
    val y1 = ArgOut[Int]
    val y2 = ArgOut[Int]
    val y3 = ArgOut[Int]
    val y4 = ArgOut[Int]
    val y5 = ArgOut[Int]
    val y_unused = ArgOut[Int]
    val m1 = DRAM[Int](16)
    val m2 = DRAM[Int](16)
    val m_unused = DRAM[Int](16)
    setArg(io1, cst1)
    setArg(io2, cst2)
    setArg(x1, cst3)
    setArg(x2, cst4)
    val data = Array[Int](0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
    // val data = Array.tabulate(16){i => i}
    setMem(m1, data)

    Accel {
      Pipe { io1 := io1.value + 2}
      Pipe { io2 := io2.value + 4}
      Pipe { y2 := 999 }
      Pipe { y1 := x1.value + 6 }
      Pipe { y2 := x2.value + 8 }

      val reg = Reg[Int](0) // Nbuffered reg with multi writes, note that it does not do what you think!
      Sequential.Foreach(3 by 1) {i => 
        Pipe{reg :+= 1}
        Pipe{y4 := reg}
        Pipe{reg :+= 1}
        Pipe{y5 := reg}
      }
      val sram1 = SRAM[Int](16)
      val sram2 = SRAM[Int](16)
      sram1 load m1
      sram2 load m1
      m2 store sram1
      Pipe { y3 := sram2(3) }
    }

    val r1 = getArg(io1)
    val g1 = cst1 + 2
    val r2 = getArg(io2)
    val g2 = cst2 + 4
    val r3 = getArg(y1)
    val g3 = cst3 + 6
    val r4 = getArg(y2)
    val g4 = cst4 + 8
    val r5 = getMem(m2)
    val g6 = data(3)
    val r6 = getArg(y3)
    val g7 = 5
    val r7 = getArg(y4)
    val g8 = 6
    val r8 = getArg(y5)
    println("expected: " + g1 + ", " + g2 + ", " + g3 + ", " + g4 + ", "+ g6 + ", " + g7 + ", " + g8)
    println("received: " + r1 + ", " + r2 + ", " + r3 + ", " + r4 + ", "+ r6 + ", " + r7 + ", " + r8)
    printArray(r5, "Mem: ")
    val cksum = r1 == g1 && r2 == g2 && r3 == g3 && r4 == g4 && r6 == g6 && data.zip(r5){_==_}.reduce{_&&_} && r7 == g7 && r8 == g8
    println("PASS: " + cksum + " (MixedIOTest) ")
  }
}

object MultiplexedWriteTestZynq extends SpatialApp { // Regression (Unit) // Args: none
  override val target = targets.Zynq


  val tileSize = 16
  val I = 5
  val N = 192

  @virtualize
  def multiplexedwrtest[W:Type:Num](w: Array[W], i: Array[W]): Array[W] = {
    val T = param(tileSize)
    val P = param(4)
    val weights = DRAM[W](N)
    val inputs  = DRAM[W](N)
    val weightsResult = DRAM[W](N*I)
    setMem(weights, w)
    setMem(inputs,i)
    Accel {
      val wt = SRAM[W](T)
      val in = SRAM[W](T)
      Sequential.Foreach(N by T){i =>
        wt load weights(i::i+T par 16)
        in load inputs(i::i+T par 16)

        // Some math nonsense (definitely not a correct implementation of anything)
        Foreach(I by 1){x =>
          val niter = Reg[Int]
          niter := x+1
          MemReduce(wt)(niter by 1){ i =>  // s0 write
            in
          }{_+_}
          weightsResult(i*I+x*T::i*I+x*T+T par 16) store wt //s1 read
        }
      }

    }
    getMem(weightsResult)
  }

  @virtualize
  def main() = {
    val w = Array.tabulate(N){ i => i % 256}
    val i = Array.tabulate(N){ i => i % 256 }

    val result = multiplexedwrtest(w, i)

    val gold = Array.tabulate(N/tileSize) { k =>
      Array.tabulate(I){ j => 
        val in = Array.tabulate(tileSize) { i => (j)*(k*tileSize + i) }
        val wt = Array.tabulate(tileSize) { i => k*tileSize + i }
        in.zip(wt){_+_}
      }.flatten
    }.flatten
    printArray(gold, "gold: ");
    printArray(result, "result: ");

    val cksum = gold.zip(result){_==_}.reduce{_&&_}
    println("PASS: " + cksum  + " (MultiplexedWriteTest)")
  }
}


// Args: None
object MultiplexedWriteTest extends SpatialApp { // Regression (Unit) // Args: none


  val tileSize = 16
  val I = 5
  val N = 192

  @virtualize
  def multiplexedwrtest[W:Type:Num](w: Array[W], i: Array[W]): Array[W] = {
    val T = param(tileSize)
    val P = param(4)
    val weights = DRAM[W](N)
    val inputs  = DRAM[W](N)
    val weightsResult = DRAM[W](N*I)
    setMem(weights, w)
    setMem(inputs,i)
    Accel {
      val wt = SRAM[W](T)
      val in = SRAM[W](T)
      Sequential.Foreach(N by T){i =>
        wt load weights(i::i+T par 16)
        in load inputs(i::i+T par 16)

        // Some math nonsense (definitely not a correct implementation of anything)
        Foreach(I by 1){x =>
          val niter = Reg[Int]
          niter := x+1
          MemReduce(wt)(niter by 1){ i =>  // s0 write
            in
          }{_+_}
          weightsResult(i*I+x*T::i*I+x*T+T par 16) store wt //s1 read
        }
      }

    }
    getMem(weightsResult)
  }

  @virtualize
  def main() = {
    val w = Array.tabulate(N){ i => i % 256}
    val i = Array.tabulate(N){ i => i % 256 }

    val result = multiplexedwrtest(w, i)

    val gold = Array.tabulate(N/tileSize) { k =>
      Array.tabulate(I){ j => 
        val in = Array.tabulate(tileSize) { i => (j)*(k*tileSize + i) }
        val wt = Array.tabulate(tileSize) { i => k*tileSize + i }
        in.zip(wt){_+_}
      }.flatten
    }.flatten
    printArray(gold, "gold: ");
    printArray(result, "result: ");

    val cksum = gold.zip(result){_==_}.reduce{_&&_}
    println("PASS: " + cksum  + " (MultiplexedWriteTest)")
  }
}

// TODO: Make this actually check a bubbled NBuf (i.e.- s0 = wr, s2 = wr, s4 =rd, s1s2 = n/a)
// because I think this will break the NBuf SM since it won't detect drain completion properly
// Args: None
object BubbledWriteTest extends SpatialApp { // Regression (Unit) // Args: none


  val tileSize = 16
  val I = 5
  val N = 192

  @virtualize
  def bubbledwrtest(w: Array[Int], i: Array[Int]): Array[Int] = {
    val T = param(tileSize)
    val P = param(4)
    val weights = DRAM[Int](N)
    val inputs  = DRAM[Int](N)
    val weightsResult = DRAM[Int](N*I)
    // val dummyWeightsResult = DRAM[Int](T)
    // val dummyOut = DRAM[Int](T)
    // val dummyOut2 = DRAM[Int](T)
    setMem(weights, w)
    setMem(inputs,i)
    Accel {

      val wt = SRAM[Int](T)
      val in = SRAM[Int](T)
      Sequential.Foreach(N by T){i =>
        wt load weights(i::i+T par 16)
        in load inputs(i::i+T par 16)
        val niter = Reg[Int]
        Pipe{niter.reset}
        Pipe{niter.reset} // Testing codegen for multiple resetters
        Foreach(I by 1){x =>
          // niter := niter + 1
          niter :+= 1
          MemReduce(wt)(niter by 1){ k =>  // s0 write
            in
          }{_+_}
          val dummyReg1 = Reg[Int]
          val dummyReg2 = Reg[Int]
          val dummyReg3 = Reg[Int]
          Foreach(T by 1) { i => dummyReg1 := in(i)} // s1 do not touch
          Foreach(T by 1) { i => dummyReg2 := wt(i)} // s2 read
          Foreach(T by 1) { i => dummyReg3 := in(i)} // s3 do not touch
          weightsResult(i*I+x*T::i*I+x*T+T par 16) store wt //s4 read
        }
      }

    }
    getMem(weightsResult)
  }

  @virtualize
  def main() = {
    val w = Array.tabulate(N){ i => i % 256}
    val i = Array.tabulate(N){ i => i % 256 }

    val result = bubbledwrtest(w, i)

    // // Non-resetting SRAM check
    // val gold = Array.tabulate(N/tileSize) { k =>
    //   Array.tabulate(I){ j => 
    //     Array.tabulate(tileSize) { i => 
    //       ( 1 + (j+1)*(j+2)/2 ) * (i + k*tileSize)
    //     }
    //   }.flatten
    // }.flatten
    // Resetting SRAM check
    val gold = Array.tabulate(N/tileSize) { k =>
      Array.tabulate(I){ j => 
        val in = Array.tabulate(tileSize) { i => (j)*(k*tileSize + i) }
        val wt = Array.tabulate(tileSize) { i => k*tileSize + i }
        in.zip(wt){_+_}
      }.flatten
    }.flatten
    printArray(gold, "gold: ")
    printArray(result, "result: ")

    val cksum = gold.zip(result){_==_}.reduce{_&&_}
    println("PASS: " + cksum  + " (BubbledWriteTest)")


  }
}

object ArbitraryLambda extends SpatialApp { // Regression (Unit) // Args: 8


  @virtualize
  def main() {
    // Declare SW-HW interface vals
    val x = ArgIn[Int]
    val r_xor = ArgOut[Int]
    val f_xor = ArgOut[Int]
    val N = args(0).to[Int]

    // Connect SW vals to HW vals
    setArg(x, N)

    // Create HW accelerator
    Accel {
      val reduce_xor = Reg[Int](99)
      Reduce(reduce_xor)(x by 1){i =>
        val temp = mux(i % 3 == 1, i, i+1)
        temp
      } { _^_ }
      r_xor := reduce_xor

      val fold_xor = Reg[Int](99)
      Fold(fold_xor)(x by 1){i =>
        val temp = Reg[Int](0)
        temp := mux(i % 3 == 1, i, i+1)
        temp
      } { _^_ }
      f_xor := fold_xor
    }


    // Extract results from accelerator
    val reduce_xor_result = getArg(r_xor)
    val fold_xor_result = getArg(f_xor)

    // Create validation checks and debug code
    val gold_reduce_xor = Array.tabulate(N){i => if (i % 3 == 1) i else i+1}.reduce{_^_}
    val gold_fold_xor = Array.tabulate(N){i => if (i % 3 == 1) i else i+1}.reduce{_^_} ^ 99
    println("Reduce XOR: ")
    println("  expected: " + gold_reduce_xor)
    println("  result: " + reduce_xor_result)
    println("Reduce XOR: ")
    println("  expected: " + gold_fold_xor)
    println("  result: " + fold_xor_result)

    val cksum_reduce_xor = gold_reduce_xor == reduce_xor_result
    val cksum_fold_xor = gold_fold_xor == fold_xor_result
    val cksum = cksum_reduce_xor && cksum_fold_xor
    println("PASS: " + cksum + " (ArbitraryLambda)")
  }
}

object Niter extends SpatialApp { // Regression (Unit) // Args: 100

  
  val constTileSize = 16

  @virtualize
  def nIterTest[T:Type:Num](len: Int): T = {
    val innerPar = 1 (1 -> 1)
    val tileSize = constTileSize (constTileSize -> constTileSize)
    bound(len) = 9216

    val N = ArgIn[Int]
    val out = ArgOut[T]
    setArg(N, len)

    Accel {
      Sequential {
        Sequential.Foreach(N by tileSize){ i =>
          val redMax = Reg[Int](999)
          Pipe{ redMax := min(tileSize, N.value-i) }
          val accum = Reduce(Reg[T](0.to[T]))(redMax par innerPar){ ii =>
            (i + ii).to[T]
          } {_+_}
          Pipe { out := accum }
        }
      }
    }

    getArg(out)
  }

  @virtualize
  def main() {
    val len = args(0).to[Int]

    val result = nIterTest[Int](len)

    val m = (len-1)%constTileSize + 1
    val b1 = m*(m-1)/2
    val gold = b1 + (len - m)*m
    println("expected: " + gold)
    println("result:   " + result)

    val cksum = gold == result
    println("PASS: " + cksum + " (Niter)")
  }
}


object MemTest1D extends SpatialApp { // Regression (Unit) // Args: 7


  @virtualize
  def main() {

    // Declare SW-HW interface vals
    val x = ArgIn[Int]
    val y = ArgOut[Int]
    val N = args(0).to[Int]

    // Connect SW vals to HW vals
    setArg(x, N)

    // Create HW accelerator
    Accel {
      val mem = SRAM[Int](384)
      Sequential.Foreach(384 by 1) { i =>
        mem(i) = x + i.to[Int]
      }
      Pipe { y := mem(383) }
    }


    // Extract results from accelerator
    val result = getArg(y)

    // Create validation checks and debug code
    val gold = N+383
    println("expected: " + gold)
    println("result: " + result)

    val cksum = gold == result
    println("PASS: " + cksum + " (MemTest1D)")
  }
}

object MemTest2D extends SpatialApp { // Regression (Unit) // Args: 7


  @virtualize
  def main() {

    // Declare SW-HW interface vals
    val x = ArgIn[Int]
    val y = ArgOut[Int]
    val N = args(0).to[Int]

    // Connect SW vals to HW vals
    setArg(x, N)

    // Create HW accelerator
    Accel {
      val mem = SRAM[Int](64, 128)
      Sequential.Foreach(64 by 1, 128 by 1) { (i,j) =>
        mem(i,j) = x + (i.to[Index]*128+j.to[Index]).to[Int]
      }
      Pipe { y := mem(63,127) }
    }


    // Extract results from accelerator
    val result = getArg(y)

    // Create validation checks and debug code
    val gold = N+63*128+127
    println("expected: " + gold)
    println("result: " + result)

    val cksum = gold == result
    println("PASS: " + cksum + " (MemTest2D)")
  }
}

object FifoLoadSRAMStore extends SpatialApp { // Regression (Unit) // Args: 192

  @virtualize
  def fifoLoad[T:Type:Num](srcHost: Array[T], N: Int) = {
    val tileSize = 16 (64 -> 64)

    val size = ArgIn[Int]
    setArg(size, N)

    val srcFPGA = DRAM[T](size)
    val dstFPGA = DRAM[T](size)
    setMem(srcFPGA, srcHost)

    Accel {
      val f1 = FIFO[T](tileSize)
      Sequential.Foreach(size by tileSize) { i =>
        f1 load srcFPGA(i::i + tileSize par 1)
        val b1 = SRAM[T](tileSize)
        Sequential.Foreach(tileSize by 1) { i =>
          Pipe{b1(i) = f1.peek()}
          Pipe{f1.deq()}
        }
        dstFPGA(i::i + tileSize par 1) store b1
      }
      ()
    }
    getMem(dstFPGA)
  }

  @virtualize
  def main() {
    val arraySize = args(0).to[Int]

    val src = Array.tabulate(arraySize){i => i % 256}
    val dst = fifoLoad(src, arraySize)

    val gold = src

    println("Sent in: ")
    (0 until arraySize) foreach { i => print(gold(i) + " ") }
    println("\nGot out:")
    (0 until arraySize) foreach { i => print(dst(i) + " ") }
    println("")

    val cksum = dst.zip(gold){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (FifoLoad)")


  }
}

object SimpleSequential extends SpatialApp { // Regression (Unit) // Args: 5 8


  def simpleSeq(xIn: Int, yIn: Int): Int = {
    val innerPar = 1 (1 -> 1)
    val tileSize = 64 (64 -> 64)

    val x = ArgIn[Int]
    val y = ArgIn[Int]
    val out = ArgOut[Int]
    setArg(x, xIn)
    setArg(y, yIn)
    Accel {
      val bram = SRAM[Int](tileSize)
      Foreach(tileSize by 1 par innerPar){ ii =>
        bram(ii) = x.value * ii
      }
      out := bram(y.value)
    }
    getArg(out)
  }

  @virtualize
  def main() {
    val x = args(0).to[Int]
    val y = args(1).to[Int]
    val result = simpleSeq(x, y)

    val a1 = Array.tabulate(64){i => x * i}
    val gold = a1(y)

    println("expected: " + gold)
    println("result:   " + result)
    val chkSum = result == gold
    // assert(chkSum)
    println("PASS: " + chkSum + " (SimpleSeq)")
  }
}


object DeviceMemcpy extends SpatialApp { // Regression (Unit) // Args: 50


  val N = 192
  type T = Int
  def memcpyViaFPGA(srcHost: Array[T]): Array[T] = {
    val fpgaMem = DRAM[Int](N)
    setMem(fpgaMem, srcHost)

    val y = ArgOut[Int]
    Accel { y := 10 }

    getMem(fpgaMem)
  }

  @virtualize
  def main() {
    val arraySize = N
    val c = args(0).to[Int]

    val src = Array.tabulate(arraySize){i => i*c }
    val dst = memcpyViaFPGA(src)
    println("Sent in: ")
    for (i <- 0 until arraySize){ print(src(i) + " ") }
    println("\nGot out: ")
    for (i <- 0 until arraySize){ print(dst(i) + " ") }
    println("")
    val chkSum = dst.zip(src){_ == _}.reduce{_&&_}
    println("PASS: " + chkSum + " (DeviceMemcpy)")
  }
}

object SimpleTileLoadStore extends SpatialApp { // Regression (Unit) // Args: 100


  val N = 192

  @virtualize
  def simpleLoadStore[T:Type:Num](srcHost: Array[T], value: T) = {
    val loadPar  = 1 (1 -> 1)
    val storePar = 1 (1 -> 1)
    val tileSize = 16 (16 -> 16)

    val srcFPGA = DRAM[T](N)
    val dstFPGA = DRAM[T](N)
    setMem(srcFPGA, srcHost)

    val x = ArgIn[T]
    setArg(x, value)
    Accel {
      Sequential.Foreach(N by tileSize par 2) { i =>
        val b1 = SRAM[T](tileSize)

        b1 load srcFPGA(i::i+tileSize par 1)

        val b2 = SRAM[T](tileSize)
        Foreach(tileSize by 1 par 4) { ii =>
          b2(ii) = b1(ii) * x
        }

        dstFPGA(i::i+tileSize par 1) store b2
      }
    }
    getMem(dstFPGA)
  }

  @virtualize
  def main() {
    val arraySize = N
    val value = args(0).to[Int]

    val src = Array.tabulate[Int](arraySize) { i => i % 256 }
    val dst = simpleLoadStore(src, value)

    val gold = src.map { _ * value }

    println("Sent in: ")
    (0 until arraySize) foreach { i => print(gold(i) + " ") }
    println("Got out: ")
    (0 until arraySize) foreach { i => print(dst(i) + " ") }
    println("")

    val cksum = dst.zip(gold){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (SimpleTileLoadStore)")
  }
}

object StridedLoad extends SpatialApp {


  val N = 192
  @virtualize
  def main() {

    val src = (0::64,0::64){(i,j) => i+j}
    val dram = DRAM[Int](64,64)
    val out = DRAM[Int](32,64)
    setMem(dram, src)
    Accel{
      val sram = SRAM[Int](32,64)
      sram load dram(0::64 by 2, 0::64)
      out store sram
    }

    val gold = (0::32, 0::64){(i,j) => src(2*i, j)}
    val received = getMatrix(out)

    printMatrix(gold, "gold")
    printMatrix(received, "received")

    val cksum = received.zip(gold){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (StridedLoad)")
  }
}

object OHM extends SpatialApp { // Regression (Unit) // Args: 400

  @virtualize
  def main() {

    val src1 = (0::16,0::16){(i,j) => i}
    val dram1 = DRAM[Int](16,16)
    val dram2 = DRAM[Int](16,16)

    setMem(dram1,src1)

    Accel {
      val sram1 = SRAM[Int](16)
      Foreach(-5 until 15 by 1){i =>
        val ldrow = if ((i.to[Int] + 1.to[Int]) >= 0.to[Int] && (i.to[Int] + 1.to[Int]) <= 15) {i.to[Int] + 1.to[Int]} else 0
        sram1 load dram1(ldrow,0::16)
        dram2(ldrow,0::16) store sram1
      }
    }
    val out = getMatrix(dram2)
    printMatrix(out, "Result:") 
    printMatrix(src1, "Gold:")

    val cksum = out.zip(src1){_==_}.reduce{_&&_}
    println("PASS: " + cksum + " (OHM)")
  }
}


object UnalignedFifoLoad extends SpatialApp { // Regression (Unit) // Args: 400

  
  val tileSize = 20

  @virtualize
  def singleFifoLoad[T:Type:Num](src1: Array[T], in: Int) = {

    val P1 = 1 (16 -> 16)

    val N = ArgIn[Int]
    setArg(N, in)

    val src1FPGA = DRAM[T](N)
    val out = ArgOut[T]
    setMem(src1FPGA, src1)

    Accel {
      val f1 = FIFO[T](3*tileSize)
      Foreach(N by tileSize) { i =>
        f1 load src1FPGA(i::i+tileSize par P1)
        val accum = Reg[T](0.to[T])
        accum.reset
        Reduce(accum)(tileSize by 1 par 1){i =>
          f1.deq()
        }{_+_}
        Pipe { out := accum }
      }
      ()
    }
    getArg(out)
  }

  @virtualize
  def main() {
    val arraySize = args(0).to[Int]

    val src1 = Array.tabulate(arraySize) { i => i % 256}
    val out = singleFifoLoad(src1, arraySize)

    val sub1_for_check = Array.tabulate(arraySize-tileSize) {i => i % 256}

    // val gold = src1.zip(src2){_*_}.zipWithIndex.filter( (a:Int, i:Int) => i > arraySize-64).reduce{_+_}
    val gold = src1.reduce{_+_} - sub1_for_check.reduce(_+_)
    println("gold: " + gold)
    println("out: " + out)

    val cksum = out == gold
    println("PASS: " + cksum + " (UnalignedFifoLoad)")
  }
}

// object StridedConv extends SpatialApp { // DISABLED Regression (Unit) // Args: 192

  
//   val maxcols = 64
//   val stride = 2
//   val kernel = 3

//   @virtualize
//   def main() = {
//     val rows = args(0).to[Int]
//     val M = ArgIn[Int]
//     val N = ArgIn[Int]
//     val Md2 = ArgIn[Int]
//     val Nd2 = ArgIn[Int]
//     setArg(M, rows)
//     setArg(N, maxcols)
//     setArg(Md2, rows/2)
//     setArg(Nd2, maxcols/2)

//     val src = (0::rows,0::maxcols){(i,j) => i + j}
//     val dram1 = DRAM[Int](M,N)
//     val dram2 = DRAM[Int](Md2,Nd2)

//     setMem(dram1, src)

//     Accel {
//       val lb = LineBuffer.strided[Int](kernel, maxcols, stride) // Can't it figure this out from the access?
//       val sr = RegFile[Int](kernel, kernel)
//       val filter = LUT[Int](kernel, kernel)(1,0,1,
//                                             2,1,2,
//                                             1,0,1)
//       val buffer = SRAM[Int](maxcols)
//       Foreach(M by stride){line => 
//         lb load dram1(line, 0::N par 4)
//         Foreach(N by stride){j => 
//           Foreach(kernel by 1 par kernel){i => sr(i,*) <<= lb(i,j::j+stride)}
//           val accum = Reduce(Reg[Int](0))(kernel by 1, kernel by 1){(ii,jj) => 
//             sr(ii,jj) * filter(ii,jj)
//           }{_+_}
//           buffer(j/2) = if (j < kernel || line < kernel) 0 else accum.value
//         }
//         dram2(line/2, 0::Nd2) store buffer
//       }
//     }

//     val result = getMatrix(dram2)
//     printMatrix(result, "Result: ")

//     val gold = (0::rows/2, 0::maxcols/2){(i,j) => 
//       if (i*2 < kernel || j*2 < kernel) 0 else 1 // TODO
//     }

//     val cksum = result.zip(gold){_==_}.reduce{_&&_}
//     println("PASS: " + cksum + " (StridedConv)")
//   }

// }

object CompactingFifo extends SpatialApp { // Regression (Unit) // Args: 640
  val tileSize = 64

  @virtualize
  def main() {
    val arraySize = args(0).to[Int]

    val bitmask = Array.tabulate(arraySize) { i => random[Int](2)}
    printArray(bitmask, "Bitmask: ")

    val P1 = 4 (16 -> 16)

    val N = ArgIn[Int]
    setArg(N, arraySize)

    val bitmaskDRAM = DRAM[Int](N)
    setMem(bitmaskDRAM, bitmask)
    val out = DRAM[Int](N)

    Accel{
      Sequential.Foreach(N by tileSize){ i => 
        val bitmasks = SRAM[Int](tileSize)
        val fifo = FIFO[Int](tileSize)
        bitmasks load bitmaskDRAM(i :: i + tileSize)

        // Load while respecting bitmask
        Foreach(tileSize by 1 par P1){ j => 
          fifo.enq(i+j, bitmasks(j) == 1)
        }

        // Fill remainder with 0s
        FSM[Int](filler => filler != 1){filler => 
          if (!fifo.full) {
            Pipe{fifo.enq(-1)}
          }
        }{ filler => mux(fifo.full, 1, 0)}

        // Store back
        out(i :: i + tileSize par 2) store fifo
      }
    }

    val result = getMem(out)

    val gold = Array.empty[Int](arraySize)
    var head = 0
    var tail = tileSize-1
    for (j <- 0 until arraySize by tileSize) {
      head = 0
      tail = tileSize-1
      for (k <- 0 until tileSize) {
        if (bitmask(j+k) == 1){
          gold(j+head) = j+k
          head = head + 1
        } else {
          gold(j+tail) = -1
          tail = tail - 1
        }
      }
    }
    printArray(gold, "gold: ")
    printArray(result, "got: ")

    val cksum = result.zip(gold){_==_}.reduce{_&&_}
    println("PASS: " + cksum + " (CompactingFifo)")
  }
}

object ParFifoLoad extends SpatialApp { // Regression (Unit) // Args: 384


  val tileSize = 64
  @virtualize
  def parFifoLoad[T:Type:Num](src1: Array[T], src2: Array[T], src3: Array[T], in: Int) = {

    val P1 = 1 (16 -> 16)

    val N = ArgIn[Int]
    setArg(N, in)

    val src1FPGA = DRAM[T](N)
    val src2FPGA = DRAM[T](N)
    val src3FPGA = DRAM[T](N)
    val out = ArgOut[T]
    setMem(src1FPGA, src1)
    setMem(src2FPGA, src2)
    setMem(src3FPGA, src3)

    Accel {
      val f1 = FIFO[T](tileSize)
      val f2 = FIFO[T](tileSize)
      val f3 = FIFO[T](tileSize)
      Foreach(N by tileSize) { i =>
        Parallel {
          f1 load src1FPGA(i::i+tileSize par P1)
          f2 load src2FPGA(i::i+tileSize par P1)
          f3 load src3FPGA(i::i+tileSize par P1)
        }
        val accum = Reduce(Reg[T](0.to[T]))(tileSize by 1){i =>
          f1.deq() * f2.deq() * f3.deq()
        }{_+_}
        Pipe { out := accum }
      }
      ()
    }
    getArg(out)
  }

  @virtualize
  def main() {
    val arraySize = args(0).to[Int]

    val src1 = Array.tabulate(arraySize) { i => i % 4 }
    val src2 = Array.tabulate(arraySize) { i => i % 4 + 16}
    val src3 = Array.tabulate(arraySize) { i => i % 4 + 2*16}
    val out = parFifoLoad(src1, src2, src3, arraySize)

    val sub1_for_check = Array.tabulate(arraySize-tileSize) {i => i % 4}
    val sub2_for_check = Array.tabulate(arraySize-tileSize) {i => i % 4 + 16}
    val sub3_for_check = Array.tabulate(arraySize-tileSize) {i => i % 4 + 2*16}

    // val gold = src1.zip(src2){_*_}.zipWithIndex.filter( (a:Int, i:Int) => i > arraySize-64).reduce{_+_}
    val gold = src1.zip(src2){_*_}.zip(src3){_*_}.reduce{_+_} - sub1_for_check.zip(sub2_for_check){_*_}.zip(sub3_for_check){_*_}.reduce(_+_)
    println("gold: " + gold)
    println("out: " + out)

    val cksum = out == gold
    println("PASS: " + cksum + " (ParFifoLoad)")
  }
}


object EfficiencyTest extends SpatialApp {
  def main() {

    val N = ArgIn[Int]
    setArg(N, args(0).to[Int])
    val out = ArgOut[Int]
    Accel {
      val reg = Reg[Int]
      Foreach(N by 1){i => 
        Foreach(3 by 1){j => reg := j}
        Foreach(3 by 1){j => reg := j}
        Foreach(3 by 1){j => reg := j}
        Foreach(3 by 1){j => out := j}
      }

      Sequential.Foreach(N by 1){i => 
        Foreach(3 by 1){j => out := j}
        Foreach(3 by 1){j => out := j}
        Foreach(3 by 1){j => out := j}
        Foreach(3 by 1){j => out := j}
      }
    }
  }
}

object FifoLoadStore extends SpatialApp { // Regression (Unit) // Args: none


  val N = 32

  def fifoLoadStore[T:Type:Bits](srcHost: Array[T]) = {
    val tileSize = N

    val srcFPGA = DRAM[T](N)
    val dstFPGA = DRAM[T](N)
    setMem(srcFPGA, srcHost)

    Accel {
      val f1 = FIFO[T](tileSize)
      // Parallel {
      Sequential {
        f1 load srcFPGA(0::tileSize par 16)
        dstFPGA(0::tileSize par 16) store f1
      }
      // Pipe(tileSize by 1) { i => // This pipe forces the loadstore to run for enough iters
      //   dummyOut := i
      // }
      // }
      ()
    }
    getMem(dstFPGA)
  }

  @virtualize
  def main() {
    val arraySize = N

    val src = Array.tabulate(arraySize) { i => i % 256 }
    val dst = fifoLoadStore(src)

    val gold = src

    println("gold:")
    (0 until arraySize) foreach { i => print(gold(i) + " ") }
    println("")
    println("dst:")
    (0 until arraySize) foreach { i => print(dst(i) + " ") }
    println("")

    val cksum = dst.zip(gold){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (FifoLoadStore)")
  }
}

object StackLoadStore extends SpatialApp { // Regression (Unit) // Args: none


  val N = 32

  @virtualize
  def stackLoadStore[T:Type:Bits](srcHost: Array[T]) = {
    val tileSize = N

    val srcFPGA = DRAM[T](N)
    val dstFPGA = DRAM[T](N)
    setMem(srcFPGA, srcHost)

    Accel {
      val f1 = FILO[T](tileSize)
      // Parallel {
      Sequential {
        f1 load srcFPGA(0::tileSize par 16)
        dstFPGA(0::tileSize par 8) store f1
      }
      // Pipe(tileSize by 1) { i => // This pipe forces the loadstore to run for enough iters
      //   dummyOut := i
      // }
      // }
      ()
    }
    getMem(dstFPGA)
  }

  @virtualize
  def main() {
    val arraySize = N

    val src = Array.tabulate(arraySize) { i => i % 256 }
    val dst = stackLoadStore(src)

    val gold = Array.tabulate(arraySize) {i => src(arraySize-1-i) }

    println("gold:")
    (0 until arraySize) foreach { i => print(gold(i) + " ") }
    println("")
    println("dst:")
    (0 until arraySize) foreach { i => print(dst(i) + " ") }
    println("")

    val cksum = dst.zip(gold){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (StackLoadStore)")
  }
}



object SimpleReduce extends SpatialApp { // Regression (Unit) // Args: 7
  val N = 16.to[Int]

  @virtualize
  def simpleReduce[T:Type:Num](xin: T) = {

    val x = ArgIn[T]
    val out = ArgOut[T]
    setArg(x, xin)

    Accel {
      out := Reduce(Reg[T](0.to[T]))(-N until 0 by 1){ ii =>
        x.value * ii.to[T]
      }{_+_}
    }
    getArg(out)
  }

  @virtualize
  def main() {
    val x = args(0).to[Int]

    val result = simpleReduce(x)

    val gold = Array.tabulate(N){i => x * (i-N)}.reduce{_+_}
    println("expected: " + gold)
    println("result:   " + result)

    val cksum = gold == result
    println("PASS: " + cksum + " (SimpleReduce)")
  }
}

object SimpleMemReduce extends SpatialApp { // Regression (Unit) // Args: none


  val N = 16.to[Int]

  @virtualize
  def main() {

    val out = DRAM[Int](16)
    val out2 = DRAM[Int](16)

    Accel {
      val a = SRAM[Int](16)
      MemReduce(a)(-5 until 0 by 1){i =>
        val tmp = SRAM[Int](16)
        Foreach(16 by 1) { j => tmp(j) = 1}
        tmp
      }{_+_}
      val b = SRAM[Int](16)
      Foreach(15 until -1 by -1){i => b(i) = 2}
      out store a
      out2 store b
    }
    val result = getMem(out)
    val result2 = getMem(out2)

    val gold = Array.tabulate(16){i => 5.to[Int]}
    val gold2 = Array.tabulate(16){i => 2.to[Int]}
    printArray(gold, "expected: ")
    printArray(result, "result:   ")
    printArray(gold2, "expected: ")
    printArray(result2, "result:   ")

    val cksum = gold.zip(result){_==_}.reduce{_&&_} && gold2.zip(result2){_==_}.reduce{_&&_}
    println("PASS: " + cksum + " (SimpleMemReduce)")
  }
}



object SimpleFold extends SpatialApp { // Regression (Unit) // Args: 1920
  val constTileSize = 16

  def simple_fold[T:Type:Num](src: Array[T]) = {
    val outerPar = 1 (16 -> 16)
    val innerPar = 1 (16 -> 16)
    val tileSize = constTileSize (constTileSize -> constTileSize)
    val len = src.length; bound(len) = 9216

    val N = ArgIn[Int]
    val out = ArgOut[T]
    setArg(N, len)

    val v1 = DRAM[T](N)
    setMem(v1, src)

    Accel {
      val accum = Reg[T](0.to[T])
      Reduce(accum)(N by tileSize par outerPar){ i =>
        val b1 = SRAM[T](tileSize)
        b1 load v1(i::i+tileSize par 16)
        Reduce(Reg[T](0.to[T]))(tileSize par innerPar){ ii =>
          b1(ii)
        } {_+_}
      } {_+_}
      Pipe { out := accum }
    }

    getArg(out)
  }

  @virtualize
  def main() {
    val len = args(0).to[Int]

    val src = Array.tabulate(len){i => i % 256}
    val result = simple_fold(src)

    val gold = src.reduce{_+_}
    println("expected: " + gold)
    println("result:   " + result)

    val cksum = result == gold
    println("PASS: " + cksum + " (SimpleFold) * Here is an example for how to leave regression comments")
  }
}

object Memcpy2D extends SpatialApp { // Regression (Unit) // Args: none


  val R = 16
  val C = 16

  def memcpy_2d[T:Type:Num](src: Array[T], rows: Int, cols: Int): Array[T] = {
    val tileDim1 = R
    val tileDim2 = C

    val rowsIn = rows
    val colsIn = cols

    val srcFPGA = DRAM[T](rows, cols)
    val dstFPGA = DRAM[T](rows, cols)

    // Transfer data and start accelerator
    setMem(srcFPGA, src)

    Accel {
      Sequential.Foreach(rowsIn by tileDim1, colsIn by tileDim2) { (i,j) =>
        val tile = SRAM[T](tileDim1, tileDim2)
        tile load srcFPGA(i::i+tileDim1, j::j+tileDim2 par 1)
        dstFPGA (i::i+tileDim1, j::j+tileDim2 par 1) store tile
      }
    }
    getMem(dstFPGA)
  }

  @virtualize
  def main() = {
    val rows = R
    val cols = C
    val src = Array.tabulate(rows*cols) { i => i % 256 }

    val dst = memcpy_2d(src, rows, cols)

    printArray(src, "src:")
    printArray(dst, "dst:")

    val cksum = dst.zip(src){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (Memcpy2D)")

  }
}

object IndirectLoad extends SpatialApp { // This hangs with retime on in SPMV_CRS
  @virtualize
  def main() {
    val ids = Array.tabulate(16){i => 32*i}
    val data = Array.tabulate(32*16){i => random[Int](5)}
    val id_dram = DRAM[Int](16)
    val data_dram = DRAM[Int](32*16)
    val result_dram = DRAM[Int](32)
    setMem(id_dram, ids)
    setMem(data_dram, data)
    Accel{
      val id_sram = SRAM[Int](16)
      val data_sram = SRAM[Int](32)
      id_sram load id_dram
      Foreach(8 by 1) {i => 
        val start = id_sram(i)
        val end = id_sram(i+1)
        Parallel{
          Pipe{data_sram load data_dram(start::end)} // Remove pipe when bug #244 is fixed, required for now for retime to pass
        }
        result_dram store data_sram 
      }
    }
    val result = getMem(result_dram)
    val gold = Array.tabulate(32){i => data(ids(7) + i)}
    printArray(result, "result")
    printArray(gold, "gold")
    val cksum = gold.zip(result){_==_}.reduce{_&&_}
    println("PASS: " + cksum + " (IndirectLoad)")
  }
}

object UniqueParallelLoad extends SpatialApp { // Regression (Unit) // Args: none


  val dim0 = 144 //144
  val dim1 = 96 //96
  // val dim0 = 48
  // val dim1 = 16

  def awkwardload[T:Type:Num](src1: Array[T], src2: Array[T]):T = {


    val mat = DRAM[T](dim0, dim1)
    val other = DRAM[T](dim1, dim1)
    val result = ArgOut[T]
    // Transfer data and start accelerator
    setMem(mat, src1)
    setMem(other, src2)

    Accel {
      val s1 = SRAM[T](dim0, dim1)
      val s2 = SRAM[T](dim1, dim1)
      Parallel{
        s1 load mat(0::dim0, 0::dim1)
        s2 load other(0::dim1, 0::dim1)
      }

      val accum = Reg[T](0.to[T])
      Reduce(accum)(dim0 by 1, dim1 by 1) { (i,j) =>
        s1(i,j)
      }{_+_}
      val accum2 = Reg[T](0.to[T])
      Reduce(accum2)(dim1 by 1, dim1 by 1) { (i,j) =>
        s2(i,j)
      }{_+_}
      result := accum.value + accum2.value
    }
    getArg(result)
  }

  @virtualize
  def main() = {
    type T = FixPt[TRUE,_32,_32]
    val srcA = Array.tabulate(dim0) { i => Array.tabulate(dim1){ j => ((j + i) % 8) }}
    val srcB = Array.tabulate(dim1) { i => Array.tabulate(dim1){ j => ((j + i) % 8) }}

    val dst = awkwardload(srcA.flatten, srcB.flatten)

    val goldA = srcA.map{ row => row.map{el => el}.reduce{_+_}}.reduce{_+_}
    val goldB = srcB.map{ row => row.map{el => el}.reduce{_+_}}.reduce{_+_}
    val gold = goldA + goldB

    println("Gold: " + gold)
    println("result: " + dst)
    val cksum = gold == dst
    println("PASS: " + cksum + " (UniqueParallelLoad)")

  }
}


object BlockReduce1D extends SpatialApp { // Regression (Unit) // Args: 1920
  val tileSize = 64
  val p = 1

  @virtualize
  def blockreduce_1d[T:Type:Num](src: Array[T], size: Int) = {
    val sizeIn = ArgIn[Int]
    setArg(sizeIn, size)

    val srcFPGA = DRAM[T](sizeIn)
    val dstFPGA = DRAM[T](tileSize)

    setMem(srcFPGA, src)

    Accel {
      val accum = SRAM[T](tileSize)
      MemReduce(accum)(sizeIn by tileSize par p){ i  =>
        val tile = SRAM[T](tileSize)
        tile load srcFPGA(i::i+tileSize par 16)
        tile
      }{_+_}
      dstFPGA(0::tileSize par 16) store accum
    }
    getMem(dstFPGA)
  }

  @virtualize
  def main() = {
    val size = args(0).to[Int]
    val src = Array.tabulate(size){i => i % 256}

    val dst = blockreduce_1d(src, size)

    val tsArr = Array.tabulate(tileSize){i => i % 256}
    val perArr = Array.tabulate(size/tileSize){i => i}
    val gold = tsArr.map{ i => perArr.map{j => src(i+j*tileSize)}.reduce{_+_}}

    printArray(gold, "src:")
    printArray(dst, "dst:")
    val cksum = dst.zip(gold){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (BlockReduce1D)")

    //    (0 until tileSize) foreach { i => assert(dst(i) == gold(i)) }
  }
}

object UnalignedLd extends SpatialApp { // Regression (Unit) // Args: 100 9
  val N = 19200

  val paddedCols = 1920

  @virtualize
  def unaligned_1d[T:Type:Num](src: Array[T], ii: Int, numCols: Int) = {
    val iters = ArgIn[Int]
    val srcFPGA = DRAM[T](paddedCols)
    val ldSize = ArgIn[Int]
    val acc = ArgOut[T]

    setArg(iters, ii)
    setArg(ldSize, numCols)
    setMem(srcFPGA, src)

    Accel {
      val ldSizeReg = Reg[Int](0.to[Int])
      ldSizeReg := ldSize.value
      val accum = Reduce(Reg[T](0.to[T]))(iters by 1 par 1) { k =>
        val mem = SRAM[T](16)
        mem load srcFPGA(k*ldSizeReg.value::(k+1)*ldSizeReg.value)
        Reduce(Reg[T](0.to[T]))(ldSizeReg.value by 1){i => mem(i) }{_+_}
      }{_+_}
      acc := accum
    }
    getArg(acc)
  } 

  @virtualize
  def main() = {
    type T = FixPt[TRUE,_32,_32]
    // val size = args(0).to[Int]
    val ii = args(0).to[Int]
    val cols = args(1).to[Int]
    val size = paddedCols
    val src = Array.tabulate[T](size) {i => (i % 256).to[T] }

    val dst = unaligned_1d(src, ii, cols)

    val goldArray = Array.tabulate[T](ii*cols){ i => (i % 256).to[T] }
    val gold = goldArray.reduce{_+_}

    printArray(src, "src")
    printArray(goldArray, "gold")

    println("src:" + gold)
    println("dst:" + dst)
    val cksum = gold == dst
    println("PASS: " + cksum + " (UnalignedLd)")

    //    (0 until tileSize) foreach { i => assert(dst(i) == gold(i)) }
  }
}


// Args: 192 384
object BlockReduce2D extends SpatialApp { // Regression (Unit) // Args: 192 384


  val N = 1920
  val tileSize = 16


  @virtualize
  def main() = {
    val numRows = args(0).to[Int]
    val numCols = args(1).to[Int]
    val src = Array.tabulate(numRows) { i => Array.tabulate(numCols) { j => (i*numCols + j)%256 } } // Standard array
    val flatsrc = src.flatten

    val rowsIn = ArgIn[Int]; setArg(rowsIn, numRows)
    val colsIn = ArgIn[Int]; setArg(colsIn, numCols)

    val srcFPGA = DRAM[Int](rowsIn, colsIn)
    val dstFPGA = DRAM[Int](tileSize, tileSize)
    val probe = ArgOut[Int]

    setMem(srcFPGA, src.flatten)

    Accel {
      val accum = SRAM[Int](tileSize,tileSize)
      MemReduce(accum)(rowsIn by tileSize, colsIn by tileSize par 2){ (i,j)  =>
        val tile = SRAM[Int](tileSize,tileSize)
        tile load srcFPGA(i::i+tileSize, j::j+tileSize  par 1)
        tile
      }{_+_}
      probe := accum(tileSize-1, tileSize-1)
      dstFPGA(0::tileSize, 0::tileSize par 1) store accum
    }
    val dst = getMem(dstFPGA)


    val numHorizontal = numRows/tileSize
    val numVertical = numCols/tileSize
    val numBlocks = numHorizontal*numVertical
    // val gold = Array.tabulate(tileSize){i =>
    //   Array.tabulate(tileSize){j =>

    //     flatsrc(i*tileSize*tileSize + j*tileSize) }}.flatten
    // }.reduce{(a,b) => a.zip(b){_+_}}
    val a1 = Array.tabulate(tileSize) { i => i }
    val a2 = Array.tabulate(tileSize) { i => i }
    val a3 = Array.tabulate(numHorizontal) { i => i }
    val a4 = Array.tabulate(numVertical) { i => i }
    val gold = a1.map{i=> a2.map{j => a3.map{ k=> a4.map {l=> 
      flatsrc(i*numCols + j + k*tileSize*tileSize + l*tileSize) }}.flatten.reduce{_+_}
    }}.flatten

    // val first_el = (0 until numVertical).map{ case j => (0 until numHorizontal).map {case i => src.flatten(tileSize*j + tileSize*tileSize*i)}}.flatten.reduce{_+_}
    // val first_collapse_cols = ((numVertical*tileSize)/2)*(numVertical-1)
    // val last_collapse_cols = (( numVertical*tileSize*tileSize*(numHorizontal-1) + (first_collapse_cols + numVertical*tileSize*tileSize*(numHorizontal-1)) ) / 2)*(numVertical-1)
    // val first_collapse_rows = if (numHorizontal == 1) {first_collapse_cols} else { ((first_collapse_cols + last_collapse_cols) / 2) * (numHorizontal-1) }
    // // TODO: Why does DEG crash if I add first_collapse_rows rather???
    // val gold = Array.tabulate(tileSize*tileSize) { i => first_collapse_cols + i*numBlocks }

    printArray(gold, "src:")
    printArray(dst, "dst:")
    println("Probe is " + getArg(probe) + ".  Should equal " + gold(tileSize * tileSize - 1))
    // dst.zip(gold){_==_} foreach {println(_)}
    val cksum = dst.zip(gold){_ == _}.reduce{_&&_} && getArg(probe) == gold(tileSize * tileSize - 1)
    println("PASS: " + cksum + " (BlockReduce2D)")

    //    (0 until tileSize) foreach { i => assert(dst(i) == gold(i)) }
  }
}

object EvilNesting extends SpatialApp {

  def main() {

    val out = ArgOut[Int]
    Accel{
      Pipe {
        Pipe {
          out := Reduce(Reg[Int])(3 by 1, 3 by 1) { (k,l) => k + l}{_+_}
        }
      }
      Pipe {
        Pipe {
          out := Reduce(Reg[Int])(3 by 1, 3 by 1 par 3) { (k,l) => k + l}{_+_}
        }
      }
      Pipe {
        Pipe {
          out := Reduce(Reg[Int])(3 by 1 par 3) {k => 
            Reduce(Reg[Int])(3 by 1 par 3) { l => k + l}{_+_}
          }{_+_}
        }
      }
      Pipe {
        Pipe {
          out := List.tabulate(3){k => List.tabulate(3){l => k + l}}.flatten.reduce{_+_}
        }
      }

      Pipe {
        Foreach(4 by 1 par 4){ j => out := j}
      }
    }

  }
}

object EvilMemory extends SpatialApp {

  def main() {
    val DATA = DRAM[Int](4,16,128,128)

    Accel{
      val THICK = SRAM[Int](4,16,3,3)
      val WIDE = SRAM[Int](2, 128)
      val TALL = SRAM.buffer[Int](128, 6)
      val BIGSQUARE = SRAM[Int](64,64)
      val LILSQUARE = SRAM[Int](16,16)
      val BIGLINE = SRAM[Int](128)
      val LILLINE = SRAM[Int](8)

      // One-time loads
      THICK load DATA(0::4, 0::16, 0::3, 0::3 par 1)
      WIDE load DATA(0,0,0::2, 0::128 par 1)  
      LILSQUARE load DATA(0,0,0::16, 0::16 par 1)
      Parallel{
        BIGSQUARE load DATA(0,0,0::64,0::64 par 1)
        LILLINE load DATA(0,0,0,0::8 par 1)
      }

      Foreach(200 by 1) {i => 
        Parallel{
          BIGLINE load DATA(0,0,0,0::128 par 1)
          LILLINE load DATA(0,0,0,128::136 par 1)
        }
        Foreach(10 by 1) {j => 
          TALL load DATA(0,0,0::128, 0::6 par 1)
          Foreach(128 by 1) {k => WIDE(0,k) = TALL(k,0) + THICK(0,0,0,0) + BIGLINE(0) + LILLINE(0) + BIGSQUARE(0,0) + LILSQUARE(0,0)}
          BIGSQUARE load DATA(0,0,0::64,0::64 par 1)
        }
        Sequential.Foreach(10 by 1) {j => 
          DATA(0,0,0,0::128 par 1) store BIGLINE
          DATA(0,0,0::2,0::128 par 1) store WIDE
        }
        DATA(0,0,0::2,0::128 par 1) store WIDE
      }
      
    }
  }
}

// Args: none
object GatherStore extends SpatialApp { // Regression (Sparse) // Args: none


  val tileSize = 128
  val numAddr = tileSize * 100
  val numData = tileSize * 1000

  val P = param(1)

  @virtualize
  def gatherStore[T:Type:Num](addrs: Array[Int], offchip_data: Array[T]) = {

    val srcAddrs = DRAM[Int](numAddr)
    val gatherData = DRAM[T](numData)
    val denseResult = DRAM[T](numAddr)

    setMem(srcAddrs, addrs)
    setMem(gatherData, offchip_data)

    Accel {
      val addrs = SRAM[Int](tileSize)
      Sequential.Foreach(numAddr by tileSize) { i =>
        val sram = SRAM[T](tileSize)
        addrs load srcAddrs(i::i + tileSize par P)
        sram gather gatherData(addrs par P, tileSize)
        denseResult(i::i+tileSize) store sram
      }
    }

    getMem(denseResult)
  }

  @virtualize
  def main() = {

    val addrs = Array.tabulate(numAddr) { i =>
      // i*2 // for debug
      // TODO: Macro-virtualized winds up being particularly ugly here..
      if      (i == 4)  lift(199)
      else if (i == 6)  lift(numData-2)
      else if (i == 7)  lift(191)
      else if (i == 8)  lift(203)
      else if (i == 9)  lift(381)
      else if (i == 10) lift(numData-97)
      else if (i == 15) lift(97)
      else if (i == 16) lift(11)
      else if (i == 17) lift(99)
      else if (i == 18) lift(245)
      else if (i == 94) lift(3)
      else if (i == 95) lift(1)
      else if (i == 83) lift(101)
      else if (i == 70) lift(203)
      else if (i == 71) lift(numData-1)
      else if (i % 2 == 0) i*2
      else i*2 + numData/2
    }

    val offchip_data = Array.tabulate[Int](numData){ i => i }

    val received = gatherStore(addrs, offchip_data)

    val gold = Array.tabulate(numAddr){ i => offchip_data(addrs(i)) }

    printArray(gold, "gold:")
    printArray(received, "received:")
    val cksum = received.zip(gold){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (GatherStore)")
  }
}

object ScatterGather extends SpatialApp { // Regression (Sparse) // Args: 160


  val tileSize = 32
  // val tileSize = 128
  // val numAddr = tileSize * 10
  // val numData = tileSize * 100

  val P = param(1)

  @virtualize
  def loadScatter[T:Type:Num](addrs: Array[Int], offchip_data: Array[T], numAddr: Int, numData: Int) = {

    val na = ArgIn[Int]
    setArg(na, numAddr)
    val nd = ArgIn[Int]
    setArg(nd, numData)
    // val scatgats_per = ArgIn[Int]
    // setArg(scatgats_per, args(1).to[Int])

    val srcAddrs = DRAM[Int](na)
    val inData = DRAM[T](nd)
    val scatterResult = DRAM[T](nd)

    setMem(srcAddrs, addrs)
    setMem(inData, offchip_data)

    Accel {
      val addrs = SRAM[Int](tileSize)
      Sequential.Foreach(na by tileSize) { i =>
        val sram = SRAM[T](tileSize)
        // val numscats = scatgats_per + random[Int](8) 
        val numscats = tileSize
        addrs load srcAddrs(i::i + numscats par P)
        sram gather inData(addrs par P, numscats)
        scatterResult(addrs par P, numscats) scatter sram
      }
    }

    getMem(scatterResult)
  }

  @virtualize
  def main() = {

//    val addrs = Array.tabulate(numAddr) { i =>
//      // i*2 // for debug
//      // TODO: Macro-virtualized winds up being particularly ugly here..
//      if      (i == 4)  lift(199)
//      else if (i == 6)  lift(numData-2)
//      else if (i == 7)  lift(191)
//      else if (i == 8)  lift(203)
//      else if (i == 9)  lift(381)
//      else if (i == 10) lift(numData-97)
//      else if (i == 15) lift(97)
//      else if (i == 16) lift(11)
//      else if (i == 17) lift(99)
//      else if (i == 18) lift(245)
//      else if (i == 94) lift(3)
//      else if (i == 95) lift(1)
//      else if (i == 83) lift(101)
//      else if (i == 70) lift(203)
//      else if (i == 71) lift(numData-1)
//      else if (i % 2 == 0) i*2
//      else i*2 + numData/2
//    }

    val numAddr = args(0).to[Int]
    val mul = 2
    val numData = numAddr*mul*mul

    val nd = numData
    val na = numAddr
    val addrs = Array.tabulate(na) { i => i * mul }
    val offchip_data = Array.tabulate[Int](nd){ i => i * 10 }

    val received = loadScatter(addrs, offchip_data, na,nd)

    def contains(a: Array[Int], elem: Int) = {
      a.map { e => e == elem }.reduce {_||_}
    }

    def indexOf(a: Array[Int], elem: Int) = {
      val indices = Array.tabulate(a.length.to[Int]) { i => i }
      if (contains(a, elem)) {
        a.zip(indices) { case (e, idx) => if (e == elem) idx else lift(0) }.reduce {_+_}
      } else lift(-1)
    }

    val gold = Array.tabulate(nd) { i =>
//      if (contains(addrs, i)) offchip_data(indexOf(addrs, i)) else lift(0)
      if (contains(addrs, i)) offchip_data(i) else lift(0)
    }

    printArray(offchip_data, "data:")
    printArray(addrs, "addrs:")
    printArray(gold, "gold:")
    printArray(received, "received:")
    val cksum = received.zip(gold){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (ScatterGather)")
  }
}



object SequentialWrites extends SpatialApp { // Regression (Unit) // Args: 7


  val tileSize = 16
  val N = 5

  def sequentialwrites[A:Type:Num](srcData: Array[A], x: A) = {
    val T = param(tileSize)
    val P = param(4)
    val src = DRAM[A](T)
    val dst = DRAM[A](T)
    val xx = ArgIn[A]
    setArg(xx, x)
    setMem(src, srcData)
    Accel {
      val in = SRAM[A](T)
      in load src(0::T par 16)

      MemReduce(in)(1 until (N+1) by 1){ ii =>
        val d = SRAM[A](T)
        Foreach(T by 1){ i => d(i) = xx.value + i.to[A] }
        d
      }{_+_}

      dst(0::T par 16) store in
    }
    getMem(dst)
  }

  @virtualize
  def main() = {
    val x = args(0).to[Int]
    val srcData = Array.tabulate(tileSize){ i => i % 256 }

    val result = sequentialwrites(srcData, x)

    val first = x*N
    val gold = Array.tabulate(tileSize) { i => first + i*N}

    printArray(gold, "gold: ")
    printArray(result, "result: ")
    val cksum = result.zip(gold){_ == _}.reduce{_&&_}
    println("PASS: " + cksum  + " (SequentialWrites)")

  }
}

// Args: None
object ChangingCtrMax extends SpatialApp { // Regression (Unit) // Args: none


  val tileSize = 16
  val N = 5

  def changingctrmax[T:Type:Num](): Array[T] = {
    val result = DRAM[T](16)
    Accel {
      val rMem = SRAM[T](16)
      Sequential.Foreach(16 by 1) { i =>
        val accum = Reduce(0)(i by 1){ j => j }{_+_}
        rMem(i) = accum.value.to[T]
      }
      result(0::16 par 16) store rMem
    }
    getMem(result)
  }

  @virtualize
  def main() = {
    //val i = args(0).to[Int] [Unused]

    val result = changingctrmax[Int]()

    // Use strange if (i==0) b/c iter1: 0 by 1 and iter2: 1 by 1 both reduce to 0
    val gold = Array.tabulate(tileSize) { i => if (i==0) lift(0) else (i-1)*i/2}

    printArray(gold, "gold: ")
    printArray(result, "result: ")
    val cksum = result.zip(gold){_ == _}.reduce{_&&_}
    println("PASS: " + cksum  + " (ChangingCtrMax)")

  }
}


object FifoPushPop extends SpatialApp { // Regression (Unit) // Args: 384


  def fifopushpop(N: Int) = {
    val tileSize = 16 (16 -> 16)

    val size = ArgIn[Int]
    setArg(size, N)
    val acc = ArgOut[Int]

    Accel {
      val f1 = FIFO[Int](tileSize)
      val accum = Reg[Int](0)
      Sequential.Reduce(accum)(size by tileSize){ iter =>
        Foreach(tileSize/2 by 1 par 2){i => f1.enq(iter + i) }
        Foreach(tileSize-1 until (tileSize/2)-1 by -1 par 2){i => f1.enq(iter + i) }
        Reduce(0)(tileSize by 1){ i =>
          f1.deq()
        }{_+_}
      }{_+_}
      acc := accum
    }
    getArg(acc)
  }

  @virtualize
  def main() {
    val arraySize = args(0).to[Int]

    val gold = Array.tabulate(arraySize){ i => i }.reduce{_+_}
    val dst = fifopushpop(arraySize)

    println("gold: " + gold)
    println("dst: " + dst)

    val cksum = dst == gold
    println("PASS: " + cksum + " (FifoPushPop)")
  }
}

// object MultilevelPar extends SpatialApp { 
//
//   val dim = 32
//   val M = dim
//   val N = dim

//   def multilevelpar() = {
//     val result = DRAM[Int](dim)

//     Accel {
//       val a = SRAM[Int](M)
//       val b = SRAM[Int](N)
//       Foreach(M by 1 par 4, N by 1 par 8) { (i,j) =>
//         a(i) = i*2
//         b(j) = j*4
//       }
//       val c = SRAM[Int](dim)
//       Foreach(dim by 1 par 4) { i =>
//         c(i) = a(i) + b(i)
//       }
//       result store c
//     }

//     getMem(result)

//   }

//   @virtualize
//   def main() {

//     val gold = Array.tabulate(dim){ i => 6*i }
//     val ans = multilevelpar()

//     printArray(gold, "Gold:")
//     printArray(ans, "Result:")

//     val cksum = gold.zip(ans){_==_}.reduce{_&&_}
//     println("PASS: " + cksum + " (MultilevelPar)")
//   }
// }


object StreamTest extends SpatialApp {
   override val target = targets.DE1

   @virtualize
   def main() {
     type T = Int

     val frameRows = 16
     val frameCols = 16
     val onboardVideo = target.VideoCamera
     val mem = DRAM[T](frameRows, frameCols)
     val conduit = StreamIn[T](onboardVideo)
     // val avalon = StreamOut()

    // Raw Spatial streaming pipes
    Accel {
      Foreach(*, frameCols by 1) { (_,j) =>
        val fifo1 = FIFO[T](frameCols)
        val fifo2 = FIFO[T](frameCols)
        Stream(frameCols by 1) { j =>
          Pipe {
            fifo1.enq(conduit)
          }
          Pipe {
            val pop = fifo1.deq()
            fifo2.enq(pop)
          }
          Pipe {
            val pop = fifo2.deq()
            // avalon.enq(pop)
          }
        }
      }
    }


  }

}

object BasicFSM extends SpatialApp { // Regression (Unit) // Args: none


  @virtualize
  def main() {
    val dram = DRAM[Int](32)
    val out = ArgOut[Int]
    Accel {
      val bram = SRAM[Int](32)

      FSM[Int]{state => state < 32}{state =>
        bram(state) = state
      }{state => state + 1}

      val y = Reg[Int](0)
      FSM[Boolean,Boolean](true)(x => x){x => 
        y :+= 1
      }{x => mux(y < 5, true, false)}

      out := y
      dram(0::32 par 16) store bram
    }
    val gold = Array.tabulate(32){i => i}

    val result = getMem(dram)
    val argresult = getArg(out)
    printArray(result, "Result")
    printArray(gold, "Gold")
    println("Arg is " + argresult + " =?= 5")
    val cksum = gold.zip(result){_ == _}.reduce{_&&_} && argresult == 5
    // for(i <- 0 until 32) { assert(result(i) == i, "Incorrect at index " + i) }
    println("PASS: " + cksum + " (BasicFSM)")
  }
}

object BasicCondFSM extends SpatialApp { // Regression (Unit) // Args: none


  @virtualize
  def main() {
    val dram = DRAM[Int](32)
    Accel {
      val bram = SRAM[Int](32)
      val reg = Reg[Int](0)
      reg := 16
      FSM[Int]{state => state < 32} { state =>
        if (state < 16) {
          if (state < 8) {
            bram(31 - state) = state // 16:31 [7, 6, ... 0]  
          } else {
            bram(31 - state) = state+1 // 16:31 [16, 15, ... 9]  
          }
        }
        else {
          bram(state - 16) = if (state == 16) 17 else if (state == 17) reg.value else state // Test const, regread, and bound Mux1H
        }
      }{state => state + 1}

      dram(0::32 par 16) store bram
    }
    val result = getMem(dram)
    val gold = Array[Int](17, 16, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 
                          29, 30, 31, 16, 15, 14, 13, 12, 11, 10, 9, 7, 6, 5, 4, 3, 2, 1, 0)
    printArray(result, "Result")
    printArray(gold, "Gold")
    // for (i <- 0 until 32){ assert(result(i) == gold(i)) }
    val cksum = gold.zip(result){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (BasicCondFSM)")
  }
}

object DotProductFSM extends SpatialApp { // Regression (Unit) // Args: none


  @virtualize
  def main() {
    val vectorA = Array.fill(128) {
      random[Int](10)
    }
    val vectorB = Array.fill(128) {
      random[Int](10)
    }
    val vecA = DRAM[Int](128)
    val vecB = DRAM[Int](128)
    val out = ArgOut[Int]
    setMem(vecA, vectorA)
    setMem(vecB, vectorB)
    Accel {
      val outer_accum = Reg[Int](0)
      FSM[Int](i => i < 128) { i =>
        val a = SRAM[Int](16)
        val b = SRAM[Int](16)
        Parallel {
          a load vecA(i :: i + 16 par 16)
          b load vecB(i :: i + 16 par 16)
        }
        outer_accum := outer_accum + Reduce(0)(0 until 16) { i => a(i) * b(i) } {
          _ + _
        }
      } { i => i + 16 }
      Pipe{out := outer_accum}
    }
    val result = getArg(out)
    val gold = vectorA.zip(vectorB){_ * _}.reduce {_ + _}
    println("Expected: " + gold + ", got: " + result)
    // assert(result == gold, "Result (" + result + ") did not equal expected (" + gold + ")")
    val cksum = result == gold
    println("PASS: " + cksum + " (DotProductFSM)")
  }
}

object CtrlEnable extends SpatialApp { // Regression (Unit) // Args: 9


  @virtualize
  def main() {
    val vectorA = Array.fill[Int](128) {
      4 // Please don't change this to random
    }
    val vectorB = Array.fill[Int](128) {
      8 // Please don't change this to random
    }
    val vectorC = Array.fill[Int](128) {
      14 // Please don't change this to random
    }
    val vecA = DRAM[Int](128)
    val vecB = DRAM[Int](128)
    val vecC = DRAM[Int](128)
    val result = DRAM[Int](128)
    val x = ArgIn[Int]
    setArg(x, args(0).to[Int])
    setMem(vecA, vectorA)
    setMem(vecB, vectorB)
    setMem(vecC, vectorC)
    Accel {

      val mem = SRAM[Int](128)

      if (x <= 4.to[Int]) {
        mem load vecA
      } else if (x <= 8.to[Int]) {
        mem load vecB
      } else {
        mem load vecC
      }
    
      result store mem
    }      
    val res = getMem(result)
    val gold = Array.fill(128){ if (args(0).to[Int] <= 4) 4.to[Int] else if (args(0).to[Int] <= 8) 8.to[Int] else 14.to[Int] }
    println("Expected array of : " + gold(0) + ", got array of : " + res(0))
    val cksum = res.zip(gold){_==_}.reduce{_&&_}
    println("PASS: " + cksum + " (CtrlEnable)")
  }
}

object FifoStackFSM extends SpatialApp { // Regression (Unit) // Args: none


  @virtualize
  def main() {
    val size = 128
    val fifo_sum = ArgOut[Int]
    val fifo_sum_almost = ArgOut[Int]
    val fifo_last = ArgOut[Int]
    val stack_sum = ArgOut[Int]
    val stack_sum_almost = ArgOut[Int]
    val stack_last = ArgOut[Int]
    val init = 0
    val fill = 1
    val drain = 2
    val done = 3

    Accel {
      val fifo = FIFO[Int](size)
      val fifo_accum = Reg[Int](0)
      // Using done/empty
      FSM[Int](state => state != done) { state =>
        if (state == init || state == fill) {
          fifo.enq(fifo.numel)
        } else {
          Pipe{            
            val f = fifo.deq()
            fifo_accum := fifo_accum + f
            fifo_last := f
          }
        }
      } { state => mux(state == 0, fill, mux(fifo.full() && state == fill, drain, mux(fifo.empty() && state == drain, done, state))) }
      fifo_sum := fifo_accum

      // Using almostDone/almostEmpty, skips last 2 elements
      val fifo_almost = FIFO[Int](size)
      val fifo_accum_almost = Reg[Int](0)
      FSM[Int](state => state != done) { state =>
        if (state == init || state == fill) {
          fifo_almost.enq(fifo_almost.numel)
        } else {
          Pipe{            
            fifo_accum_almost := fifo_accum_almost + fifo_almost.deq()
          }
        }
      } { state => mux(state == 0, fill, mux(fifo_almost.almostFull() && state == fill, drain, mux(fifo_almost.almostEmpty() && state == drain, done, state))) }
      fifo_sum_almost := fifo_accum_almost

      val stack = FILO[Int](size)
      val stack_accum = Reg[Int](0)
      // Using done/empty
      FSM[Int](state => state != done) { state =>
        if (state == init || state == fill) {
          stack.push(stack.numel)
        } else {
          Pipe{            
            val f = stack.pop()
            stack_accum := stack_accum + f
            stack_last := f
          }
        }
      } { state => mux(state == 0, fill, mux(stack.full() && state == fill, drain, mux(stack.empty() && state == drain, done, state))) }
      stack_sum := stack_accum
      
      // Using almostDone/almostEmpty, skips last element
      val stack_almost = FILO[Int](size)
      val stack_accum_almost = Reg[Int](0)
      FSM[Int](state => state != done) { state =>
        if (state == init || state == fill) {
          stack_almost.push(stack_almost.numel)
        } else {
          Pipe{
            Pipe{ 
              val x = stack_almost.peek
              stack_accum_almost := stack_accum_almost + x
            }
            Pipe{stack_almost.pop()}
          }
        }
      } { state => mux(state == 0, fill, mux(stack_almost.almostFull() && state == fill, drain, mux(stack_almost.almostEmpty() && state == drain, done, state))) }
      stack_sum_almost := stack_accum_almost
      
    }

    val fifo_sum_res = getArg(fifo_sum)
    val fifo_sum_gold = Array.tabulate(size) {i => i}.reduce{_+_}
    val fifo_sum_almost_res = getArg(fifo_sum_almost)
    val fifo_sum_almost_gold = Array.tabulate(size-2) {i => i}.reduce{_+_}
    val fifo_last_res = getArg(fifo_last)
    val fifo_last_gold = size-1
    val stack_sum_res = getArg(stack_sum)
    val stack_sum_gold = Array.tabulate(size) {i => i}.reduce{_+_}
    val stack_last_res = getArg(stack_last)
    val stack_last_gold = 0
    val stack_sum_almost_res = getArg(stack_sum_almost)
    val stack_sum_almost_gold = Array.tabulate(size-1) {i => i}.reduce{_+_}

    println("FIFO: Sum-")
    println("  Expected " + fifo_sum_gold)
    println("       Got " + fifo_sum_res)
    println("FIFO: Alternate Sum-")
    println("  Expected " + fifo_sum_almost_gold)
    println("       Got " + fifo_sum_almost_res)
    println("FIFO: Last out-")
    println("  Expected " + fifo_last_gold)
    println("       Got " + fifo_last_res)
    println("")
    println("Stack: Sum-")
    println("  Expected " + stack_sum_gold)
    println("       Got " + stack_sum_res)
    println("Stack: Alternate Sum-")
    println("  Expected " + stack_sum_almost_gold)
    println("       Got " + stack_sum_almost_res)
    println("Stack: Last out-")
    println("  Expected " + stack_last_gold)
    println("       Got " + stack_last_res)

    val cksum1 = fifo_sum_gold == fifo_sum_res
    val cksum2 = fifo_last_gold == fifo_last_res
    val cksum3 = stack_sum_gold == stack_sum_res
    val cksum4 = stack_last_gold == stack_last_res
    val cksum5 = fifo_sum_almost_gold == fifo_sum_almost_res
    val cksum6 = stack_sum_almost_gold == stack_sum_almost_res
    val cksum = cksum1 && cksum2 && cksum3 && cksum4 && cksum5 && cksum6
    println("PASS: " + cksum + " (FifoStackFSM)")
  }
}

object RetimedFifoBranch extends SpatialApp { // Regression (Unit) // Args: 13 25
  @virtualize
  def main() {
    val num_enq_1 = ArgIn[Int]
    val num_enq_2 = ArgIn[Int]
    val out = ArgOut[Int]

    setArg(num_enq_1, args(0).to[Int])
    setArg(num_enq_2, args(1).to[Int])

    Accel{
      val fifo1 = FIFO[Int](128)
      val fifo2 = FIFO[Int](128)
      Foreach(num_enq_1 by 1) {i => fifo1.enq(i)}
      Foreach(num_enq_2 by 1) {i => fifo2.enq(i)}
      out := Reduce(Reg[Int])(num_enq_1 + num_enq_2 by 1) {i => 
        if (fifo1.empty) {
          fifo2.deq()
        } else {
          fifo1.deq()
        }
      }{_+_}
    }

    val result = getArg(out)
    val gold = Array.tabulate(args(0).to[Int]){i => i}.reduce{_+_} + Array.tabulate(args(1).to[Int])(i => i).reduce{_+_}
    val cksum = gold == result
    println("Got " + result + ", wanted " + gold)
    println("PASS: " + cksum + " (RetimedFifoBranch)")

  }
}


object LaneMaskPar extends SpatialApp { // Regression (Unit) // Args: 13
/* 
  This app is for testing the valids that get passed to each child of a metapipe,
  and before this bug was caught in MD_Grid, the enable for all stages was computed
  based on the current counter value for stage 0
*/

  @virtualize
  def main() {

    val x = ArgIn[Int] // Should NOT be multiple of 2
    val y = ArgOut[Int]
    val ymem = DRAM[Int](4,16)
    setArg(x, args(0).to[Int])

    Accel {
      val s = SRAM[Int](64)
      Foreach(64 by 1){i => s(i) = 0}
      Foreach(4 by 1 par 2){k => 
        val sum = Reduce(Reg[Int](0))(x by 1 par 4) {i => 
          val dummy = Reg.buffer[Int]
          Pipe{dummy := i}
          Foreach(4 by 1){j => dummy := j}
          Pipe{dummy := i}
          dummy.value
        }{_+_}
        s(k) = sum
      }
      y := Reduce(Reg[Int](0))(64 by 1) { i => s(i) }{_+_}

      val outmem = SRAM[Int](4,16)
      Foreach(4 by 1 par 1){ k => 
        val s_accum = SRAM[Int](16)
        MemReduce(s_accum)(2 by 1, x by 1 par 2){(_,i)  => 
          val piece = SRAM[Int](16)
          Foreach(16 by 1){j => piece(j) = i}
          piece
        }{_+_}
        Foreach(16 by 1){ i => outmem(k,i) = s_accum(i)}
      }
      ymem store outmem

    }

    val gold = Array.tabulate(x){i => i}.reduce{_+_} * 4
    println("Wanted " + gold)
    println("Got " + getArg(y))

    val gold_matrix = (0::4,0::16){(_,_) => 2*Array.tabulate(x){i => i}.reduce{_+_} }
    printMatrix(getMatrix(ymem), "Mem:")
    printMatrix(gold_matrix, "Expected:")

    val cksum = gold == getArg(y) && gold_matrix.zip(getMatrix(ymem)){_==_}.reduce{_&&_}
    println("PASS: " + cksum + " (LaneMaskPar)")
  }
}

object FixPtInOutArg extends SpatialApp {  // Regression (Unit) // Args: -1.5
  override val target = Zynq
  type T = FixPt[TRUE,_28,_4]
  
  @virtualize
  def main() {
    // Declare SW-HW interface vals
    val x = ArgIn[T]
    val y = ArgOut[T]
    val N = args(0).to[T]

    // Connect SW vals to HW vals
    setArg(x, N)

    // Create HW accelerator
    Accel {
      y := ((x * 9)-10)/ -1 + 7
    }


    // Extract results from accelerator
    val result = getArg(y)

    // Create validation checks and debug code
    val gold = ((N * 9)-10)/ -1 + 7
    println("expected: " + gold)
    println("result: " + result)

    val cksum = gold == result
    println("PASS: " + cksum + " (FixPtInOutArg)")
  }
}

object MaskedWrite extends SpatialApp {  // Regression (Unit) // Args: 5

  type T = Int

  @virtualize
  def main() {
    // Declare SW-HW interface vals
    val N = 128
    val a = args(0).to[T]
    val y = DRAM[T](N)
    val s = ArgIn[T]

    setArg(s, a)

    Accel {
      val yy = SRAM[T](N)
      Foreach(2*N by 1) { i =>
        if (i < s.value) { yy(i) = 1.to[T]}
      }
      Foreach(2*N by 1) { i =>
        if ((i >= s.value) && (i < N)) {yy(i) = 2.to[T] }
      }
      y(0 :: N par 1) store yy
    }


    // Extract results from accelerator
    val result = getMem(y)

    // Create validation checks and debug code
    val gold = Array.tabulate(N){i => if (i < a) {1} else {2}}
    printArray(gold, "expected: ")
    printArray(result, "got: ")

    val cksum = gold.zip(result){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (MaskedWrite)")
  }
}

object FixPtMem extends SpatialApp {  // Regression (Unit) // Args: 5.25 2.125

  type T = FixPt[TRUE,_32,_32]

  @virtualize
  def main() {
    // Declare SW-HW interface vals
    val N = 128
    val a = args(0).to[T]
    val b = args(1).to[T]
    val TWOPI = 6.28318530717959
    val x_data = Array.tabulate(N){ i => a * i.to[T]}
    val x = DRAM[T](N)
    val y = DRAM[T](N)
    val s = ArgIn[T]

    val expo_dram = DRAM[T](1024)
    val sin_dram = DRAM[T](1024)
    val cos_dram = DRAM[T](1024)
    val sqroot_dram = DRAM[T](512)

    setMem(x, x_data)
    setArg(s, b)

    Accel {
      val xx = SRAM[T](N)
      val yy = SRAM[T](N)
      xx load x(0 :: N par 16)
      Foreach(N by 1) { i => 
        yy(i) = xx(i) * s
      }
      // Test exp_taylor from -4 to 4
      // NOTE: This saturates to 0 if x < -3.5, linear from -3.5 to -1.2, and 5th degree taylor above -1.2
      val expo = SRAM[T](1024)
      Foreach(1024 by 1){ i => 
        val x = (i.as[T] - 512) / 128
        expo(i) = exp_taylor(x)
      }
      // Test sqrt_approx from 0 to 1024
      // NOTE: This does a 3rd degree taylor centered at 1 if x < 2, and then linearizes for every order of magnitude after that
      val sqroot = SRAM[T](512)
      Foreach(512 by 1){ i => 
        sqroot(i) = sqrt_approx(i.as[T]*50 + 5)
      }
      // Test sin and cos from 0 to 2pi
      // NOTE: These do an amazing job if phi is inside +/- pi/2
      val sin = SRAM[T](1024)
      val cos = SRAM[T](1024)
      Foreach(1024 by 1){ i => 
        val phi = TWOPI.to[T]*(i.as[T] / 1024.to[T]) - TWOPI.to[T]/2
        val beyond_left = phi < -TWOPI.to[T]/4
        val beyond_right = phi > TWOPI.to[T]/4
        val phi_shift = mux(beyond_left, phi + TWOPI.to[T]/2, mux(beyond_right, phi - TWOPI.to[T]/2, phi))
        cos(i) = -cos_taylor(phi_shift) * mux(beyond_left || beyond_right, -1.to[T], 1)
        sin(i) = -sin_taylor(phi_shift) * mux(beyond_left || beyond_right, -1.to[T], 1)
      }
      sin_dram store sin
      cos_dram store cos
      expo_dram store expo
      sqroot_dram store sqroot

      y(0 :: N par 16) store yy
    }


    // Extract results from accelerator
    val result = getMem(y)

    // Create validation checks and debug code
    val gold = x_data.map{ dat => dat * b }
    printArray(gold, "expected: ")
    printArray(result, "got: ")

    val expo_gold = Array.tabulate(1024){ i => exp(((i.to[Float])-512)/128) }
    val expo_got = getMem(expo_dram)
    printArray(expo_gold, "e^x gold: ")
    printArray(expo_got, "e^x taylor: ")

    val sin_gold = Array.tabulate(1024){ i => sin(TWOPI.to[Float]*((i.to[Float])/1024.to[Float])) }
    val sin_got = getMem(sin_dram)
    printArray(sin_gold, "sin gold: ")
    printArray(sin_got, "sin taylor: ")

    val cos_gold = Array.tabulate(1024){ i => cos(TWOPI.to[Float]*((i.to[Float])/1024.to[Float])) }
    val cos_got = getMem(cos_dram)
    printArray(cos_gold, "cos gold: ")
    printArray(cos_got, "cos taylor: ")

    val sqroot_gold = Array.tabulate(512){ i => sqrt((i.to[Float])*50 + 5) }
    val sqroot_got = getMem(sqroot_dram)
    printArray(sqroot_gold, "sqroot gold: ")
    printArray(sqroot_got, "sqroot taylor: ")
    // printArray(expo_gold.zip(expo_got){_-_.as[Float]}, "e^x error: ")
    // printArray(expo_gold.zip(expo_got){_.as[T]-_}, "e^x error: ")

    val cksum = gold.zip(result){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (FixPtMem)")
  }
}

object SpecialMath extends SpatialApp { // Regression (Unit) // Args: 0.125 5.625 14 1.875 -3.4375 -5

  type USGN = FixPt[FALSE,_4,_4]
  type SGN = FixPt[TRUE,_4,_4]

  @virtualize
  def main() {
    // Declare SW-HW interface vals
    val a_usgn = args(0).to[USGN] //2.625.to[USGN]
    val b_usgn = args(1).to[USGN] //5.625.to[USGN]
    val c_usgn = args(2).to[USGN] //4094.to[USGN]
    val a_sgn = args(3).to[SGN]
    val b_sgn = args(4).to[SGN]
    val c_sgn = args(5).to[SGN]
    // assert(b_usgn.to[FltPt[_24,_8]] + c_usgn.to[FltPt[_24,_8]] > 15.to[FltPt[_24,_8]], "b_usgn + c_usgn must saturate (false,4,4) FP number")
    // assert(b_sgn.to[FltPt[_24,_8]] + c_sgn.to[FltPt[_24,_8]] < -8.to[FltPt[_24,_8]], "b_sgn + c_sgn must saturate (true,4,4) FP number")
    val A_usgn = ArgIn[USGN]
    val B_usgn = ArgIn[USGN]
    val C_usgn = ArgIn[USGN]
    val A_sgn = ArgIn[SGN]
    val B_sgn = ArgIn[SGN]
    val C_sgn = ArgIn[SGN]
    setArg(A_usgn, a_usgn)
    setArg(B_usgn, b_usgn)
    setArg(C_usgn, c_usgn)
    setArg(A_sgn, a_sgn)
    setArg(B_sgn, b_sgn)
    setArg(C_sgn, c_sgn)
    val N = 256

    // Conditions we will check
    val unbiased_mul_unsigned = DRAM[USGN](N) // 1
    val unbiased_mul_signed = DRAM[SGN](N) // 2
    val satur_add_unsigned = ArgOut[USGN] // 3
    val satur_add_signed = ArgOut[SGN] // 4
    val unbiased_sat_mul_unsigned = ArgOut[USGN] // 5
    val unbiased_lower_sat_mul_signed = ArgOut[SGN] // 6
    val unbiased_upper_sat_mul_signed = ArgOut[SGN] // 6


    Accel {
      val usgn = SRAM[USGN](N)
      val sgn = SRAM[SGN](N)
      Foreach(N by 1) { i => 
        usgn(i) = A_usgn *& B_usgn // Unbiased rounding, mean(yy) should be close to a*b
        sgn(i) = A_sgn *& B_sgn
      }
      unbiased_mul_unsigned store usgn
      unbiased_mul_signed store sgn
      Pipe{ satur_add_unsigned := C_usgn <+> B_usgn}
      Pipe{ satur_add_signed := C_sgn <+> B_sgn}
      Pipe{ unbiased_sat_mul_unsigned := B_usgn <*&> C_usgn}
      Pipe{ unbiased_lower_sat_mul_signed := C_sgn <*&> A_sgn}
      Pipe{ unbiased_upper_sat_mul_signed := C_sgn <*&> (-1.to[SGN]*A_sgn)}
    }


    // Extract results from accelerator
    val unbiased_mul_unsigned_res = getMem(unbiased_mul_unsigned)
    val satur_add_unsigned_res = getArg(satur_add_unsigned)
    val unbiased_mul_signed_res = getMem(unbiased_mul_signed)
    val satur_add_signed_res = getArg(satur_add_signed)
    val unbiased_sat_mul_unsigned_res = getArg(unbiased_sat_mul_unsigned)
    val unbiased_lower_sat_mul_signed_res = getArg(unbiased_lower_sat_mul_signed)
    val unbiased_upper_sat_mul_signed_res = getArg(unbiased_upper_sat_mul_signed)

    // Create validation checks and debug code
    val gold_unbiased_mul_unsigned = (a_usgn * b_usgn).to[FltPt[_24,_8]]
    val gold_mean_unsigned = unbiased_mul_unsigned_res.map{_.to[FltPt[_24,_8]]}.reduce{_+_} / N
    val gold_unbiased_mul_signed = (a_sgn * b_sgn).to[FltPt[_24,_8]]
    val gold_mean_signed = unbiased_mul_signed_res.map{_.to[FltPt[_24,_8]]}.reduce{_+_} / N
    val gold_satur_add_signed = (-8).to[Float]
    val gold_satur_add_unsigned = (15.9375).to[Float]
    val gold_unbiased_sat_mul_unsigned = (15.9375).to[Float]
    val gold_unbiased_lower_sat_mul_signed = (-8).to[Float]
    val gold_unbiased_upper_sat_mul_signed = (7.9375).to[Float]

    // Get cksums
    val margin = scala.math.pow(2,-4).to[FltPt[_24,_8]]
    val cksum1 = (abs(gold_unbiased_mul_unsigned - gold_mean_unsigned).to[FltPt[_24,_8]] < margin) 
    val cksum2 = (abs(gold_unbiased_mul_signed - gold_mean_signed).to[FltPt[_24,_8]] < margin) 
    val cksum3 = satur_add_unsigned_res == gold_satur_add_unsigned.to[USGN]
    val cksum4 = satur_add_signed_res == gold_satur_add_signed.to[SGN]
    val cksum5 = unbiased_sat_mul_unsigned_res == gold_unbiased_sat_mul_unsigned.to[USGN]
    val cksum6 = unbiased_lower_sat_mul_signed_res == gold_unbiased_lower_sat_mul_signed.to[SGN]
    val cksum7 = unbiased_upper_sat_mul_signed_res == gold_unbiased_upper_sat_mul_signed.to[SGN]
    val cksum = cksum1 && cksum2 && cksum3 && cksum4 && cksum5 && cksum6 && cksum7

    // Helpful prints
    println(cksum1 + " Unbiased Rounding Multiplication Unsigned: |" + gold_unbiased_mul_unsigned + " - " + gold_mean_unsigned + "| = " + abs(gold_unbiased_mul_unsigned-gold_mean_unsigned) + " <? " + margin)
    println(cksum2 + " Unbiased Rounding Multiplication Signed: |" + gold_unbiased_mul_signed + " - " + gold_mean_signed + "| = " + abs(gold_unbiased_mul_signed-gold_mean_signed) + " <? " + margin)
    println(cksum3 + " Saturating Addition Unsigned: " + satur_add_unsigned_res + " =?= " + gold_satur_add_unsigned.to[USGN])
    println(cksum4 + " Saturating Addition Signed: " + satur_add_signed_res + " =?= " + gold_satur_add_signed.to[SGN])
    println(cksum5 + " Unbiased Saturating Multiplication Unsigned: " + unbiased_sat_mul_unsigned_res + " =?= " + gold_unbiased_sat_mul_unsigned.to[SGN])
    println(cksum6 + " Unbiased (lower) Saturating Multiplication Signed: " + unbiased_lower_sat_mul_signed_res + " =?= " + gold_unbiased_lower_sat_mul_signed.to[SGN])
    println(cksum6 + " Unbiased (upper) Saturating Multiplication Signed: " + unbiased_upper_sat_mul_signed_res + " =?= " + gold_unbiased_upper_sat_mul_signed.to[SGN])


    println("PASS: " + cksum + " (SpecialMath) * Need to check subtraction and division ")
  }
}


object DiagBanking extends SpatialApp {  // Regression (Unit) // Args: none

  type T = Int

  @virtualize
  def main() {
    // Declare SW-HW interface vals
    val colpar = 8
    val rowpar = 3
    val M = 64
    val N = 32
    val x_data = (0::M, 0::N){(i,j) => (i*N + j).to[T]}
    val x = DRAM[T](M,N)
    val s = ArgOut[T]

    setMem(x, x_data)

    Accel {
      val xx = SRAM[T](M,N)
      xx load x(0 :: M, 0 :: N par colpar)
      s := Reduce(Reg[T](0.to[T]))(N by 1, M by 1 par rowpar) { (j,i) => 
        xx(i,j)
      }{_+_}
    }


    // Extract results from accelerator
    val result = getArg(s)

    // Create validation checks and debug code
    val gold = x_data.reduce{_+_}
    println("Result: (gold) " + gold + " =?= " + result)

    val cksum = gold == result
    println("PASS: " + cksum + " (DiagBanking)")
  }
}

object MultiArgOut extends SpatialApp { 

  type T = Int

  @virtualize
  def main() {
    // Declare SW-HW interface vals
    val a = ArgIn[T]
    val b = ArgIn[T]
    val x = ArgOut[T]
    val y = ArgOut[T]
    val i = args(0).to[T]
    val j = args(1).to[T]
    setArg(a, i)
    setArg(b, j)


    Accel {
      x := a
      y := b
    }


    // Extract results from accelerator
    val xx = getArg(x)
    val yy = getArg(y)

    println("xx = " + xx + ", yy = " + yy)
    val cksum = (xx == i) && (yy == j)
    println("PASS: " + cksum + " (MultiArgOut)")
  }
}

object MultiWriteBuffer extends SpatialApp { // Regression (Unit) // Args: none


  @virtualize
  def main() {
    val R = 16
    val C = 16

    val mem = DRAM[Int](R, C)
    val y = ArgOut[Int]

    Accel {
      val accum = SRAM[Int](R, C)
      MemReduce(accum)(1 until (R+1)) { row =>
        val sram_seq = SRAM.buffer[Int](R, C)
         Foreach(0 until R, 0 until C) { (r, c) =>
            sram_seq(r,c) = 0
         }
         Foreach(0 until C) { col =>
            sram_seq(row-1, col) = 32*(row-1 + col)
         }
         sram_seq
      }  { (sr1, sr2) => sr1 + sr2 }

      mem store accum
    }
    
    val result = getMatrix(mem)
    val gold = (0::R, 0::C){(i,j) => 32*(i+j)}
    printMatrix(gold, "Gold:")
    printMatrix(result, "Result:")

    val cksum = gold.zip(result){_==_}.reduce{_&&_}
    println("PASS: " + cksum + " (MultiWriteBuffer)")
  }
}

object NestedIfs extends SpatialApp {

  @virtualize
  def nestedIfTest(x: Int) = {
    val in = ArgIn[Int]
    val out = ArgOut[Int]
    setArg(in, x)
    Accel {
      val sram = SRAM[Int](3)
      if (in >= 42.to[Int]) {     // if (43 >= 42)
        if (in <= 43.to[Int]) {   // if (43 <= 43)
          sram(in - 41.to[Int]) = 10.to[Int] // sram(2) = 10
        }
      }
      else {
        if (in <= 2.to[Int]){
          sram(in) = 20.to[Int]
        }
      }
      out := sram(2)
    }
    getArg(out)
  }
  @virtualize
  def main() {
    val result = nestedIfTest(43)
    println("result:   " + result)
  }
}

object Tup2Test extends SpatialApp {


  @virtualize
  def foo() : Int = {
    type Tup = Tup2[Int, Int]
    val out = ArgOut[Int]
    val dr = DRAM[Tup](10)
    Accel {
      val s = SRAM[Tup](10)
      s(5) = pack(42, 43)
      dr(0::10) store s

      val s1 = SRAM[Tup](10)
      s1 load dr(0::10)
      out := s1(5)._1 * s1(5)._2
    }
    getArg(out)
  }

  @virtualize
  def main() {
    val result = foo()
    println(result)
  }
}


object CSV1D extends SpatialApp {

  @virtualize
  def main() {
    type T = FixPt[TRUE, _16, _16]
    val tilesize = 16
    val data = loadCSV1D[T]("/remote/regression/data/1d.csv", ",")
    val memsize = ArgIn[Int]
    setArg(memsize, data.length.to[Int])
    val srcmem = DRAM[T](memsize)
    setMem(srcmem, data)
    val result = ArgOut[T]

    Accel {
      val fpgamem = SRAM[T](tilesize)
      result := Reduce(Reg[T](0.to[T]))(memsize.value by tilesize){ r =>
        fpgamem load srcmem(r :: r + tilesize)
        Reduce(Reg[T](0.to[T]))(tilesize by 1) { i =>
          fpgamem(i)
        }{_+_}
      }{_+_}
    }


    val r = getArg(result)

    val gold = data.reduce {_+_}

    writeCSV1D[T](data, "/remote/regression/data/1d_store.csv", ",")
    printArray(data)
    println("Gold sum is " + gold)
    println("Accel sum is " + r)
    val cksum = gold === r
    println("PASS: " + cksum + " (CSV1D)")
  }
}

object CSV2D extends SpatialApp {

  @virtualize
  def main() {
    type T = FixPt[TRUE, _16, _16]
    val rowtile = 2
    val coltile = 16
    val data = loadCSV2D[T]("/remote/regression/data/2d.csv", ",", "\n")
    writeCSV2D[T](data, "/remote/regression/data/2d_store.csv", ",", "\n")
    val memrows = ArgIn[Int]
    val memcols = ArgIn[Int]
    setArg(memrows, data.rows.to[Int])
    setArg(memcols, data.cols.to[Int])
    val srcmem = DRAM[T](memrows, memcols)
    setMem(srcmem, data)
    val result = ArgOut[T]

    println(data.rows + " x " + data.cols + " matrix:")
    printMatrix(data)
    val slice0 = Array.tabulate(memcols){ i => data.apply(0,i)}
    val slice1 = Array.tabulate(memcols){ i => data.apply(1,i)}
    printArray(slice0, "Slice 0")
    printArray(slice1, "Slice 1")

    Accel {
      val fpgamem = SRAM[T](rowtile, coltile)

      result := Reduce(Reg[T](0.to[T]))(memrows.value by rowtile, memcols.value by coltile) { (r, c) =>
        fpgamem load srcmem(r :: r + rowtile, c :: c + coltile)
        Reduce(Reg[T](0.to[T]))(rowtile by 1, coltile by 1) { (i, j) =>
          fpgamem(i, j)
        }{_+_}
      }{_+_}
    }


    val r = getArg(result)

    val gold = data.reduce {_+_}

    println("Gold sum is " + gold)
    println("Accel sum is " + r)
    val cksum = gold === r && gold > 0.to[T]
    println("PASS: " + cksum + " (CSV2D)")
  }
}

object SSV1D extends SpatialApp { // Regression (Unit) // Args: none

  @virtualize
  def main() {
    type T = FixPt[TRUE, _16, _16]
    val tilesize = 16
    val data = loadCSV1D[T]("/remote/regression/data/1d.ssv", " ")
    val memsize = ArgIn[Int]
    setArg(memsize, data.length.to[Int])
    val srcmem = DRAM[T](memsize)
    setMem(srcmem, data)
    val result = ArgOut[T]

    Accel {
      val fpgamem = SRAM[T](tilesize)
      result := Reduce(Reg[T](0.to[T]))(memsize.value by tilesize) { r =>
        fpgamem load srcmem(r :: r + tilesize)
        Reduce(Reg[T](0.to[T]))(-tilesize until 0 by 1) { i =>
          fpgamem(i+tilesize)
        }{_+_}
      }{_+_}
    }


    val r = getArg(result)

    val gold = data.reduce{_+_}

    printArray(data)
    println("Gold sum is " + gold)
    println("Accel sum is " + r)
    val cksum = gold === r
    println("PASS: " + cksum + " (CSV1D)")
  }
}

object SSV2D extends SpatialApp { // Regression (Unit) // Args: none

  @virtualize
  def main() {
    type T = FixPt[TRUE, _16, _16]
    val rowtile = 2
    val coltile = 16
    val data = loadCSV2D[T]("/remote/regression/data/2d.ssv", " ", "\n")
    val memrows = ArgIn[Int]
    val memcols = ArgIn[Int]
    setArg(memrows, data.rows.to[Int])
    setArg(memcols, data.cols.to[Int])
    val srcmem = DRAM[T](memrows, memcols)
    setMem(srcmem, data)
    val result = ArgOut[T]

    println(data.rows + " x " + data.cols + " matrix:")
    printMatrix(data)
    val slice0 = Array.tabulate(memcols){ i => data.apply(0,i)}
    val slice1 = Array.tabulate(memcols){ i => data.apply(1,i)}
    printArray(slice0, "Slice 0")
    printArray(slice1, "Slice 1")

    Accel {
      val fpgamem = SRAM[T](rowtile, coltile)

      result := Reduce(Reg[T](0.to[T]))(memrows.value by rowtile, memcols.value by coltile) { (r, c) =>
        fpgamem load srcmem(r :: r + rowtile, c :: c + coltile)
        Reduce(Reg[T](0.to[T]))(rowtile by 1, coltile by 1) { (i, j) =>
          fpgamem(i, j)
        }{_+_}
      }{_+_}
    }


    val r = getArg(result)

    val gold = data.reduce {_+_}

    println("Gold sum is " + gold)
    println("Accel sum is " + r)
    val cksum = gold === r && gold > 0.to[T]
    println("PASS: " + cksum + " (SSV2D)")
  }
}

// Args: 1920
object OldSimpleFold extends SpatialApp {
  val constTileSize = 96

  @virtualize def simple_fold[T:Type:Num](src: Array[T]) = {
    val outerPar = 1 (16 -> 16)
    val innerPar = 1 (16 -> 16)
    val tileSize = constTileSize (constTileSize -> constTileSize)
    val len = src.length; bound(len) = 9216

    val N = ArgIn[Int]
    val out = ArgOut[T]
    setArg(N, len)

    val v1 = DRAM[T](N)
    setMem(v1, src)

    Accel {
      val accum = Reg[T](0.to[T])
      val local = SRAM[T](1920)
      local load v1
      Reduce(accum)(0 until 1920 par 16){i => local(i) }{_+_}
      /*Reduce(accum)(N by tileSize par outerPar){ i =>
        val b1 = SRAM[T](tileSize)
        b1 load v1(i::i+tileSize par 16)
        val sum = Reduce(Reg[T](0.to[T]))(tileSize par innerPar){ ii =>
          b1(ii)
        } {_+_}
        println("sum: " + sum.value)
        sum
      } {_+_}*/
      Pipe { out := accum }
    }

    getArg(out)
  }

  @virtualize
  def main() {
    val len = args(0).to[Int]

    val src = Array.tabulate(len){i => i % 256 }
    val result = simple_fold(src)

    val gold = src.reduce{_+_}
    println("expected: " + gold)
    println("result:   " + result)

    val cksum = result == gold
    println("PASS: " + cksum + " (SimpleFold)")
    assert(cksum)
  }
}

object BasicBLAS extends SpatialApp { // Regression (Dense) // Args: 0.2 0.8 64 128 96

  // DSE Parameters
  val tileSize  = 16 (16 -> 16 -> 1024)
  val outer_par = 1 (1 -> 1 -> 32)
  val inner_par = 1 (1 -> 1 -> 16)
  val load_par  = 8 (1 -> 1 -> 16)
  val store_par = 8 (1 -> 1 -> 16)

  // gemm and gemmv specific
  val tileSizeN    = 16 (16 -> 16 -> 1024)
  val tileSizeM    = 16 (16 -> 16 -> 1024)
  val tileSizeK    = 16 (16 -> 16 -> 1024)
  val m_inner_par  = 1 (1 -> 1 -> 8)
  val n_inner_par  = 1 (1 -> 1 -> 8)
  val k_inner_par  = 1 (1 -> 1 -> 8)
  val m_outer_par  = 1 (1 -> 1 -> 8)
  val n_outer_par  = 1 (1 -> 1 -> 8)
  val k_outer_par  = 1 (1 -> 1 -> 8)
  val c_reduce_par = 1 (1 -> 1 -> 8)
  val y_reduce_par = 1 (1 -> 1 -> 8)

  @virtualize
  def Dot[T:Type:Num](N: Reg[Int], 
                      X: DRAM1[T], incX: Int,
                      Y: DRAM1[T], incY: Int,
                      res: Reg[T]): Unit = {
    // Loop over whole vectors
    val outer_res = Reduce(Reg[T])(N.value by tileSize par outer_par){i => 
      // Compute elements left in this tile
      val elements = min(tileSize, N.value - i)
      // Create onchip structures
      val x_tile = SRAM[T](tileSize)
      val y_tile = SRAM[T](tileSize)
      // Load local tiles
      x_tile load X(i::i+elements par load_par)
      y_tile load Y(i::i+elements par load_par)
      // Loop over elements in local tiles
      val inner_res = Reduce(Reg[T])(elements by 1 par inner_par){j => 
        x_tile(j) * y_tile(j)
      }{_+_}
      inner_res
    }{_+_}
    res := outer_res

  }

  @virtualize
  def Axpy[T:Type:Num](N: Reg[Int], alpha: T, 
                       X: DRAM1[T], incX: Int,
                       Y: DRAM1[T], incY: Int,
                       res: DRAM1[T]): Unit = {
    // Loop over whole vectors
    Foreach(N.value by tileSize par outer_par){i => 
      // Compute elements left in this tile
      val elements = min(tileSize, N.value - i)
      // Create onchip structures
      val x_tile = SRAM[T](tileSize)
      val y_tile = SRAM[T](tileSize)
      val z_tile = SRAM[T](tileSize)
      // Load local tiles
      x_tile load X(i::i+elements par load_par)
      y_tile load Y(i::i+elements par load_par)
      // Loop over elements in local tiles
      Foreach(elements by 1 par inner_par){j => 
        z_tile(j) = alpha * x_tile(j) + y_tile(j)
      }
      // Store tile to DRAM
      res(i::i+elements par store_par) store z_tile
    }
  }

  @virtualize
  def Gemm[T:Type:Num](M: Reg[Int], N: Reg[Int], K: Reg[Int],
                       alpha: T, 
                       A: DRAM2[T], lda: Int,
                       B: DRAM2[T], ldb: Int,
                       beta: T,
                       C: DRAM2[T], ldc: Int): Unit = {
    Foreach(M.value by tileSizeM par m_outer_par){i =>
      // Compute leftover dim
      val elements_m = min(tileSizeM, M.value - i)
      Foreach(N.value by tileSizeN par n_outer_par){j =>
        // Compute leftover dim
        val elements_n = min(tileSizeN, N.value - j)
        // Create C tile for accumulating
        val c_tile = SRAM[T](tileSizeM, tileSizeN)
        MemReduce(c_tile par c_reduce_par)(K.value by tileSizeK par k_outer_par){l =>
          // Create local C tile
          val c_tile_local = SRAM[T](tileSizeM, tileSizeN)
          // Compute leftover dim
          val elements_k = min(tileSizeK, K.value - l)
          // Generate A and B tiles
          val a_tile = SRAM[T](tileSizeM, tileSizeK)
          val b_tile = SRAM[T](tileSizeK, tileSizeN)
          // Transfer tiles to sram
          Parallel{
            a_tile load A(i::i+elements_m, l::l+elements_k par load_par) 
            b_tile load B(l::l+elements_k, j::j+elements_n par load_par) 
          }
          Foreach(elements_m by 1 par m_inner_par){ii => 
            Foreach(elements_n by 1 par n_inner_par){jj => 
              c_tile_local(ii,jj) = Reduce(Reg[T])(elements_k by 1 par k_inner_par){ll => 
                a_tile(ii,ll) * b_tile(ll,jj)
              }{_+_}
            }
          }
          c_tile_local
        }{_+_}
        C(i::i+elements_m, j::j+elements_n par store_par) store c_tile
      }
    }
  }

  @virtualize
  def Gemv[T:Type:Num](M: Reg[Int], N: Reg[Int],
                       alpha: T, 
                       A: DRAM2[T], lda: Int,
                       X: DRAM1[T], incX: Int,
                       beta: T,
                       Y: DRAM1[T], incY: Int): Unit = {
    Foreach(M.value by tileSizeM par m_outer_par){i =>
      // Compute leftover dim
      val elements_m = min(tileSizeM, M.value - i)
      // Create Y tile
      val y_tile = SRAM[T](tileSizeM)
      MemReduce(y_tile par y_reduce_par)(N.value by tileSizeN par n_outer_par){j =>
        // Compute leftover dim
        val elements_n = min(tileSizeN, N.value - j)
        // Create local Y tile for accumulating
        val y_tile_local = SRAM[T](tileSizeM)
        // Create X tile
        val x_tile = SRAM[T](tileSizeN)
        // Load vector tile
        x_tile load X(j::j+elements_n par load_par)
        // Create A tile
        val a_tile = SRAM[T](tileSizeM, tileSizeN)
        // Load matrix tile
        a_tile load A(i::i+elements_m, j::j+elements_n par load_par)
        Foreach(elements_m by 1 par m_inner_par){ii => 
          y_tile_local(ii) = Reduce(Reg[T])(elements_n by 1 par n_inner_par){jj => 
            a_tile(ii,jj) * x_tile(jj)
          }{_+_}
        }
        y_tile_local
      }{_+_}
      Y(i::i+elements_m par store_par) store y_tile
    }
  }

  @virtualize
  def Ger[T:Type:Num](M: Reg[Int], N: Reg[Int],
                       alpha: T, 
                       X: DRAM1[T], incX: Int,
                       Y: DRAM1[T], incY: Int,
                       A: DRAM2[T], lda: Int): Unit = {
    Foreach(M.value by tileSizeM par m_outer_par){i =>
      // Compute leftover dim
      val elements_m = min(tileSizeM, M.value - i)
      // Create X tile
      val x_tile = SRAM[T](tileSizeM)
      // Load x data into tile
      x_tile load X(i::i+elements_m)
      Foreach(N.value by tileSizeN par n_outer_par){j => 
        // Compute leftover dim
        val elements_n = min(tileSizeN, N.value - j)
        // Create Y and A tiles
        val y_tile = SRAM[T](tileSizeN)
        val a_tile = SRAM[T](tileSizeM, tileSizeN)
        // Load x data into tile
        y_tile load Y(j::j+elements_n)
        Foreach(elements_m by 1 par m_inner_par){ii =>
          Foreach(elements_n by 1 par n_inner_par){jj =>
            a_tile(ii,jj) = x_tile(ii) * y_tile(jj)
          }
        }
        A(i::i+elements_m, j::j+elements_n par store_par) store a_tile
      }
    }
  }

  @virtualize
  def Scal[T:Type:Num](N: Reg[Int], alpha: T, 
                       X: DRAM1[T], incX: Int,
                       Y: DRAM1[T]): Unit = {
    // Loop over whole vectors
    Foreach(N.value by tileSize par outer_par){i => 
      // Compute elements left in this tile
      val elements = min(tileSize, N.value - i)
      // Create onchip structures
      val x_tile = SRAM[T](tileSize)
      val y_tile = SRAM[T](tileSize)
      // Load local tiles
      x_tile load X(i::i+elements par load_par)
      // Loop over elements in local tiles
      Foreach(elements by 1 par inner_par){j => 
        y_tile(j) = alpha * x_tile(j)
      }
      // Store tile to DRAM
      Y(i::i+elements par store_par) store y_tile
    }
  }

  @virtualize
  def Axpby[T:Type:Num](N: Reg[Int],
                       alpha: T, 
                       X: DRAM1[T], incX: Int,
                       beta: T,
                       Y: DRAM1[T], incY: Int,
                       Z: DRAM1[T]): Unit = {
    Scal[T](N, beta, Y, incY, Z)
    Axpy[T](N, alpha, X, incX, Z, incY, Z)
  }

  type T = FixPt[TRUE,_16,_16]

  @virtualize
  def main() {

    // cmd-line args (i.e.- "20 0.5 0.5 64 64 64")
    val alpha  = args(0).to[T]
    val beta   = args(1).to[T]
    val dim_M = args(2).to[Int]
    val dim_N = args(3).to[Int]
    val dim_K = args(4).to[Int]

    // Create random data structures
    val X_data = Array.tabulate(dim_N){i => random[T](3)}
    val ger_X_data = Array.tabulate(dim_M){i => random[T](3)}
    val Y_data = Array.tabulate(dim_N){i => random[T](3)}
    val matrix_A = (0::dim_M,0::dim_K){(i,j) => random[T](3)}
    val matrix_B = (0::dim_K,0::dim_N){(i,j) => random[T](3)}
    val init_matrix_C = (0::dim_M,0::dim_N){(i,j) => 0.to[T]}
    val gemv_X_data = Array.tabulate(dim_K){i => random[T](3)}
    val init_vec_Y = Array.tabulate(dim_M){i => 0.to[T]}

    // Offchip structures
    val a = ArgIn[T]
    val b = ArgIn[T]
    val dot = ArgOut[T]
    val KK = ArgIn[Int]
    val NN = ArgIn[Int]
    val MM = ArgIn[Int]
    setArg(a, alpha)
    setArg(b, beta)
    setArg(MM, dim_M)
    setArg(NN, dim_N)
    setArg(KK, dim_K)
    val X = DRAM[T](NN)
    val ger_X = DRAM[T](MM)
    val Y = DRAM[T](NN)
    val axpby_Z = DRAM[T](NN)
    val A = DRAM[T](MM,KK)
    val B = DRAM[T](KK,NN)
    val C = DRAM[T](MM,NN)
    val gemv_X = DRAM[T](KK)
    val gemv_Y = DRAM[T](MM)
    val ger_A = DRAM[T](MM,NN)
    val scal_Y = DRAM[T](NN)
    val axpy = DRAM[T](NN)
    setMem(X, X_data)
    setMem(ger_X, ger_X_data)
    setMem(Y, Y_data)
    setMem(A, matrix_A)
    setMem(B, matrix_B)
    setMem(C, init_matrix_C)
    setMem(gemv_X, gemv_X_data)
    setMem(gemv_Y, init_vec_Y)

    // Run Accel functions
    Accel{
      // Use defs from spatial's stdlib
      BLAS.Dot[T](NN, X, 1, Y, 1, dot)
      BLAS.Axpy[T](NN, a, X, 1, Y, 1, axpy)
      BLAS.Gemm[T](MM, NN, KK, a, A, A.cols, B, B.cols, b, C, C.cols)
      BLAS.Gemv[T](MM, KK, a, A, A.cols, gemv_X, 1, b, gemv_Y, 1)
      BLAS.Ger[T](MM, NN, a, ger_X, 1, Y, 1, ger_A, ger_A.cols)
      BLAS.Scal[T](NN, a, X, 1, scal_Y)
      BLAS.Axpby[T](NN, a, X, 1, b, Y, 1, axpby_Z)

      // // Use defs in the app
      // Dot[T](NN, X, 1, Y, 1, dot)
      // Axpy[T](NN, a, X, 1, Y, 1, axpy)
      // Gemm[T](MM, NN, KK, a, A, A.cols, B, B.cols, b, C, C.cols)
      // Gemv[T](MM, KK, a, A, A.cols, gemv_X, 1, b, gemv_Y, 1)
      // Ger[T](MM, NN, a, ger_X, 1, Y, 1, ger_A, ger_A.cols)
      // Scal[T](NN, a, X, 1, scal_Y)
      // Axpby[T](NN, a, X, 1, b, Y, 1, axpby_Z)
    }

    // Get results
    val dot_res = getArg(dot)
    val axpy_res = getMem(axpy)
    val gemm_res = getMatrix(C)
    val gemv_res = getMem(gemv_Y)
    val ger_res = getMatrix(ger_A)
    val scal_res = getMem(scal_Y)
    val axpby_res = getMem(axpby_Z)

    // Compute Golds
    val dot_gold = X_data.zip(Y_data){_*_}.reduce{_+_}
    val axpy_gold = X_data.zip(Y_data){case (x,y) => alpha*x+y}
    val gemm_gold = (0::dim_M,0::dim_N){(i,j) => 
      Array.tabulate(dim_K){l => matrix_A(i,l)*matrix_B(l,j)}.reduce{_+_}
    }
    val gemv_gold = Array.tabulate(dim_M){i => 
      Array.tabulate(dim_K){l => matrix_A(i,l)*gemv_X_data(l)}.reduce{_+_}
    }
    val ger_gold = (0::dim_M, 0::dim_N){(i,j) => ger_X_data(i)*Y_data(j)}
    val scal_gold = X_data.map{_*alpha}
    val axpby_gold = X_data.zip(Y_data){case (x,y) => alpha*x+beta*y}

    // Collect cksums
    val margin = 0.25.to[T]
    val dot_cksum = abs(dot_res - dot_gold) < margin
    val axpy_cksum = axpy_res.zip(axpy_gold){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    val gemm_cksum = gemm_res.zip(gemm_gold){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    val gemv_cksum = gemv_res.zip(gemv_gold){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    val ger_cksum = ger_res.zip(ger_gold){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    val scal_cksum = scal_res.zip(scal_gold){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    val axpby_cksum = axpby_res.zip(axpby_gold){(a,b) => abs(a-b) < margin}.reduce{_&&_}
    val cksum = dot_cksum && axpy_cksum && gemm_cksum && gemv_cksum && ger_cksum && scal_cksum && axpby_cksum

    // Print results
    println("Dot Result:")
    println("  " + dot_res + " =?= " + dot_gold)
    println("Axpy Result:")
    printArray(axpy_res, "  Got")
    printArray(axpy_gold, "  Wanted")
    println("Gemm Result:")
    printMatrix(gemm_res, "  Got")
    printMatrix(gemm_gold, "  Wanted")
    println("Gemv Result:")
    printArray(gemv_res, "  Got")
    printArray(gemv_gold, "  Wanted")
    println("Ger Result:")
    printMatrix(ger_res, "  Got")
    printMatrix(ger_gold, "  Wanted")
    println("Scal Result:")
    printArray(scal_res, "  Got")
    printArray(scal_gold, "  Wanted")
    println("Axpby Result:")
    printArray(axpby_res, "  Got")
    printArray(axpby_gold, "  Wanted")
    println("  cksum: " + dot_cksum + " (Dot)")
    println("  cksum: " + axpy_cksum + " (Axpy)")
    println("  cksum: " + gemm_cksum + " (Gemm)")
    println("  cksum: " + gemv_cksum + " (Gemv)")
    println("  cksum: " + ger_cksum + " (Ger)")
    println("  cksum: " + scal_cksum + " (Scal)")
    println("  cksum: " + axpby_cksum + " (Axpby)")

    println("PASS: " + cksum + " (BasicBLAS)")

  }
}

object Convolutions extends SpatialApp { // Regression (Dense) // Args: 16

  // DSE Parameters
  val coltile = 32 // (16 -> 16 -> 1280)

  @virtualize
  def ConvolutionSlide[T:Type:Num](output: DRAM2[T], 
                      input: DRAM2[T],
                      filter: LUT2[T],
                      colstride: scala.Int, rowstride: scala.Int): Unit = {

    val lb = LineBuffer.strided[T](filter.rows, coltile, rowstride)
    val sr = RegFile[T](filter.rows, filter.cols)
    val lineout = SRAM[T](coltile/colstride)
    Foreach(input.rows by rowstride){row =>
      lb load input(row, 0::input.cols) // TODO: load with correct rowstride
      Foreach(input.cols by colstride){j => 
        Foreach(filter.rows by 1 par filter.rows){i => sr(i,*) <<= lb(i,j::j+colstride)}
        lineout(j/colstride) = Reduce(Reg[T](0.to[T]))(filter.rows by 1, filter.cols by 1){(ii,jj) => 
          val img = if ((row.to[Int]+rowstride-1) - (filter.rows - 1 - ii.to[Int]) < 0 || (j.to[Int]+colstride-1) - (filter.cols - 1 - jj.to[Int]) < 0) 0.to[T] else sr(ii,filter.cols - 1 - jj)
          img * filter(ii,jj)
        }{_+_}
        // lineout(j/colstride) = mux(row + (rowstride-1) < filter.rows.to[Int]-1 || j + (colstride-1) < filter.cols.to[Int]-1, 0.to[T], Reduce(Reg[T](0.to[T]))(filter.rows by 1, filter.cols by 1){(ii,jj) => sr(ii,jj) * filter(ii,jj)}{_+_}.value)
      }
      output(row/rowstride, 0::output.cols) store lineout
    }
  }


  // gemm and gemmv specific
  val tileSizeN    = 16 (16 -> 16 -> 1024)
  val tileSizeM    = 16 (16 -> 16 -> 1024)
  val tileSizeK    = 16 (16 -> 16 -> 1024)
  val m_inner_par  = 1 (1 -> 1 -> 8)
  val n_inner_par  = 1 (1 -> 1 -> 8)
  val k_inner_par  = 1 (1 -> 1 -> 8)
  val m_outer_par  = 1 (1 -> 1 -> 8)
  val n_outer_par  = 1 (1 -> 1 -> 8)
  val k_outer_par  = 1 (1 -> 1 -> 8)
  val c_reduce_par = 1 (1 -> 1 -> 8)
  val y_reduce_par = 1 (1 -> 1 -> 8)
  val store_par = 1 (1 -> 1 -> 16)
  val load_par = 1 (1 -> 1 -> 16)

  @virtualize
  def ConvolutionGEMM[T:Type:Num](output: DRAM1[T], 
                      input: DRAM1[T],
                      filter: DRAM2[T]): Unit = {    
    Foreach(filter.rows by tileSizeM par m_outer_par){i =>
      // Compute leftover dim
      val elements_m = min(tileSizeM, filter.rows - i)
      // Create Y tile
      val y_tile = SRAM[T](tileSizeM)
      MemReduce(y_tile par y_reduce_par)(filter.cols by tileSizeN par n_outer_par){j =>
        // Compute leftover dim
        val elements_n = min(tileSizeN, filter.cols - j)
        // Create local Y tile for accumulating
        val y_tile_local = SRAM[T](tileSizeM)
        // Create X tile
        val x_tile = SRAM[T](tileSizeN)
        // Load vector tile
        x_tile load input(j::j+elements_n par load_par)
        // Create A tile
        val a_tile = SRAM[T](tileSizeM, tileSizeN)
        // Load matrix tile
        a_tile load filter(i::i+elements_m, j::j+elements_n par load_par)
        Foreach(elements_m by 1 par m_inner_par){ii => 
          y_tile_local(ii) = Reduce(Reg[T])(elements_n by 1 par n_inner_par){jj => 
            a_tile(ii,jj) * x_tile(jj)
          }{_+_}
        }
        y_tile_local
      }{_+_}
      output(i::i+elements_m par store_par) store y_tile
    }
}

  type T = FixPt[TRUE,_16,_16]

  @virtualize
  def main() {

    // Setup strides
    val row_stride1 = 1
    val col_stride1 = 1
    val row_stride2 = 2
    val col_stride2 = 2
    val row_stride3 = 1
    val col_stride3 = 1
    val row_stride4 = 2
    val col_stride4 = 2
    val row_stride5 = 1
    val col_stride5 = 1
    val row_stride6 = 1
    val col_stride6 = 1
    val row_stride7 = 1
    val col_stride7 = 1
    val D = 3

    // cmd-line args (i.e.- "20 0.5 0.5 64 64 64")
    val in_rows = args(0).to[Int]

    // Create random data structures
    val data1 = (0::in_rows,0::coltile){(i,j) => random[T](2)}
    val filter1_data = Array[T](1,2,1,0,0,0,-1,-2,-1)
    val filter1_list = List[T](1,2,1,0,0,0,-1,-2,-1)
    val img3d = (0::D, 0::in_rows, 0::coltile){(i,j,k) => ((i*10 + j + k)%32).to[T]}
    val filter5_data = List[T](1,0,0,
                              0,0,1,
                              1,0,0,

                              0,1,0,
                              1,1,1,
                              0,1,0,

                              0,0,0,
                              0,0,0,
                              0,1,1
                            )


    // Create toeplitz for filter and padded image
    val data3 = (0::in_rows + (3 - row_stride3), 0::coltile + (3 - col_stride3)){(i,j) => if (i < (3 - row_stride3) || j < (3 - col_stride3)) 0 else data1( i-(3 - row_stride3), j-(3 - col_stride3) )}.flatten
    val data4 = (0::in_rows + (3 - row_stride4), 0::coltile + (3 - col_stride4)){(i,j) => if (i < (3 - row_stride4) || j < (3 - col_stride4)) 0 else data1( i-(3 - row_stride4), j-(3 - col_stride4) )}.flatten
    val filter3_tplz = filter1_data.toeplitz(3,3,in_rows,coltile, row_stride3, col_stride3)
    // println("Expanded filter is " + filter3_tplz.rows + " x " + filter3_tplz.cols)
    // println("Padded data is " + data3.length + " elements long")
    val filter4_tplz = filter1_data.toeplitz(3,3,in_rows,coltile, row_stride4, col_stride4)
    // println("Expanded filter is " + filter4_tplz.rows + " x " + filter4_tplz.cols)
    // println("Padded data is " + data4.length + " elements long")

    // Show inputs
    printMatrix(data1, "Img1")
    // printArray(data3, "Flattened padded img")
    // printMatrix(filter3_tplz, "Toeplitz Filter")
    // printMatrix(filter4_tplz, "Toeplitz Filter, colstride=2")

    // ArgIns
    val M = ArgIn[Int]
    val N = ArgIn[Int]
    val Mds1 = ArgIn[Int]
    val Nds1 = ArgIn[Int]
    val Mds2 = ArgIn[Int]
    val Nds2 = ArgIn[Int]
    val Len3 = ArgIn[Int]
    val Len4 = ArgIn[Int]
    val OutLen3 = ArgIn[Int]
    val Mds3 = ArgIn[Int]
    val Nds3 = ArgIn[Int]
    val Mds4 = ArgIn[Int]
    val Nds4 = ArgIn[Int]
    val OutLen4 = ArgIn[Int]
    setArg(M, in_rows)
    setArg(N, coltile)
    setArg(Mds1, in_rows / row_stride1)
    setArg(Nds1, coltile / col_stride1)
    setArg(Mds2, in_rows / row_stride2)
    setArg(Nds2, coltile / col_stride2)
    setArg(Len3, data3.length)
    setArg(Len4, data4.length)
    setArg(OutLen3, filter3_tplz.rows)
    setArg(OutLen4, filter4_tplz.rows)
    setArg(Mds3, filter3_tplz.rows)
    setArg(Nds3, filter3_tplz.cols)
    setArg(Mds4, filter4_tplz.rows)
    setArg(Nds4, filter4_tplz.cols)

    // Offchip structures
    val image = DRAM[T](M, N)
    val flatimg = DRAM[T](Len3)
    val flatimg4 = DRAM[T](Len4)
    val dram1 = DRAM[T](Mds1, Nds1)
    val dram2 = DRAM[T](Mds2, Nds2)
    val dram3 = DRAM[T](OutLen3)
    val dram4 = DRAM[T](OutLen4)
    val dram5 = DRAM[T](M, N)
    val dram6 = DRAM[T](2, Mds1, Nds1)
    val dram7 = DRAM[T](2, M, N)
    val filter3 = DRAM[T](Mds3, Nds3)
    val filter4 = DRAM[T](Mds4, Nds4)
    val image3d = DRAM[T](D,M,N)

    setMem(image, data1)
    setMem(image3d, img3d)
    setMem(flatimg, data3)
    setMem(flatimg4, data4)
    setMem(filter3, filter3_tplz)
    setMem(filter4, filter4_tplz)

    // Run Accel functions
    Accel{
      val filter = LUT[T](3,3)(filter1_list:_*)
      val filter5 = LUT[T](3,3,3)(filter5_data:_*)
      val filter6 = LUT[T](3,3)(filter1_list.map{_+1}:_*)
      val filter7 = LUT[T](3,3,3)(filter5_data.map{_+1}:_*)

      // Use stdlib defs
      Pipe{Convolution.ConvolutionSlide[T](dram1, image, filter, col_stride1, row_stride1, 16, 16)}
      Pipe{Convolution.ConvolutionSlide[T](dram2, image, filter, col_stride2, row_stride2, 16, 16)}
      Pipe{Convolution.ConvolutionGEMM[T](dram3, flatimg, filter3)}
      Pipe{Convolution.ConvolutionGEMM[T](dram4, flatimg4, filter4)}
      Pipe{Convolution.MCConvolutionSlide(dram5, image3d, filter5, col_stride5, row_stride5, 16, 16, 3)}
      Pipe{Convolution.MFConvolutionSlide[T](dram6, image, List(filter, filter6), col_stride6, row_stride6, 16, 16)}
      Pipe{Convolution.MCMFConvolutionSlide[T](dram7, image3d, List(filter5, filter7), col_stride7, row_stride7, 16, 16, 3)}

      // // Use defs in this app
      // ConvolutionSlide[T](dram1, image, filter, col_stride1, row_stride1)
      // ConvolutionSlide[T](dram2, image, filter, col_stride2, row_stride2)
      // ConvolutionGEMM[T](dram3, flatimg, filter3)
      // ConvolutionGEMM[T](dram4, flatimg, filter4)
    }

    // Get results
    val res1 = getMatrix(dram1)
    val res2 = getMatrix(dram2)
    val res3 = getMem(dram3).reshape(in_rows, coltile)
    val res4 = getMem(dram4).reshape(res2.rows, res2.cols)
    val res5 = getMatrix(dram5)
    val res6 = getTensor3(dram6)
    val res7 = getTensor3(dram7)

    // Compute Golds
    val gold1 = (0::in_rows / row_stride1, 0::coltile / col_stride1){(i,j) => 
      Array.tabulate(3){ii => Array.tabulate(3){jj => 
        val img = if (i*row_stride1-ii < 0 || j*col_stride1-jj < 0) 0 else data1(i*row_stride1-ii,j*col_stride1-jj)
        img * filter1_data((2-ii)*3+(2-jj))
      }}.flatten.reduce{_+_}
    }
    val gold2 = (0::in_rows / row_stride2, 0::coltile / col_stride2){(i,j) => 
      Array.tabulate(3){ii => Array.tabulate(3){jj => 
        val real_i = i*row_stride2-ii+(row_stride2-1)
        val real_j = j*col_stride2-jj+(col_stride2-1)
        val img = if (real_i < 0 || real_j < 0) 0 else data1(real_i,real_j)
        img * filter1_data((2-ii)*3+(2-jj))
      }}.flatten.reduce{_+_}
    }
    val gold3 = gold1
    val gold4 = gold2
    val friendly_filter5 = Array[T](filter5_data:_*)
    val gold5 = (0::M, 0::N){(i,j) => 
      Array.tabulate(D){page => 
        Array.tabulate(3){ii => Array.tabulate(3){jj => 
          val pxl = if (i-ii < 0 || j-jj < 0) 0.to[T] else img3d(page,i-ii,j-jj)
          pxl * friendly_filter5(page*9+(2-ii)*3+(2-jj))
        }}.flatten.reduce{_+_}
      }.reduce{_+_}
    }
    val gold6 = (0::2, 0::in_rows / col_stride6, 0::coltile / col_stride6){(k,i,j) => 
      Array.tabulate(3){ii => Array.tabulate(3){jj => 
        val f = if (k == 0) filter1_data((2-ii)*3+(2-jj)) else filter1_data((2-ii)*3+(2-jj)) + 1
        val img = if (i*row_stride1-ii < 0 || j*col_stride1-jj < 0) 0 else data1(i*row_stride1-ii,j*col_stride1-jj)
        // println("for " + k + "," + i + "," + j + " = " + f + " * " + img)
        img * f
      }}.flatten.reduce{_+_}
    }
    val gold7 = (0::2, 0::M, 0::N){(k,i,j) => 
      Array.tabulate(D){page => 
        Array.tabulate(3){ii => Array.tabulate(3){jj => 
          val pxl = if (i-ii < 0 || j-jj < 0) 0.to[T] else img3d(page,i-ii,j-jj)
          val f = if (k == 0) friendly_filter5(page*9+(2-ii)*3+(2-jj)) else friendly_filter5(page*9+(2-ii)*3+(2-jj)) + 1
          pxl * f
        }}.flatten.reduce{_+_}
      }.reduce{_+_}
    }

    // Collect cksums
    val margin = 0.25.to[T]
    val cksum1 = res1.zip(gold1){_==_}.reduce{_&&_}
    val cksum2 = res2.zip(gold2){_==_}.reduce{_&&_}
    val cksum3 = res3.zip(gold3){_==_}.reduce{_&&_}
    val cksum4 = res4.zip(gold4){_==_}.reduce{_&&_}
    val cksum5 = res5.zip(gold5){_==_}.reduce{_&&_}
    val cksum6 = res6.zip(gold6){_==_}.reduce{_&&_}
    val cksum7 = res7.zip(gold7){_==_}.reduce{_&&_}
    val cksum = cksum1 && cksum2 && cksum3 && cksum4 && cksum5 && cksum6 && cksum7

    // Print results
    println("Conv1 Result: ")
    printMatrix(res1, "  Got")
    printMatrix(gold1, "  Wanted")
    println("Conv2 Result: ")
    printMatrix(res2, "  Got")
    printMatrix(gold2, "  Wanted")
    println("Conv3 Result: ")
    printMatrix(res3, "  Got")
    printMatrix(gold3, "  Wanted")
    println("Conv4 Result: ")
    printMatrix(res4, "  Got")
    printMatrix(gold4, "  Wanted")
    println("Conv5 Result: ")
    printMatrix(res5, "  Got")
    printMatrix(gold5, "  Wanted")
    println("Conv6 Result: ")
    printTensor3(res6, "  Got")
    printTensor3(gold6, "  Wanted")
    println("Conv7 Result: ")
    printTensor3(res7, "  Got")
    printTensor3(gold7, "  Wanted")

    println("  cksum: " + cksum1 + " (Conv1)")
    println("  cksum: " + cksum2 + " (Conv2)")
    println("  cksum: " + cksum3 + " (Conv3)")
    println("  cksum: " + cksum4 + " (Conv4)")
    println("  cksum: " + cksum5 + " (Conv5)")
    println("  cksum: " + cksum6 + " (Conv6)")
    println("  cksum: " + cksum7 + " (Conv7)")

    println("PASS: " + cksum + " (Convolutions)")

  }
}

object PipeMergerTest  extends SpatialApp { // Regression (Unit) // Args: none
  @virtualize
  def main(): Unit = {
    val N = ArgIn[Int]
    setArg(N, args(0).to[Int])
    val mat = (0::16,0::16){(i,j) => i }

    val img = DRAM[Int](16,16)
    setMem(img, mat)

    val res1 = ArgOut[Int]
    val res2 = ArgOut[Int]
    val res3 = ArgOut[Int]
    val res4 = ArgOut[Int]
    val res5 = ArgOut[Int]
    val res6 = ArgOut[Int]

    Accel {
      val sram = SRAM[Int](16,16)
      sram load img
      Pipe{
        Foreach(N by 1){i =>
          res1 := Reduce(Reg[Int])(5 by 1 par 5){i =>
            Reduce(Reg[Int])(5 by 1 par 5){j => 
              sram(i,j) * 3
            }{_+_}
          }{_+_}
        }
      }

      Pipe{
        Foreach(N by 1){i =>
          res2 := Reduce(Reg[Int])(5 by 1, 5 by 1 par 5){(i,j) =>
            sram(i,j) * 3
          }{_+_}
        }
      }

      Pipe{
        Foreach(N by 1){i =>
          res3 := List.tabulate(5){i => List.tabulate(5){j => sram(i,j) * 3}}.flatten.reduce{_+_}
        }
      }

      Pipe{Pipe{Pipe{Pipe{res4 := 5}}}}
      Pipe{Pipe{Pipe{Foreach(5 by 1){i => res5 := 5}}}}

      res6 := 5
    }

    println("y1 = " + getArg(res1))
    println("y2 = " + getArg(res2))
    println("y3 = " + getArg(res3))
    println("y4 = " + getArg(res4))
    println("y5 = " + getArg(res5))
    println("y6 = " + getArg(res6))
  }
}
object SimpleRowStridedConv extends SpatialApp { // Regression (Unit) // Args: none
  @virtualize
  def main(): Unit = {
    val R = 20
    val C = 16

    val mat = (0::R,0::C){(i,j) => i }

    val img = DRAM[Int](R, C)
    val out = DRAM[Int](R/2, C)
    val out2 = DRAM[Int](R/2-2, C)
    setMem(img, mat)

    Accel {
      // Test regular row strided lb
      val lb = LineBuffer.strided[Int](3, C, 2)
      Foreach(R/2 by 1){row =>
        val line = SRAM[Int](C)
        lb load img(row*2::row*2+2, 0::C par 16)
        Foreach(C by 1){col =>
          val conv = Reduce(0)(3 by 1, 3 by 1){(r,c) => if (row - 1 + r < 0) 0 else lb(r, (col + c)%C)}{_+_} / 9
          line(col) = conv
        }
        out(row,0::C) store line
      }

      // Test lb with transient load
      val lb2 = LineBuffer.strided[Int](5, C, 2)
      lb2 load img(0::3, 0::C)
      Foreach(R/2-2 by 1){row =>
        val line = SRAM[Int](C)
        val rowstart = 3 + row*2
        lb2 load img(rowstart::rowstart+2, 0::C)
        Foreach(C by 1){col =>
          val conv = Reduce(0)(5 by 1, 3 by 1){(r,c) => lb2(r, (col + c)%C)}{_+_} / 15
          line(col) = conv
        }
        out2(row,0::C) store line
      }

    }

    val result = getMatrix(out)
    val result2 = getMatrix(out2)

    printMatrix(mat, "Input")
    printMatrix(result, "Result")
    printMatrix(result2, "Result2")
    val gold = (0::R/2, 0::C){(i,j) => 2*i}
    val gold2 = (0::R/2-2, 0::C){(i,j) => 2*i+2}

    val cksum = result.zip(gold){_==_}.reduce{_&&_}
    val cksum2 = result2.zip(gold2){_==_}.reduce{_&&_}
    println("PASS: " + {cksum && cksum2} + " (SimpleRowStridedConv)")
  }
}


