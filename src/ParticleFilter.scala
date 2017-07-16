import spatial.dsl._
import org.virtualized._
import spatial.SpatialCompiler
import spatial.interpreter.Interpreter
import spatial.interpreter.Streams
import spatial.metadata._
import collection.mutable.Map

trait ParticleFilter extends SpatialStream {

  val N: scala.Int                                                    = 1
  val initV: (scala.Double, scala.Double, scala.Double)               = (0.0, 0.0, 0.0)
  val initP: (scala.Double, scala.Double, scala.Double)               = (0.0, 0.0, 0.0)
  val initQ: (scala.Double, scala.Double, scala.Double, scala.Double) = (1.0, 0.0, 0.0, 0.0)
  val initCov = 0.00001

  val initTime: scala.Double  = 0.0
  val covGyro: scala.Double   = 0.01
  val covAcc: scala.Double    = 0.01
  val covViconP: scala.Double = 0.01
  val covViconQ: scala.Double = 0.01

  type Real         = FltPt[_32, _0]
  type Time         = Real
  type Position     = Vec3
  type Velocity     = Vec3
  type Acceleration = Vec3
  type Omega        = Vec3
  type Attitude     = Quat

  @struct case class Quat(r: Real, i: Real, j: Real, k: Real)

  //For matrices, the vecs are vector columns e.g Mat33(Vec3, Vec3, Vec3)
  //where Vec3 is 3x1
  @struct case class Vec3(x: Real, y: Real, z: Real)
  @struct case class Vec6(a: Real, b: Real, c: Real, d: Real, e: Real, f: Real)

  case class Mat33(reg: RegFile2[Real])
  case class Mat63(reg: RegFile2[Real])
  case class Mat36(reg: RegFile2[Real])
  case class Mat66(reg: RegFile2[Real])

  @struct case class IMU(t: Time, a: Acceleration, g: Omega)
  @struct case class POSE(p: Vec3, q: Attitude)
  @struct case class Vicon(t: Time, pose: POSE)
  @struct case class Sigma(a: Vec6, b: Vec6, c: Vec6, d: Vec6, e: Vec6, f: Vec6)      
  @struct case class State(x: Vec6, sig: Sigma)
  //x(v1, v2, v3, p1, p2, p3)
  @struct case class Particle(w: Real, q: Quat, st: State, lastA: Acceleration, lastQ: Quat)

  /* Mat33(
   0, 1, 2,
   3, 4, 5
   6, 7, 8
   )
   */

  def createReg(a: scala.Int, b: scala.Int, elems: Seq[Real]): RegFile2[Real] = {
    val r = RegFile[Real](a, b)
    Pipe {
      List
        .tabulate(a*b)(i => i)
        .foreach(i => r(i/b, i%b) = elems(i))
    }
    r
  }
  def createMat33(elems: Real*): Mat33 =
    Mat33(createReg(3, 3, elems))    

  def createMat36(elems: Real*): Mat36 =
    Mat36(createReg(3, 6, elems))

  def createMat63(elems: Real*): Mat63 =
    Mat63(createReg(6, 3, elems))    

  def createMat66(elems: Real*): Mat66 = 
    Mat66(createReg(6, 6, elems))        

  def add(h: scala.Int, w: scala.Int, from: RegFile2[Real], to: RegFile2[Real]) = {
    Foreach(h by 1, w by 1) { (i, j) =>
      to(i, j) = to(i, j) + from(i, j)
    }
  }

  def add(h: scala.Int, from: RegFile1[Real], to: RegFile1[Real]) = {
    Foreach(h by 1) { i =>
      to(i) = to(i) + from(i)
    }
  }
  
  def zero(h: scala.Int, w: scala.Int, out: RegFile2[Real]) = {
    Foreach(h by 1, w by 1) { (i, j) =>
      out(i, j) = 0
    }
  }

  def mult3(b: scala.Int, cols: RegFile2[Real], rows: RegFile1[Real]): Vec3 = {
    val a: scala.Int = 3
    val sum = RegFile[Real](a)
    Foreach(b by 1) { i => {
      val col = Vec3(cols(0, i), cols(1, i), cols(2, i))
      val row = rows(i)
      val out = (col*row).reg
      add(a, out, sum)
    }}
    Vec3(sum(0), sum(1), sum(2))
  }

  def mult6(b: scala.Int, cols: RegFile2[Real], rows: RegFile1[Real]): Vec6 = {
    val a: scala.Int = 6
    val sum = RegFile[Real](a)
    Foreach(b by 1) { i => {
      val col = Vec6(cols(0, i), cols(1, i), cols(2, i), cols(3, i), cols(4, i), cols(5, i))
      val row = rows(i)
      val out = (col*row).reg
      add(a, out, sum)
    }}
    Vec6(sum(0), sum(1), sum(2), sum(3), sum(4), sum(5))
  }
  
  def mult33(b: scala.Int, cols: RegFile2[Real], rows: RegFile2[Real]): Mat33 = {
    val a: scala.Int = 3
    val c: scala.Int = 3
    val sum = RegFile[Real](a, c)
    Foreach(b by 1) { i => {
      val out = RegFile[Real](a, c)
      val col = Vec3(cols(0, i), cols(1, i), cols(2, i))
      val row = Vec3(rows(i, 0), rows(i, 1), rows(i, 2))
      col.outerProd(row, out)
      add(a, c, out, sum)
    }}
    Mat33(sum)
  }

  def mult36(b: scala.Int, cols: RegFile2[Real], rows: RegFile2[Real]): Mat36 = {
    val a: scala.Int = 3
    val c: scala.Int = 6
    val sum = RegFile[Real](a, c)
    Foreach(b by 1) { i => {
      val out = RegFile[Real](a, c)
      val col = Vec3(cols(0, i), cols(1, i), cols(2, i))
      val row = Vec6(rows(i, 0), rows(i, 1), rows(i, 2), rows(i, 3), rows(i, 4), rows(i, 5))      
      col.outerProd(row, out)
      add(a, c, out, sum)
    }}
    Mat36(sum)
  }

  def mult63(b: scala.Int, cols: RegFile2[Real], rows: RegFile2[Real]): Mat63 = {
    val a: scala.Int = 6
    val c: scala.Int = 3
    val sum = RegFile[Real](a, c)
    Foreach(b by 1) { i => {
      val out = RegFile[Real](a, c)
      val col = Vec6(cols(0, i), cols(1, i), cols(2, i), cols(3, i), cols(4, i), cols(5, i))      
      val row = Vec3(rows(i, 0), rows(i, 1), rows(i, 2))            
      col.outerProd(row, out)
      add(a, c, out, sum)
    }}
    Mat63(sum)
  }
  
  def mult66(b: scala.Int, cols: RegFile2[Real], rows: RegFile2[Real]): Mat66 = {
    val a: scala.Int = 6
    val c: scala.Int = 6
    val sum = RegFile[Real](a, c)
    Foreach(b by 1) { i => {
      val out = RegFile[Real](a, c)
      val col = Vec6(cols(0, i), cols(1, i), cols(2, i), cols(3, i), cols(4, i), cols(5, i))
      val row = Vec6(rows(i, 0), rows(i, 1), rows(i, 2), rows(i, 3), rows(i, 4), rows(i, 5))            
      col.outerProd(row, out)
      add(a, c, out, sum)
    }}
    Mat66(sum)
  }

  def transposeReg(a:scala.Int, b:scala.Int, r: RegFile2[Real]): RegFile2[Real] = {
    val out = RegFile[Real](b, a)
    Foreach(a by 1, b by 1){ (i, j) =>      
      out(j, i) = r(i, j)
    }
    out
  }

  def addRegs(a:scala.Int, b:scala.Int, r1: RegFile2[Real], r2: RegFile2[Real]): RegFile2[Real] = {
    val out = RegFile[Real](b, a)
    Foreach(a by 1, b by 1){ (i, j) =>      
      out(i, j) = r1(i, j) + r2(i, j)
    }
    out
  }

  def subRegs(a:scala.Int, b:scala.Int, r1: RegFile2[Real], r2: RegFile2[Real]): RegFile2[Real] = {
    val out = RegFile[Real](b, a)
    Foreach(a by 1, b by 1){ (i, j) =>      
      out(i, j) = r1(i, j) - r2(i, j)
    }
    out
  }

  def multReg(a:scala.Int, b:scala.Int, r: RegFile2[Real], factor: Real): RegFile2[Real] = {
    Foreach(a by 1, b by 1){ (i, j) =>      
      r(i, j) = r(i, j) * factor
    }
    r
  }
  
  implicit class Mat33Ops(m: Mat33) {

    def apply(y: scala.Int, x: scala.Int) =
      m.reg(y, x)

    def t: Mat33 =
      Mat33(transposeReg(3, 3, m.reg))

    def +(y: Mat33): Mat33 =
      Mat33(addRegs(3, 3, m.reg, y.reg))

    def *(y: Real): Mat33 =
      Mat33(multReg(3, 3, m.reg, y))
    def *(y: Vec3): Vec3 =
      mult3(3, m.reg, y.reg)
    def *(y: Mat33): Mat33 =
      mult33(3, m.reg, y.reg)
    def *(y: Mat36): Mat36 =
      mult36(3, m.reg, y.reg)
  }

  implicit class Mat36Ops(m: Mat36) {
    def apply(y: scala.Int, x: scala.Int) =
      m.reg(y, x)

    def t: Mat63 =
      Mat63(transposeReg(6, 3, m.reg))
    def *(y: Mat66): Mat36 =
      mult36(6, m.reg, y.reg)      
    def *(y: Mat63): Mat33 =
      mult33(3, m.reg, y.reg)   
    def *(y: Vec6): Vec3 =      
      mult3(6, m.reg, y.reg)
  }

  implicit class Mat63Ops(m: Mat63) {

    def apply(y: scala.Int, x: scala.Int) =
      m.reg(y, x)

    def t: Mat36 =
      Mat36(transposeReg(3, 6, m.reg))      
    def *(y: Mat33): Mat63 =
      mult63(3, m.reg, y.reg)             
    def *(y: Mat36): Mat66 =
      mult66(3, m.reg, y.reg)
    def *(y: Vec3): Vec6   =
      mult6(3, m.reg, y.reg)
    
  }
  
  implicit class Mat66Ops(m: Mat66) {

    def apply(y: scala.Int, x: scala.Int) =
      m.reg(y, x)
    
    def t: Mat66 =
      Mat66(transposeReg(6, 6, m.reg))

    def *(y: Vec6): Vec6   =
      mult6(6, m.reg, y.reg)      
    def *(y: Mat66): Mat66 =
      mult66(6, m.reg, y.reg)         
    def *(y: Mat63): Mat63 =
      mult63(6, m.reg, y.reg)               
    def +(y: Mat66): Mat66 =
      Mat66(addRegs(6, 6, m.reg, y.reg))    
    def -(y: Mat66): Mat66 =
      Mat66(subRegs(6, 6, m.reg, y.reg))          
  }


  def createVec3(elems: Real*) = Vec3(elems(0), elems(1), elems(2))
  implicit class Vec3Ops(x: Vec3) {
    def *(y: Real) = Vec3(x.x * y, x.y * y, x.z * y)
    def dot(y: Vec3)      = x.x * y.x + x.x * y.y + x.z * y.z
   

    def outerProd(y: Vec3, out: RegFile2[Real]) = {
      val xr = x.reg
      val yr = y.reg      
      Foreach(9 by 1) { i => 
        out((i / 3), (i % 3)) = xr(i / 3) * yr(i % 3)
      }
    }
    
    def outerProd(y: Vec6, out: RegFile2[Real]) = {
      val xr = x.reg
      val yr = y.reg      
      Foreach(18 by 1) { i => 
        out((i / 6), (i % 6)) = xr(i / 6) * yr(i % 6)
      }
    }
    
    @virtualize def reg = {
      val rg = RegFile[Real](3)
      Pipe {
        List
          .tabulate(3)(i => i)
          .foreach(i => rg(i) = x(i))       
      }
      rg
    }
    def apply(i: scala.Int) = i match {
      case 0 => x.x
      case 1 => x.y
      case 2 => x.z
    }
    def +(y: Real) = Vec3(x.x + y, x.y + y, x.z + y)
    def +(y: Vec3) = Vec3(x.x + y.x, x.y + y.y, x.z + y.z)
    def -(y: Vec3) = Vec3(x.x - y.x, x.y - y.y, x.z - y.z)
  }

  def createVec6(elems: Real*) = Vec6(elems(0), elems(1), elems(2), elems(3), elems(4), elems(5))

  implicit class Vec6Ops(x: Vec6) {
    def vec3a = Vec3(x.a, x.b, x.c)
    def *(y: Real): Vec6 =
      createVec6(List.tabulate(6)(i => x(i) * y): _*)
    def +(y: Vec6): Vec6 =
      createVec6(List.tabulate(6)(i => x(i) + y(i)): _*)
    def -(y: Vec6): Vec6 =
      createVec6(List.tabulate(6)(i => x(i) - y(i)): _*)

    def outerProd(y: Vec6, out: RegFile2[Real]) = {
      val xr = x.reg
      val yr = y.reg
      Foreach(36 by 1) { i => 
        out((i / 6), (i % 6)) = xr(i / 6) * yr(i % 6)
      }
    }

    def outerProd(y: Vec3, out: RegFile2[Real]) = {
      val xr = x.reg
      val yr = y.reg      
      Foreach(18 by 1) { i => 
        out((i / 3), (i % 3)) = xr(i / 3) * yr(i % 3)
      }
    }

    @virtualize def reg = {
      val rg = RegFile[Real](6)
      Pipe {
        List
          .tabulate(6)(i => i)
          .foreach(i => rg(i) = x(i))
      }
      rg
    }
    def apply(i: scala.Int) = i match {
      case 0 => x.a
      case 1 => x.b
      case 2 => x.c
      case 3 => x.c
      case 4 => x.c
      case 5 => x.c
    }

  }

  def fromMat66(x: Mat66): Sigma = {
    def row(i: scala.Int): Vec6 = Vec6(x.reg(i, 0), x.reg(i, 1), x.reg(i, 2), x.reg(i, 3), x.reg(i, 4), x.reg(i, 5))
    Sigma(row(0), row(1), row(2), row(3), row(4), row(5))
  }

  implicit class SigmaOps(sig: Sigma) {
    def toMat66: Mat66 = 
      createMat66(List.tabulate(36)(i => sig(i/6, i%6)):_*)
    def apply(y: scala.Int, x: scala.Int) = (x match {
      case 0 => sig.a
      case 1 => sig.b
      case 2 => sig.c
      case 3 => sig.d
      case 4 => sig.e
      case 5 => sig.f        
    })(y)
  }

  implicit class QuatOps(x: Quat) {
    def *(y: Real)        = Quat(x.r * y, x.i * y, x.j * y, x.k * y)
    def *(y: Quat)        = QuatMult(x, y)
    def dot(y: Quat)      = x.r * y.r + x.i * y.i + x.j * y.j + x.k * y.k
    def rotateBy(q: Quat) = q * x
    def rotate(v: Vec3): Vec3 = {
      val inv = x.inverse
      val nq  = (x * Quat(0.0, v.x, v.y, v.z)) * inv
      Vec3(nq.i, nq.j, nq.k)
    }
    def inverse = QuatInverse(x)
  }

  @virtualize def prog() = {

    val inIMU     = StreamIn[IMU](In1)
    val inV       = StreamIn[Vicon](In2)
    val out       = StreamOut[POSE](Out1)
    val parFactor = N(1 -> N)

    Accel {

      val particles = SRAM[Particle](N)
      val fifoIMU   = FIFO[IMU](100)
      val fifoV     = FIFO[Vicon](100)

      val lastTime = Reg[Time](initTime)
      val lastO    = Reg[Omega](Vec3(0.0, 0.0, 0.0))

      Sequential {

        Foreach(N by 1 par parFactor)(x => {
          val initQuat = Quat(initQ._1, initQ._2, initQ._3, initQ._4)
          Parallel {
            particles(x) = Particle(
              Math.log(1.0 / N),
              initQuat,
              State(
                Vec6(initV._1, initV._2, initV._3, initP._1, initP._2, initP._3),
                Sigma(
                  Vec6(initCov, 0, 0, 0, 0, 0),
                  Vec6(0, initCov, 0, 0, 0, 0),
                  Vec6(0, 0, initCov, 0, 0, 0),
                  Vec6(0, 0, 0, initCov, 0, 0),
                  Vec6(0, 0, 0, 0, initCov, 0),
                  Vec6(0, 0, 0, 0, 0, initCov)
                )
              ),
              Vec3(0.0, 0.0, 0.0),
              initQuat
            )

          }
        })

        Parallel {
          Stream(*)(x => {
            fifoV.enq(inV)
          })

          Stream(*)(x => {
            fifoIMU.enq(inIMU)
          })

          FSM.fsm[Boolean](
            true,
            (x => x),
            (x => {
              if ((fifoV.empty && !fifoIMU.empty) || (!fifoIMU.empty && !fifoV.empty && fifoIMU.peek.t < fifoV.peek.t))
                updateFromIMU(fifoIMU.deq(), lastTime, lastO, particles, parFactor)
              else if (!fifoV.empty)
                updateFromV(fifoV.deq(), lastTime, lastO, particles, parFactor)

              out := averagePOSE(particles, parFactor)
              normWeights(particles, parFactor)
              resample(particles, parFactor)
            }),
            (x => (!fifoV.empty || !fifoIMU.empty)),
            style = SeqPipe
          )

        }
      }
    }
  }

  def rotationMatrix(q: Quat) =
    createMat33(
      1.0 - 2.0 * (q.j ** 2 + q.k ** 2),
      2.0 * (q.i * q.j - q.k * q.r),
      2.0 * (q.i * q.k + q.j * q.r),
      2.0 * (q.i * q.j + q.k * q.r),
      1.0 - 2.0 * (q.i ** 2 + q.k ** 2),
      2.0 * (q.j * q.k - q.i * q.r),
      2.0 * (q.i * q.k - q.j * q.r),
      2.0 * (q.j * q.k + q.i * q.r),
      1.0 - 2.0 * (q.i ** 2 + q.j ** 2)
    )

  @virtualize
  def update(accO: Option[Acceleration],
             vicon: Option[POSE],
             dt: Time,
             lastO: Reg[Omega],
             particles: SRAM1[Particle],
             parFactor: Int) = {
    Foreach(N by 1 par parFactor)(i => {
      val pp = particles(i)
      val nq = sampleAtt(pp.q, lastO, dt)
      val F =
        createMat66(
          1, 0, 0, 0, 0, 0,
          0, 1, 0, 0, 0, 0,
          0, 0, 1, 0, 0, 0,
          dt, 0, 0, 1, 0, 0,
          0, dt, 0, 0, 1, 0,
          0, 0, dt, 0, 0, 1
        )

      val U = Vec6(
        pp.lastA.x * dt,
        pp.lastA.y * dt,
        pp.lastA.z * dt,
        0,
        0,
        0
      )
      val rotMatrix = rotationMatrix(pp.lastQ)
      val covFixAcc = (rotMatrix * rotMatrix.t) * (covAcc * dt * dt)
      val Q = createMat66(
        covFixAcc(0, 0), covFixAcc(0, 1), covFixAcc(0, 2), 0, 0, 0,
        covFixAcc(1, 0), covFixAcc(1,1), covFixAcc(1, 2), 0, 0, 0,
        covFixAcc(2, 0), covFixAcc(2, 1), covFixAcc(2, 2), 0, 0, 0,
        0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0
      )

      val nla        = accO.map(x => nq.rotate(x)).getOrElse(pp.lastA)
      val nlq        = accO.map(x => nq).getOrElse(pp.lastQ)
      val (nx, nsig) = kalmanPredict(pp.st.x, pp.st.sig.toMat66, F, U, Q)
      val np = vicon.map(x => {
        val h: Mat36          = createMat36(1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        val r: Mat33          = createMat33(covViconP, 0, 0, 0, covViconP, 0, 0, 0, covViconP)
        val (nx2, nsig2, lik) = kalmanUpdate(nx, nsig, x.p, h, r)
        val nw                = pp.w + likelihoodPOSE(x, lik._1, nq, lik._2)
        Particle(nw, nq, State(nx, fromMat66(nsig2)), nla, nlq)
      })
      particles(i) = np.getOrElse(Particle(pp.w, nq, State(nx, fromMat66(nsig)), nla, nlq))
    })
  }

  def updateFromIMU(x: IMU, lastTime: Reg[Time], lastO: Reg[Omega], particles: SRAM1[Particle], parFactor: Int) = {
    val dt = x.t - lastTime
    lastTime := x.t
    lastO := x.g
    update(Some(x.a), None, dt, lastO, particles, parFactor)
  }

  def updateFromV(x: Vicon, lastTime: Reg[Time], lastO: Reg[Omega], particles: SRAM1[Particle], parFactor: Int) = {
    val dt = x.t - lastTime
    lastTime := x.t
    update(None, Some(x.pose), dt, lastO, particles, parFactor)
  }

  def likelihoodPOSE(measurement: POSE, expectedPosMeasure: Position, quatState: Quat, covPos: Mat33) = {
    val wPos                = normalLogPdf(measurement.p, expectedPosMeasure, covPos)
    val covViconQMat: Mat33 = createMat33(covViconQ, 0, 0, 0, covViconQ, 0, 0, 0, covViconQ)
    val error               = quatToLocalAngle(measurement.q.rotateBy(quatState.inverse))
    val wQuat               = normalLogPdf(error, Vec3(0, 0, 0), covViconQMat)
    wPos + wQuat
  }

  def sampleAtt(q: Quat, om: Omega, dt: Time): Quat = {
    val withNoise  = gaussianVec(om, covGyro)
    val integrated = withNoise * dt
    val lq         = localAngleToQuat(integrated)
    lq.rotateBy(q)
  }

  def gaussianVec(mean: Vec3, variance: Real) = {
    val g1 = gaussian()
    val g2 = gaussian()
    (Vec3(g1._1, g1._2, g2._1) * sqrt(variance)) + mean
  }

  //Box-Muller
  //http://www.design.caltech.edu/erik/Misc/Gaussian.html
  @virtualize def gaussian() = {

    val x1 = Reg[Real]
    val x2 = Reg[Real]
    val w  = Reg[Real]
    val w2 = Reg[Real]

    FSM.fsm[Boolean]((true), (x => x), (x => {
      x1 := 2.0 * random[Real](1.0) - 1.0
      x2 := 2.0 * random[Real](1.0) - 1.0
      w := (x1 * x1 + x2 * x2)
    }), (x => w.value >= 1.0), style = SeqPipe)

    w2 := sqrt((-2.0 * log(w.value)) / w)

    val y1 = x1 * w2;
    val y2 = x2 * w2;

    (y1, y2)
  }

  def QuatMult(q1: Quat, q2: Quat) = {
    Quat(
      q1.r * q2.r - q1.i * q2.i - q1.j * q2.j - q1.k * q2.k,
      q1.r * q2.i + q1.i * q2.r + q1.j * q2.k - q1.k * q2.j,
      q1.r * q2.j - q1.i * q2.k + q1.j * q2.r + q1.k * q2.i,
      q1.r * q2.k + q1.i * q2.j - q1.j * q2.i + q1.k * q2.r
    )
  }

  def QuatInverse(q: Quat) = {
    val n = q.r * q.r + q.i * q.i + q.j * q.j + q.k * q.k
    Quat(q.r, -q.i, -q.j, q.j) * (1 / n)
  }

  def normWeights(particles: SRAM1[Particle], parFactor: Int) = {
    val totalWeight = Reg[Real](0)
    Reduce(totalWeight)(N by 1 par parFactor)(i => particles(i).w)(_+_)
    Foreach(N by 1 par parFactor)(i => {
      val p = particles(i)
      particles(i) = Particle(p.w/totalWeight, p.q, p.st, p.lastA, p.lastQ)
    })    
  }

  @virtualize def resample(particles: SRAM1[Particle], parFactor: Int) = {

    val weights = SRAM[Real](N)
    val out = SRAM[Particle](N)

    val u = random[Real](1.0)

    Foreach(N by 1 par parFactor)(i => {
      if (i == 0)
        weights(i) = particles(i).w
      else
        weights(i) = weights(i-1) + particles(i).w
    })

    val k = Reg[Int](0)
    Foreach(N by 1 par parFactor)(i => {
      val b = weights(k)*N < i.to[Real] +u
      FSM[Boolean, Boolean](b)(x => x)(x =>
        k := k + 1)(x => weights(i)*N < i.to[Real]+u)

      out(i) = particles(k)
    })

    Foreach(N by 1 par parFactor)(i => {
      val p = out(i)
      particles(i) = Particle(log(1.0/N), p.q, p.st, p.lastA, p.lastQ)
    })

  }

  def normalLogPdf(measurement: Vec3, state: Vec3, cov: Mat33): Real = {
    val e = (measurement-state)
    val a = (1.0/2)*log(det(cov))
    val b = e.dot(inv(cov)*e)
    val pi2:Real = PI*2.0
    val c = log(pi2)*3.0
    a + b - c
  }

  def norm(v: Vec3) =
    v.x * v.x + v.y * v.y + v.z * v.z

  def localAngleToQuat(v: Vec3): Quat = {
    val n    = norm(v)
    val l    = n / 2
    val sl   = sin(l)
    val nrot = v * (l / n)
    Quat(cos(l), nrot.x, nrot.y, nrot.z)
  }

  def quatToLocalAngle(q: Quat): Vec3 = {
    val n = acos(q.r) * 2
    val s = n / sin(n / 2)
    Vec3(q.i * s, q.j * s, q.k * s)
  }

  def kalmanPredict(xp: Vec6, sigp: Mat66, f: Mat66, u: Vec6, q: Mat66) = {
    val xm   = f * xp + u
    val sigm = (f * sigp * f.t) + q
    (xm, sigm)
  }

  def det(a: Mat33): Real = {
    val (a11, a12, a13) = (a(0, 0), a(0, 1), a(0, 2))
    val (a21, a22, a23) = (a(1, 0), a(1, 1), a(1, 2))
    val (a31, a32, a33) = (a(2, 0), a(2, 1), a(2, 2))
    a11 * (a33 * a22 - a32 * a23) - a21 * (a33 * a12 - a32 * a13) + a31 * (a23 * a12 - a22 * a13)
  }

  def inv(a: Mat33): Mat33 = {
    val (a11, a12, a13) = (a(0, 0), a(0, 1), a(0, 2))
    val (a21, a22, a23) = (a(1, 0), a(1, 1), a(1, 2))
    val (a31, a32, a33) = (a(2, 0), a(2, 1), a(2, 2))

    val A = createMat33(
      a33*a22-a32*a23, -(a33*a12-a32*a13), a23*a12-a22*a13,
      -(a33*a21-a31*a23), a33*a11-a31*a13, -(a23*a11-a21*a13),
      a32*a21-a31*a22, -(a32*a11-a31*a12), a22*a11-a21*a12
    )
  
    A * (1 / det(a))
  }

  def kalmanUpdate(xm: Vec6, sigm: Mat66, z: Vec3, h: Mat36, r: Mat33) = {
    val s: Mat33   = (h * sigm * h.t) + r
    val za: Vec3   = h * xm
    val k: Mat63   = sigm * h.t * inv(s)
    val sig: Mat66 = sigm - (k * s * k.t)
    val x: Vec6    = xm + (k * (z - za))
    (x, sig, (za, s))
  }

  @virtualize def averagePOSE(particles: SRAM1[Particle], parFactor: Int): POSE = {
    val firstQ = particles(0).q
    val accumP = Reg[Vec3](Vec3(0, 0, 0))
    val accumQ = Reg[Quat](Quat(0, 0, 0, 0))
    val pos = Reduce(accumP)(N by 1 par parFactor)(i => {
      val p = particles(i)
      p.st.x.vec3a * p.w
    })(_ + _)
    val q = Reduce(accumQ)(N by 1 par parFactor)(i => {
      val p = particles(i)
      if (firstQ.dot(p.q) > 0.0)
        p.q * exp(p.w)
      else
        p.q * (exp(-p.w))
    })(_ + _)

    POSE(pos, q)
  }

}

object ParticleFilterInterpreter extends ParticleFilter with SpatialStreamInterpreter {

  val outs = List(Out1)

  val inputs = collection.immutable.Map[Bus, List[MetaAny[_]]](
    (In1 -> List[Real](3f, 4f, 2f, 6f).map(x => IMU(x / 10, Vec3(x, x, x), Vec3(x / 100, x / 100, x / 100)))),
    (In2 -> List[Real]().map(x => Quat(x, x, x, x)))
  )

}

object ParticleFilterCompiler extends ParticleFilter with SpatialApp {
  def main() =
    prog()
}
