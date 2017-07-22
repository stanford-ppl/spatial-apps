import spatial.dsl._
import org.virtualized._
import spatial.SpatialCompiler
import spatial.interpreter._
import spatial.metadata._
import collection.mutable.Map

trait ParticleFilter extends SpatialStream {

  val N: scala.Int                                                    = 10
  val initV: (scala.Double, scala.Double, scala.Double)               = (0.0, 0.0, 0.0)
  val initP: (scala.Double, scala.Double, scala.Double)               = (0.0, 0.0, 0.0)
  val initQ: (scala.Double, scala.Double, scala.Double, scala.Double) = (1.0, 0.0, 0.0, 0.0)
  val initCov = 0.00001

  val initTime: scala.Double  = 0.0
  val covGyro: scala.Double   = 0.01
  val covAcc: scala.Double    = 0.01
  val covSViconP: scala.Double = 0.01
  val covSViconQ: scala.Double = 0.01

  type SReal         = Float
  type STime         = SReal
  type SPosition     = SVec3
  type SVelocity     = SVec3
  type SAcceleration = SVec3
  type SOmega        = SVec3
  type SAttitude     = SQuat

  @struct case class SQuat(r: SReal, i: SReal, j: SReal, k: SReal)

  //For matrices, the vecs are vector columns e.g Mat33(SVec3, SVec3, SVec3)
  //where SVec3 is 3x1
  @struct case class SVec3(x: SReal, y: SReal, z: SReal)
  @struct case class SVec6(a: SReal, b: SReal, c: SReal, d: SReal, e: SReal, f: SReal)

  case class Mat33(reg: RegFile2[SReal])
  case class Mat63(reg: RegFile2[SReal])
  case class Mat36(reg: RegFile2[SReal])
  case class Mat66(reg: RegFile2[SReal])

  @struct case class SIMU(t: STime, a: SAcceleration, g: SOmega)
  @struct case class SPOSE(p: SVec3, q: SAttitude)
  @struct case class SVicon(t: STime, pose: SPOSE)
  @struct case class Sigma(a: SVec6, b: SVec6, c: SVec6, d: SVec6, e: SVec6, f: SVec6)      
  @struct case class State(x: SVec6, sig: Sigma)
  //x(v1, v2, v3, p1, p2, p3)
  @struct case class Particle(w: SReal, q: SQuat, st: State, lastA: SAcceleration, lastQ: SQuat)

  /* Mat33(
   0, 1, 2,
   3, 4, 5
   6, 7, 8
   )
   */
  def createReg(a: scala.Int, b: scala.Int, elems: Seq[SReal]): RegFile2[SReal] = {
    val r = RegFile[SReal](a, b)
    Pipe {
      List
        .tabulate(a*b)(i => i)
        .foreach(i => Pipe { r(i/b, i%b) = elems(i) })
    }
    r
  }
  def createMat33(elems: SReal*): Mat33 =
    Mat33(createReg(3, 3, elems))    

  def createMat36(elems: SReal*): Mat36 =
    Mat36(createReg(3, 6, elems))

  def createMat63(elems: SReal*): Mat63 =
    Mat63(createReg(6, 3, elems))    

  def createMat66(elems: SReal*): Mat66 = 
    Mat66(createReg(6, 6, elems))        

  def add(h: scala.Int, w: scala.Int, from: RegFile2[SReal], to: RegFile2[SReal]) = {
    Foreach(h by 1, w by 1) { (i, j) =>
      to(i, j) = to(i, j) + from(i, j)
    }
  }

  def add(h: scala.Int, from: RegFile1[SReal], to: RegFile1[SReal]) = {
    Foreach(h by 1) { i =>
      to(i) = to(i) + from(i)
    }
  }
  
  def zero(h: scala.Int, w: scala.Int, out: RegFile2[SReal]) = {
    Foreach(h by 1, w by 1) { (i, j) =>
      out(i, j) = 0
    }
  }

  def zero(h: scala.Int, out: RegFile1[SReal]) = {
    Foreach(h by 1) { i =>
      out(i) = 0
    }
  }
  
  def mult3(b: scala.Int, cols: RegFile2[SReal], rows: RegFile1[SReal]): SVec3 = {
    val a: scala.Int = 3
    val sum = RegFile[SReal](a)
    zero(a, sum)
    Foreach(b by 1) { i => {
      val col = SVec3(cols(0, i), cols(1, i), cols(2, i))
      val row = rows(i)
      val out = (col*row).reg
      add(a, out, sum)
    }}
    SVec3(sum(0), sum(1), sum(2))
  }

  def mult6(b: scala.Int, cols: RegFile2[SReal], rows: RegFile1[SReal]): SVec6 = {
    val a: scala.Int = 6
    val sum = RegFile[SReal](a)
    zero(a, sum)    
    Foreach(b by 1) { i => {
      val col = SVec6(cols(0, i), cols(1, i), cols(2, i), cols(3, i), cols(4, i), cols(5, i))
      val row = rows(i)
      val out = (col*row).reg
      add(a, out, sum)
    }}
    SVec6(sum(0), sum(1), sum(2), sum(3), sum(4), sum(5))
  }
  
  def mult33(b: scala.Int, cols: RegFile2[SReal], rows: RegFile2[SReal]): Mat33 = {
    val a: scala.Int = 3
    val c: scala.Int = 3
    val sum = RegFile[SReal](a, c)
    zero(a, c, sum)
    Foreach(b by 1) { i => {
      val out = RegFile[SReal](a, c)
      val col = SVec3(cols(0, i), cols(1, i), cols(2, i))
      val row = SVec3(rows(i, 0), rows(i, 1), rows(i, 2))
      col.outerProd(row, out)
      add(a, c, out, sum)
    }}
    Mat33(sum)
  }

  def mult36(b: scala.Int, cols: RegFile2[SReal], rows: RegFile2[SReal]): Mat36 = {
    val a: scala.Int = 3
    val c: scala.Int = 6
    val sum = RegFile[SReal](a, c)
    zero(a, c, sum)    
    Foreach(b by 1) { i => {
      val out = RegFile[SReal](a, c)
      val col = SVec3(cols(0, i), cols(1, i), cols(2, i))
      val row = SVec6(rows(i, 0), rows(i, 1), rows(i, 2), rows(i, 3), rows(i, 4), rows(i, 5))      
      col.outerProd(row, out)
      add(a, c, out, sum)
    }}
    Mat36(sum)
  }

  def mult63(b: scala.Int, cols: RegFile2[SReal], rows: RegFile2[SReal]): Mat63 = {
    val a: scala.Int = 6
    val c: scala.Int = 3
    val sum = RegFile[SReal](a, c) 
    zero(a, c, sum)   
    Foreach(b by 1) { i => {
      val out = RegFile[SReal](a, c)
      val col = SVec6(cols(0, i), cols(1, i), cols(2, i), cols(3, i), cols(4, i), cols(5, i))      
      val row = SVec3(rows(i, 0), rows(i, 1), rows(i, 2))            
      col.outerProd(row, out)
      add(a, c, out, sum)
    }}
    Mat63(sum)
  }
  
  def mult66(b: scala.Int, cols: RegFile2[SReal], rows: RegFile2[SReal]): Mat66 = {
    val a: scala.Int = 6
    val c: scala.Int = 6
    val sum = RegFile[SReal](a, c)
    zero(a, c, sum)    
    Foreach(b by 1) { i => {
      val out = RegFile[SReal](a, c)
      val col = SVec6(cols(0, i), cols(1, i), cols(2, i), cols(3, i), cols(4, i), cols(5, i))
      val row = SVec6(rows(i, 0), rows(i, 1), rows(i, 2), rows(i, 3), rows(i, 4), rows(i, 5))            
      col.outerProd(row, out)
      add(a, c, out, sum)
    }}
    Mat66(sum)
  }

  def transposeReg(a:scala.Int, b:scala.Int, r: RegFile2[SReal]): RegFile2[SReal] = {
    val out = RegFile[SReal](b, a)    
    Foreach(a by 1, b by 1){ (i, j) =>      
      out(j, i) = r(i, j)
    }
    out
  }

  def addRegs(a:scala.Int, b:scala.Int, r1: RegFile2[SReal], r2: RegFile2[SReal]): RegFile2[SReal] = {
    val out = RegFile[SReal](a, b)
    Foreach(a by 1, b by 1){ (i, j) =>      
      out(i, j) = r1(i, j) + r2(i, j)
    }
    out
  }

  def subRegs(a:scala.Int, b:scala.Int, r1: RegFile2[SReal], r2: RegFile2[SReal]): RegFile2[SReal] = {
    val out = RegFile[SReal](a, b)
    Foreach(a by 1, b by 1){ (i, j) =>      
      out(i, j) = r1(i, j) - r2(i, j)
    }
    out
  }

  def multReg(a:scala.Int, b:scala.Int, r: RegFile2[SReal], factor: SReal): RegFile2[SReal] = {
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

    def *(y: SReal): Mat33 =
      Mat33(multReg(3, 3, m.reg, y))
    def *(y: SVec3): SVec3 =
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
      Mat63(transposeReg(3, 6, m.reg))
    def *(y: Mat66): Mat36 =
      mult36(6, m.reg, y.reg)      
    def *(y: Mat63): Mat33 =
      mult33(3, m.reg, y.reg)   
    def *(y: SVec6): SVec3 =      
      mult3(6, m.reg, y.reg)
  }

  implicit class Mat63Ops(m: Mat63) {

    def apply(y: scala.Int, x: scala.Int) =
      m.reg(y, x)

    def t: Mat36 =
      Mat36(transposeReg(6, 3, m.reg))      
    def *(y: Mat33): Mat63 =
      mult63(3, m.reg, y.reg)             
    def *(y: Mat36): Mat66 =
      mult66(3, m.reg, y.reg)
    def *(y: SVec3): SVec6   =
      mult6(3, m.reg, y.reg)
    
  }
  
  implicit class Mat66Ops(m: Mat66) {

    def apply(y: scala.Int, x: scala.Int) =
      m.reg(y, x)
    
    def t: Mat66 =
      Mat66(transposeReg(6, 6, m.reg))

    def *(y: SVec6): SVec6   =
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


  def createSVec3(elems: SReal*) = SVec3(elems(0), elems(1), elems(2))
  implicit class SVec3Ops(x: SVec3) {
    def *(y: SReal) = SVec3(x.x * y, x.y * y, x.z * y)
    def dot(y: SVec3)      = x.x * y.x + x.y * y.y + x.z * y.z
   

    def outerProd(y: SVec3, out: RegFile2[SReal]) = {
      val xr = x.reg
      val yr = y.reg      
      Foreach(9 by 1) { i => 
        out((i / 3), (i % 3)) = xr(i / 3) * yr(i % 3)
      }
    }
    
    def outerProd(y: SVec6, out: RegFile2[SReal]) = {
      val xr = x.reg
      val yr = y.reg      
      Foreach(18 by 1) { i => 
        out((i / 6), (i % 6)) = xr(i / 6) * yr(i % 6)
      }
    }
    
    @virtualize def reg = {
      val rg = RegFile[SReal](3)
      Pipe {
        List
          .tabulate(3)(i => i)
          .foreach(i => Pipe { rg(i) = x(i) })
      }
      rg
    }
    def apply(i: scala.Int) = i match {
      case 0 => x.x
      case 1 => x.y
      case 2 => x.z
    }
    def +(y: SReal) = SVec3(x.x + y, x.y + y, x.z + y)
    def +(y: SVec3) = SVec3(x.x + y.x, x.y + y.y, x.z + y.z)
    def -(y: SVec3) = SVec3(x.x - y.x, x.y - y.y, x.z - y.z)
  }

  def createSVec6(elems: SReal*) = SVec6(elems(0), elems(1), elems(2), elems(3), elems(4), elems(5))

  implicit class SVec6Ops(x: SVec6) {
    def vec3a = SVec3(x.a, x.b, x.c)
    def *(y: SReal): SVec6 =
      createSVec6(List.tabulate(6)(i => x(i) * y): _*)
    def +(y: SVec6): SVec6 =
      createSVec6(List.tabulate(6)(i => x(i) + y(i)): _*)
    def -(y: SVec6): SVec6 =
      createSVec6(List.tabulate(6)(i => x(i) - y(i)): _*)

    def outerProd(y: SVec6, out: RegFile2[SReal]) = {
      val xr = x.reg
      val yr = y.reg
      Foreach(36 by 1) { i => 
        out((i / 6), (i % 6)) = xr(i / 6) * yr(i % 6)
      }
    }

    def outerProd(y: SVec3, out: RegFile2[SReal]) = {
      val xr = x.reg
      val yr = y.reg      
      Foreach(18 by 1) { i => 
        out((i / 3), (i % 3)) = xr(i / 3) * yr(i % 3)
      }
    }

    @virtualize def reg = {
      val rg = RegFile[SReal](6)
      Pipe {
        List
          .tabulate(6)(i => i)
          .foreach(i => Pipe{rg(i) = x(i)})
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
    def row(i: scala.Int): SVec6 = SVec6(x.reg(i, 0), x.reg(i, 1), x.reg(i, 2), x.reg(i, 3), x.reg(i, 4), x.reg(i, 5))
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

  implicit class SQuatOps(x: SQuat) {
    def *(y: SReal)        = SQuat(x.r * y, x.i * y, x.j * y, x.k * y)
    def *(y: SQuat)        = SQuatMult(x, y)
    def dot(y: SQuat)      = x.r * y.r + x.i * y.i + x.j * y.j + x.k * y.k
    def rotateBy(q: SQuat) = q * x
    def rotate(v: SVec3): SVec3 = {
      val inv = x.inverse
      val nq  = (x * SQuat(0.0, v.x, v.y, v.z)) * inv
      SVec3(nq.i, nq.j, nq.k)
    }
    def inverse = SQuatInverse(x)
  }

  def SQuatMult(q1: SQuat, q2: SQuat) = {
    SQuat(
      q1.r * q2.r - q1.i * q2.i - q1.j * q2.j - q1.k * q2.k,
      q1.r * q2.i + q1.i * q2.r + q1.j * q2.k - q1.k * q2.j,
      q1.r * q2.j - q1.i * q2.k + q1.j * q2.r + q1.k * q2.i,
      q1.r * q2.k + q1.i * q2.j - q1.j * q2.i + q1.k * q2.r
    )
  }

  def SQuatInverse(q: SQuat) = {
    val n = q.r * q.r + q.i * q.i + q.j * q.j + q.k * q.k
    SQuat(q.r, -q.i, -q.j, q.j) * (1 / n)
  }

  def det(a: Mat33): SReal = {
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
  

  @virtualize def prog() = {

    val inSIMU     = StreamIn[SIMU](In1)
    val inV       = StreamIn[SVicon](In2)
    val out       = StreamOut[SVicon](Out1)
    val parFactor = N (1 -> N)

    Accel {

      val particles = SRAM[Particle](N)
      val fifoSIMU   = FIFO[SIMU](100)
      val fifoV     = FIFO[SVicon](100)

      val lastSTime = Reg[STime](initTime)
      val lastO    = Reg[SOmega](SVec3(0.0, 0.0, 0.0))

      Sequential {

        Foreach(N by 1 par parFactor)(x => {
          val initSQuat = SQuat(initQ._1, initQ._2, initQ._3, initQ._4)
          Parallel {
            particles(x) = Particle(
              math.log(1.0 / N),
              initSQuat,
              State(
                SVec6(initV._1, initV._2, initV._3, initP._1, initP._2, initP._3),
                Sigma(
                  SVec6(initCov, 0, 0, 0, 0, 0),
                  SVec6(0, initCov, 0, 0, 0, 0),
                  SVec6(0, 0, initCov, 0, 0, 0),
                  SVec6(0, 0, 0, initCov, 0, 0),
                  SVec6(0, 0, 0, 0, initCov, 0),
                  SVec6(0, 0, 0, 0, 0, initCov)
                )
              ),
              SVec3(0.0, 0.0, 0.0),
              initSQuat
            )

          }
        })

        Parallel {
          Stream(*)(x => {
            fifoV.enq(inV)
          })

          Stream(*)(x => {
            fifoSIMU.enq(inSIMU)
          })

          FSM[Boolean, Boolean](true)(x => x)(x => {
              if ((fifoV.empty && !fifoSIMU.empty) || (!fifoSIMU.empty && !fifoV.empty && fifoSIMU.peek.t < fifoV.peek.t)) {
                val imu = fifoSIMU.deq() 
                updateFromSIMU(imu, lastSTime, lastO, particles, parFactor)
              }
              else if (!fifoV.empty) {
                val v = fifoV.deq() 
                updateFromV(v, lastSTime, lastO, particles, parFactor)
              }

              out := SVicon(lastSTime, averageSPOSE(particles, parFactor))
              normWeights(particles, parFactor)
              resample(particles, parFactor)
            })(x => (!fifoV.empty || !fifoSIMU.empty))

        }
      }
    }
  }

  def rotationMatrix(q: SQuat) =
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
  def update(accO: Option[SAcceleration],
             vicon: Option[SPOSE],
             dt: STime,
             lastO: Reg[SOmega],
             particles: SRAM1[Particle],
             parFactor: Int) = {
    Foreach(N by 1 par parFactor)(i => {
      val pp = particles(i)
      val nq =
        if (dt > 0.00001)
          sampleAtt(pp.q, lastO, dt)
        else
          pp.q

      val F =
        createMat66(
          1, 0, 0, 0, 0, 0,
          0, 1, 0, 0, 0, 0,
          0, 0, 1, 0, 0, 0,
          dt, 0, 0, 1, 0, 0,
          0, dt, 0, 0, 1, 0,
          0, 0, dt, 0, 0, 1
        )

      val U = SVec6(
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
        covFixAcc(1, 0), covFixAcc(1, 1), covFixAcc(1, 2), 0, 0, 0,
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
        val r: Mat33          = createMat33(covSViconP, 0, 0, 0, covSViconP, 0, 0, 0, covSViconP)
        val (nx2, nsig2, lik) = kalmanUpdate(nx, nsig, x.p, h, r)
        val nw                = pp.w + likelihoodSPOSE(x, lik._1, nq, lik._2)
        Particle(nw, nq, State(nx, fromMat66(nsig2)), nla, nlq)
      })
      particles(i) = np.getOrElse(Particle(pp.w, nq, State(nx, fromMat66(nsig)), nla, nlq))
    })
  }

  def updateFromSIMU(x: SIMU, lastSTime: Reg[STime], lastO: Reg[SOmega], particles: SRAM1[Particle], parFactor: Int) = {
    val dt = x.t - lastSTime
    lastSTime := x.t
    lastO := x.g
    update(Some(x.a), None, dt, lastO, particles, parFactor)
  }

  def updateFromV(x: SVicon, lastSTime: Reg[STime], lastO: Reg[SOmega], particles: SRAM1[Particle], parFactor: Int) = {
    val dt = x.t - lastSTime
    lastSTime := x.t
    update(None, Some(x.pose), dt, lastO, particles, parFactor)
  }

  @virtualize def likelihoodSPOSE(measurement: SPOSE, expectedPosMeasure: SPosition, quatState: SQuat, covPos: Mat33) = {
    val wPos                = unnormalizedGaussianLogPdf(measurement.p, expectedPosMeasure, covPos)
    val covSViconQMat: Mat33 = createMat33(covSViconQ, 0, 0, 0, covSViconQ, 0, 0, 0, covSViconQ)
    val error               = quatToLocalAngle(measurement.q.rotateBy(quatState.inverse))
    val wSQuat               = unnormalizedGaussianLogPdf(error, SVec3(0, 0, 0), covSViconQMat)
//    println("wPos: " + wPos)
//    println("wSQuat: " + wSQuat)
    wPos + wSQuat
  }

  def sampleAtt(q: SQuat, om: SOmega, dt: STime): SQuat = {
    val withNoise  = gaussianVec(om, covGyro)
    val integrated = withNoise * dt
    val lq         = localAngleToSQuat(integrated)
    lq.rotateBy(q)
  }

  def gaussianVec(mean: SVec3, variance: SReal) = {
    val g1 = gaussian()
    val g2 = gaussian()
    (SVec3(g1._1, g1._2, g2._1) * sqrt(variance)) + mean
  }

  //Box-Muller
  //http://www.design.caltech.edu/erik/Misc/Gaussian.html
  @virtualize def gaussian() = {

    val x1 = Reg[SReal]
    val x2 = Reg[SReal]
    val w  = Reg[SReal]
    val w2 = Reg[SReal]

    FSM[Boolean, Boolean](true)(x => x)(x => {
      x1 := 2.0 * random[SReal](1.0) - 1.0
      x2 := 2.0 * random[SReal](1.0) - 1.0
      w := (x1 * x1 + x2 * x2)
    })(x => w.value >= 1.0)

    w2 := sqrt((-2.0 * log(w.value)) / w)

    val y1 = x1 * w2;
    val y2 = x2 * w2;

    (y1, y2)
  }


  @virtualize def normWeights(particles: SRAM1[Particle], parFactor: Int) = {
    val totalWeight = Reg[SReal](0)
    val maxR = Reg[SReal]

    maxR := particles(0).w

    Reduce(maxR)(N by 1 par parFactor)(i => particles(i).w)(max(_,_))
    Reduce(totalWeight)(N by 1 par parFactor)(i => exp(particles(i).w - maxR))(_+_)
    totalWeight := maxR + log(totalWeight)
    Foreach(N by 1 par parFactor)(i => {
      val p = particles(i)
      particles(i) = Particle(p.w - totalWeight, p.q, p.st, p.lastA, p.lastQ)
    })    
  }

  @virtualize def resample(particles: SRAM1[Particle], parFactor: Int) = {

    val weights = SRAM[SReal](N)
    val out = SRAM[Particle](N)

    val u = random[SReal](1.0)

    Foreach(N by 1)(i => {
      if (i == 0)
        weights(i) = exp(particles(i).w)
      else
        weights(i) = weights(i-1) + exp(particles(i).w)
    })

    val k = Reg[Int](0)
    Foreach(N by 1)(i => {
      val b = weights(k)*N < i.to[SReal] + u
      FSM[Boolean, Boolean](b)(x => x)(x => k := k + 1)(x => weights(k)*N < i.to[SReal]+u)

      out(i) = particles(k)
    })

    Foreach(N by 1 par parFactor)(i => {
      val p = out(i)
      particles(i) = Particle(log(1.0/N), p.q, p.st, p.lastA, p.lastQ)
    })

  }

  def unnormalizedGaussianLogPdf(measurement: SVec3, state: SVec3, cov: Mat33): SReal = {
    val e = (measurement-state)
    -1/2.0*e.dot(inv(cov)*e)
  }

  def norm(v: SVec3) =
    sqrt(v.dot(v))

  def localAngleToSQuat(v: SVec3): SQuat = {
    val n    = norm(v)
    val l    = n / 2
    val sl   = sin(l)
    val nrot = v * (sl / n)
    SQuat(cos(l), nrot.x, nrot.y, nrot.z)
  }

  def quatToLocalAngle(q: SQuat): SVec3 = {
    val r: SReal = min(q.r, 1.0)
    val n = acos(r) * 2
    val s = n / sin(n / 2)
    SVec3(q.i * s, q.j * s, q.k * s)
  }

  def kalmanPredict(xp: SVec6, sigp: Mat66, f: Mat66, u: SVec6, q: Mat66) = {
    val xm   = f * xp + u
    val sigm = (f * sigp * f.t) + q
    (xm, sigm)
  }


  def kalmanUpdate(xm: SVec6, sigm: Mat66, z: SVec3, h: Mat36, r: Mat33) = {
    val s: Mat33   = (h * sigm * h.t) + r
    val za: SVec3   = h * xm
    val k: Mat63   = sigm * h.t * inv(s)
    val sig: Mat66 = sigm - (k * s * k.t)
    val x: SVec6    = xm + (k * (z - za))
    (x, sig, (za, s))
  }

  @virtualize def averageSPOSE(particles: SRAM1[Particle], parFactor: Int): SPOSE = {
    val firstQ = particles(0).q
    val accumP = Reg[SVec3](SVec3(0, 0, 0))
    val accumQ = Reg[SQuat](SQuat(0, 0, 0, 0))    
    Parallel {
      Reduce(accumP)(N by 1 par parFactor)(i => {
        val p = particles(i)
      p.st.x.vec3a * exp(p.w)
    })(_ + _)
      
      Reduce(accumQ)(N by 1 par parFactor)(i => {
        val p = particles(i)
        if (firstQ.dot(p.q) > 0.0)
          p.q * exp(p.w)
        else
          p.q * -(exp(p.w))
      })(_ + _)
    }
    SPOSE(accumP, accumQ)
  }

}

object ParticleFilterInterpreter extends ParticleFilter with SpatialStreamInterpreter {

  val outs = List(Out1)

  val inputs = collection.immutable.Map[Bus, List[MetaAny[_]]](
    (In1 -> List[SReal](3f, 4f, 2f, 6f).map(x => SIMU(x / 10, SVec3(x, x, x), SVec3(x / 100, x / 100, x / 100)))),
    (In2 -> List[SReal](3.5f, 5f).map(x => SVicon(x / 10, SPOSE(SVec3(x, x, x), SQuat(1, 0, 0, 0)))))
  )

}

object ParticleFilterCompiler extends ParticleFilter with SpatialStreamCompiler
