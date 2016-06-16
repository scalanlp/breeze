# Breeze [![Build Status](https://travis-ci.org/scalanlp/breeze.png?branch=master)](https://travis-ci.org/scalanlp/breeze)

Breeze is a library for numerical processing. It aims to be generic, clean, and powerful without sacrificing (much) efficiency.

The current version is 0.12. The latest release is 0.12.

## Documentation

* https://github.com/scalanlp/breeze/wiki/Quickstart
* https://github.com/scalanlp/breeze/wiki/Linear-Algebra-Cheat-Sheet
* [Scaladoc](http://www.scalanlp.org/api/breeze/) (Scaladoc is typically horribly out of date, and not a good way to learn Breeze.)
* There is also the [scala-breeze google group](https://groups.google.com/forum/#!forum/scala-breeze) for general questions and discussion.

## About the extensions

The functions:

**Householder:**
use the factory method to return a householder object
val h = householder(Matrix,Boolean)  
with boolean flag to initiate a complete transform. Otherwise manipulate using
makeHouseholder(shift)
applyHouseholderOnTheLeft(shift)
applyHouseholderOnTheRight(shift)

class variables are:
matrixh: the householder matrix
essential:  the essential part of the vector  v
tau : the scaling factor of the Householder transformation
beta : the result of H * M

```{r, engine='bash'}
scala> householder(DenseMatrix((1,2,3),(4,5,6),(7,8,9)),true).matrixH
res1: breeze.linalg.DenseMatrix[breeze.math.Complex] =
1.0 + 0.0i                 -3.597007303087045 + 0.0i  -0.24806946917841666 + 0.0i
-8.06225774829855 + 0.0i   14.046153846153848 + 0.0i  2.8307692307692314 + 0.0i
0.5803225354712214 + 0.0i  0.8307692307692303 + 0.0i  -0.04615384615384621 + 0.0i
```

**Hessenberg:**
val (p,h) = hessenberg(M)
returns tuple from hessenberg decomposition
p unitary matrix 
h hessenberg Matrix

```{r, engine='bash'}
scala> val (p,h) = hessenberg(DenseMatrix((1,2,3),(4,5,6),(7,8,9)))
p: breeze.linalg.DenseMatrix[breeze.math.Complex] =
1.0 + 0.0i  0.0 + 0.0i                   0.0 + 0.0i
0.0 + 0.0i  -0.49613893835683376 + 0.0i  -0.8682431421244592 + 0.0i
0.0 + 0.0i  -0.8682431421244592 + 0.0i   0.49613893835683387 + 0.0i
h: breeze.linalg.DenseMatrix[breeze.math.Complex] =
1.0 + 0.0i                -3.597007303087045 + 0.0i  -0.24806946917841666 + 0.0i
-8.06225774829855 + 0.0i  14.046153846153842 + 0.0i  2.8307692307692296 + 0.0i
0.0 + 0.0i                0.8307692307692296 + 0.0i  -0.046153846153847766 + 0.0i
```

**Schur:**
val (t,q) = schur(M)
returns tuple from schur decomposition
q is a unitary matrix (inverse q−1 is also the conjugate transpose q* of q)
t is an upper triangular matrix, (Schur form of A)

```{r, engine='bash'}
scala> val (p,h) = schur(DenseMatrix((1,2,3),(4,5,6),(7,8,9)))
p: breeze.linalg.DenseMatrix[breeze.math.Complex] =
16.116843969807043 + 0.0i  4.898979485566353 + 0.0i    1.5882058226456454E-15 + 0.0i
0.0 + 0.0i                 -1.1168439698070427 + 0.0i  -1.1164318374713465E-15 + 0.0i
0.0 + 0.0i                 0.0 + 0.0i                  -1.3036777264747022E-15 + 0.0i
h: breeze.linalg.DenseMatrix[breeze.math.Complex] =
-0.23197068724628608 + 0.0i  -0.8829059596535858 + 0.0i   0.40824829046386274 + 0.0i
-0.5253220933012334 + 0.0i   -0.23952042005420568 + 0.0i  -0.816496580927726 + 0.0i
-0.8186734993561813 + 0.0i   0.40386511954517373 + 0.0i   0.4082482904638631 + 0.0i
```

**Jacobi Rotation (with Givens):**

See examples below

```{r, engine='bash'}
scala>  var (r1, g2) = jacobi(DenseMatrix((1,2,3),(4,5,6),(7,8,9))) rotate2R (0, 1)
r1: breeze.linalg.DenseMatrix[breeze.math.Complex] =
1.0 + 0.0i                 2.0 + 0.0i                3.0 + 0.0i
8.055983888048337 + 0.0i   9.433981132056605 + 0.0i  10.811978376064872 + 0.0i
0.3179993640019081 + 0.0i  0.0 + 0.0i                -0.3179993640019072 + 0.0i
g2: breeze.linalg.DenseMatrix[breeze.math.Complex] =
1.0 + 0.0i  0.0 + 0.0i                 0.0 + 0.0i
0.0 + 0.0i  0.52999894000318 + 0.0i    0.847998304005088 + 0.0i
0.0 + 0.0i  -0.847998304005088 + 0.0i  0.52999894000318 + 0.0i
```

**MatrixPow:**
uses the ** operator or matrixPow(M , exp)
 
```{r, engine='bash'}
scala> DenseMatrix((1,2,3),(4,5,6),(7,8,9)) ** 2
res0: breeze.linalg.DenseMatrix[breeze.math.Complex] =
30.0 + 0.0i   36.0 + 0.0i   42.0 + 0.0i
66.0 + 0.0i   81.0 + 0.0i   96.0 + 0.0i
102.0 + 0.0i  126.0 + 0.0i  150.0 + 0.0i
```
In summary:

The Power of a Double / Integer Matrix is processed mainly by LAPACK and works for all Real powers.
There is a fall back for Complex Matrices, implemented in Scala. There are no solutions for Complex Matrices taken to a negative power. (~~Imaginary part not handled due to lapack.dgesv and~~ fommil lapack being for real numbers only. - This was the main stumbling block for this apparently simple problem to begin with)

I have compared accuracy/precision with Eigen and Matlab and will post results later.

Contains:
packages:
**breeze.linalg.matrixPow** (power of square matrix with native java fall back for complex matrices) using:
**breeze.linalg.schur** (Schur decomposition)(pade approximation folded inside matrixPow function)
**breeze.linalg.jacobi** {currently Jacobi rotation with Givens)
**breeze.linalg.hessenberg** (Hessenberg decomposition using householder reflections)
**breeze.linalg.householder** (Contains Householder transforms)
**breeze.linalg.realorthoganal** (generates a real orthogonal matrix)

in addition there are a few Helper functions in breeze.linalg.Helper - these will need to be reviewed sensibly

In addition to our previous discussion. The Math symbol ** would be great to integrate into breeze for Powering but I may need a little help with this

Functions:


```scala 
import breeze.linalg._
import breeze.math._
import java.text._

object Helper {

  def promptEnterKey: Option[Unit] = if (Console.in.read > 10) None else promptEnterKey
  def form(i: Double) = { val p = new DecimalFormat("#.############").format(i); s"$p"; }

  implicit class IMPLshowSI(M: DenseMatrix[Int]) {
    def showMI(s: String) {
      var cnt = -1
      println(s + "(Returning Int):\n[" + M.mapValues((i) => {
        cnt = cnt + 1
        (if ((cnt == 0) || (cnt % (M.cols) == 0)) { "" } else { " " }) + form(i) + (if (cnt < (M.cols * M.cols - 1) && cnt >= (M.cols * (M.cols - 1))) { " ;" } else { "" })
      }) + "]\n")

      //    println(s + "(Returning Integer)\n" + M.mapValues(i => form(i)) + "\n")
    }
  }

  implicit class IMPLshowSD(M: DenseMatrix[Double]) {
    def showMD(s: String) {
      var cnt = -1
      println(s + "(Returning Double):\n[" + M.mapValues((i) => {
        cnt = cnt + 1
        (if ((cnt == 0) || (cnt % (M.cols) == 0)) { "" } else { " " }) + form(i) + (if (cnt < (M.cols * M.cols - 1) && cnt >= (M.cols * (M.cols - 1))) { " ;" } else { "" })
      }) + "]\n")

      //    println(s + "(Returning Integer)\n" + M.mapValues(i => form(i)) + "\n")
    }
  }
  implicit class IMPLshowSC(M: DenseMatrix[Complex]) {

    def showMC(s: String) {
      var cnt = -1
      println(s + "(Returning Complex):\n[" + M.mapValues((i) => {
        cnt = cnt + 1
        (if ((cnt == 0) || (cnt % (M.cols) == 0)) { "" } else { " " }) + form(i.real) + (if (i.imag < 0) { "" } else { "+" }) + form(i.imag) + "i" + (if (cnt < (M.cols * M.cols - 1) && cnt >= (M.cols * (M.cols - 1))) { " ;" } else { "" })
      }) + "]\n")
    } //println(s + "(Returning Complex)\n" + M.mapValues(i => form(i.real) + "," + form(i.imag)) + "\n") }
  }
  implicit class IMPLshowSVC(M: DenseVector[Complex]) {
    def showVC(s: String) { println(s + "(Returning Complex)\n" + M.mapValues(i => form(i.real) + (if (i.imag < 0) "" else "+") + form(i.imag) + "i") + "\n") }
  }
  implicit class IMPLshowC(M: DenseMatrix[Complex]) {

    def showMatPow(pow: Double)(implicit name: String) = {
      try {
        M ** pow showMC (s"Power $pow of $name (Complex) :")
      } catch { case e: IllegalArgumentException => println(e.getMessage + "\n") }
    }
    //(P,H, House)
    def showHessenberg(implicit name: String) {
      hessenberg(M)._1 showMC (s"Hessenberg from $name (Complex) P:")
      hessenberg(M)._2 showMC (s"Hessenberg from $name (Complex) H:")

    }
    //(T,Q, Coeffs, H)
    def showSchur(implicit name: String) {
      schur(M)._1 showMC (s"Schur from $name (Complex)T:")
      schur(M)._2 showMC (s"Schur from $name (Complex)Q:")

    }
    def showJacobi(implicit name: String) = {
      M showMC (s"Jacobi  from $name After Rotation (Complex) Q:")
    }
  }
  implicit class IMPLshowI(M: DenseMatrix[Double]) {

    def showMatPow(pow: Double)(implicit name: String) = try { (M ** pow) showMC (s"Power $pow of $name (Real):") } catch { case e: IllegalArgumentException => println(e.getMessage + "\n") }
    //(P,H, House)
    def showHessenberg(implicit name: String) = {
      hessenberg(M.mapValues(Complex(_, 0.0)))._1 showMC (s"Hessenberg from $name (Real)  P:")
      hessenberg(M.mapValues(Complex(_, 0.0)))._2 showMC (s"Hessenberg from $name (Real) H:")

      //(T,Q, Coeffs, H)
    }
    def showSchur(implicit name: String) = {
      schur(M.mapValues(Complex(_, 0.0)))._1 showMC (s"Schur from $name (Real) T:")
      schur(M.mapValues(Complex(_, 0.0)))._2 showMC (s"Schur from  $name (Real)  Q:")

    }

  }
}
import Helper._

object Main {

  def main(args: Array[String]): Unit = {

    val power1 = 2.3; //.43f;
    val power2 = 2.321; //.43f;
    val power3 = -2; //.43f;
    val power4 = -2.321; //.43f;

    val R = DenseMatrix((4.231, 1.231, -2.0, 2.0), (0.2, 2.123, 0.123, 1.0), (-2.0, 0.0, 3.2342, -2.0), (2.0, 1.123, -2.0, -1.0))
    val I = DenseMatrix((1, 2, -3, 4), (1, 5, 0, 1), (-2, 6, 3, -2), (5, 1, -2, -1))
    val C = DenseMatrix.tabulate[Complex](R.cols, R.rows)((i, j) => Complex(R(i, j), I(i, j)))
    val RtoC = R.mapValues(Complex(_, 0.0))

    implicit var name = "C"

    C showMC "Matrix C (Complex)"

    C showMatPow power1

    C showMatPow power2

    C showMatPow power3

    C showMatPow power4

    name = "R"

    R showMD "Matrix R (Real)"

    R showMatPow power1

    R showMatPow power2

    R showMatPow power3

    R showMatPow power4

    name = "RtoC"

    RtoC showMC "Matrix C (Complex)"

    RtoC showMatPow power1

    RtoC showMatPow power2

    RtoC showMatPow power3

    RtoC showMatPow power4

    name = "R"

    R showSchur

    R showHessenberg

    val (u1, q1) = schur(R)
    val ans1 = (q1 * u1 * q1.t).mapValues((i) => ((i.real * 100000.0).round) / 100000.0)

    assert(ans1 == R.mapValues((i) => ((i * 100000.0).round) / 100000.0))

    val (p, h) = hessenberg(R)
    val ans2 = (p * h * p.t).mapValues((i) => ((i.real * 100000.0).round) / 100000.0)

    assert(ans2 == R.mapValues((i) => ((i * 100000.0).round) / 100000.0))

    name = "C"

    C showSchur

    C showHessenberg

    /*
     * Example from Wikipedia Givens Rotation
     * https://en.wikipedia.org/wiki/Givens_rotation#Triangularization
     * performs inplace i.e mutates matrix so needs a rethink...
     */

    import breeze.linalg.jacobi._
    var A = DenseMatrix((6, 5, 0), (5, 1, 4), (0, 4, 3)).mapValues(_.toDouble).mapValues(Complex(_, 0.0))
    name = "A"
    A showMC "Matrix A:"

    /*Example A Givens defined from Matrix and pivoted from an index away from corner   rotateX (index, height of Givens Pair)*/
    var J1 = jacobi(A)
    var (a1, g1) = J1 rotate2L (0, 0); a1 showMC "Rotate Matrix A1:"; g1 showMC "Jacobi rotation Matrix G1"
    J1 = jacobi(a1)
    var (r1, g2) = J1 rotate2R (0, 1); r1 showMC "Rotate Matrix R1:"; g2 showMC "Jacobi rotation Matrix G2"
    var Q = g1.t * g2.t; Q showMC "Q From Example A"

    /*Example 1 Givens defined from Matrix and pivoted from an index away from corner   rotateX (index, height of Givens Pair)*/
    J1 = jacobi(A)
    var A1 = J1 rotateL (0, 0); A1 showMC "Rotate Matrix A1:"
    var G1 = J1 getGivens; G1 showMC "Jacobi rotation Matrix G1"

    var J2 = jacobi(A1)
    var R1 = J2 rotateR (0, 1); R1 showMC "Rotate Matrix R1:"
    var G2 = J2 getGivens; G2 showMC "Jacobi rotation Matrix G2"

    Q = G1.t * G2.t; Q showMC "Q From Example 1"

    /*Example 2 with  arbitrary Givens  (pivoting at corners) */
    var giv = makeGivens(A(0, 0), A(1, 0))
    J1 = jacobi(A)
    A1 = J1 rotateL giv
    G1 = J1 getGivens; G1 showMC "Jacobi rotation Matrix G1"
    A1 showMC "Rotate Matrix A1:"
    giv = makeGivens(A1(1, 1), A1(2, 1))
    J2 = jacobi(A1)
    R1 = J2 rotateR giv
    G2 = J2 getGivens; G2 showMC "Jacobi rotation Matrix G2"
    R1 showMC "Rotate Matrix R1:"

    Q = G1.t * G2.t

    Q showMC "Q From Example 2"

    /* Example 3 basic QR decomposition */

    val aa = DenseMatrix((3, 2, 1), (2, -3, 4), (5, 1, -1), (7, 4, 2))
    var j = jacobi(aa.mapValues(Complex(_, 0.0)))
    val (c1, g1_4) = j.rotate2R(0, 0); j = jacobi(c1)
    val (c2, g1_3) = j.rotate2R(1, 0); j = jacobi(c2)
    val (c3, g1_2) = j.rotate2R(2, 0); j = jacobi(c3)
    val (c4, g2_4) = j.rotate2R(0, 1); j = jacobi(c4)
    val (c5, g2_3) = j.rotate2R(1, 1); j = jacobi(c5)
    val (c6, g3_4) = j.rotate2R(0, 2)

    val r = c6
    val q = (g3_4 * g2_3 * g2_4 * g1_2 * g1_3 * g1_4).t

    q showMC "Q From Example 3"
    r showMC "R From Example 3"

    val a2 = (q * r).mapValues(_.real.round.toInt)
    assert(aa == a2)
    a2 showMI "A  From Example 3"

    /*Example 4. This example is taken from the book "Numerical Analysis" by Richard L. Burden (Author), J. Douglas Faires.
     *In this example, the given matrix is transformed to the similar tridiagonal matrix A2 by using the Householder method.
    */
    val HH  = DenseMatrix((4, 1, -2, 2), (1, 2, 0, 1), (-2, 0, 3, -2), (2, 1, -2, -1)).mapValues(Complex(_, 0.0))
    val ans3 = householder(HH,true).matrixH
    ans3 showMC "Example 4 (A2 = Q2A1Q2): "

  }
}


```
```{r, engine='bash'}
[info] Running Main
Matrix C (Complex)(Returning Complex):
[4.231+1i  1.231+2i   -2-3i       2+4i ;
 0.2+1i    2.123+5i   0.123+0i    1+1i ;
 -2-2i     0+6i       3.2342+3i   -2-2i ;
 2+5i      1.123+1i   -2-2i       -1-1i    ]

Power 2.3 of C (Complex) :(Returning Complex):
[-23.179475513003+80.97870153996i    25.366380850301+27.774746437711i    20.472474578306-79.297243769203i    -11.7732543834+44.115107008425i ;
 -21.279349665736+23.501222706794i   -49.819901605348+29.498736809409i   11.711906950064-10.809203031844i    -20.330135151353+12.39471207836i ;
 -4.508029631453-72.898431706426i    -98.494229803534+10.338166391579i   -17.332031131657+70.575014505862i   6.453550536603-32.511725751644i ;
 -11.900128507974+39.921533261791i   4.643824724365+13.395124705251i     35.063110768367-47.457069329005i    -23.704709182727+53.684483637558i   ]

Power 2.321 of C (Complex) :(Returning Complex):
[-25.489950193024+85.053065729248i   26.666556869215+29.154901061873i    22.826609133818-82.676644756724i    -13.708849942065+45.773679369307i ;
 -22.698748362748+24.187741148704i   -52.396212166173+29.452585969649i   12.692708531733-11.234656260632i    -21.722289364846+12.74251983077i ;
 -3.973369454789-76.472478889609i    -103.302731373895+8.502435238116i   -19.321367454202+73.565304323294i   7.523435684038-34.402373844467i ;
 -14.140977774148+41.26097484007i    5.029779249775+14.548308358409i     37.654390748858-49.560600530119i    -24.461441342452+55.506652258796i   ]

Cannot currently invert complex matrices.

Cannot currently invert complex matrices.

Matrix R (Real)(Returning Double):
[4.231  1.231   -2       2 ;
 0.2    2.123   0.123    1 ;
 -2     0       3.2342   -2 ;
 2      1.123   -2       -1    ]

Jun 16, 2016 5:30:33 PM com.github.fommil.netlib.LAPACK <clinit>
WARNING: Failed to load implementation from: com.github.fommil.netlib.NativeSystemLAPACK
Jun 16, 2016 5:30:33 PM com.github.fommil.jni.JniLoader liberalLoad
INFO: successfully loaded C:\Users\anthony\AppData\Local\Temp\jniloader100129759046252809netlib-native_ref-win-i686.dll
Jun 16, 2016 5:30:33 PM com.github.fommil.netlib.BLAS <clinit>
WARNING: Failed to load implementation from: com.github.fommil.netlib.NativeSystemBLAS
Jun 16, 2016 5:30:33 PM com.github.fommil.jni.JniLoader load
INFO: already loaded netlib-native_ref-win-i686.dll
Power 2.3 of R (Real):(Returning Complex):
[45.838678514107+0.169775413935i   17.742959200373+0.161031522602i   -34.171717545211-0.197707158111i   21.747239017473-0.736155327746i ;
 5.305558503066+0.226654530546i    7.956412853986+0.214662481618i    -3.250319277898-0.269157940066i    2.896908866498-0.991541991122i ;
 -34.286807921094-0.29897575455i   -9.401289761907-0.283325377572i   31.426477980265+0.352291237071i    -16.514548930544+1.303306378186i ;
 20.154382187923-0.977877699058i   7.445288232186-0.925880463016i    -16.016128083623+1.165487318762i   13.70514168836+4.285014783298i      ]

Power 2.321 of R (Real):(Returning Complex):
[47.684836040719+0.179221398751i    18.45644354046+0.170225496086i    -35.618210732026-0.211957060969i   22.718138185801-0.78268105156i ;
 5.513807592596+0.239291960417i     8.127742778003+0.226910276465i    -3.386603510994-0.28848272153i     3.079352561955-1.054214994208i ;
 -35.727518537664-0.315631417779i   -9.849199238957-0.299495136828i   32.626695658117+0.377623543577i    -17.316940676086+1.385681815491i ;
 21.075218391652-1.032422349502i    7.823016219271-0.978700886153i    -16.782748021406+1.249106316017i   13.900545197578+4.555865650765i     ]

Power -2.0 of R (Real):(Returning Complex):
[0.20279920517+0i     -0.171738182901+0i   0.151282940729+0i    -0.08603151153+0i ;
 -0.041111360404+0i   0.223104708376+0i    -0.019358598993+0i   0.003148677753+0i ;
 0.170611874107+0i    -0.118372036323+0i   0.215862130857+0i    -0.001560474622+0i ;
 -0.058572354113+0i   0.001835277507+0i    0.02504833899+0i     0.187164105618+0i     ]

Power -2.321 of R (Real):(Returning Complex):
[0.172626927556-0.005872321813i    -0.164095512526-0.001971539358i  0.13908317932+0.004303418208i     -0.065129781807+0.020253810748i ;
 -0.037468388567-0.008284294034i   0.17166924327-0.002294957378i    -0.012546834137+0.005453927731i   0.017555632344+0.027403633416i ;
 0.151262494099+0.010693161171i    -0.108023376784+0.0032050342i    0.169727640623-0.007347788643i    -0.024939702805-0.035955486744i ;
 -0.049289404304+0.036102695524i   0.032050890622+0.009627672622i   -0.012138252283-0.023293952686i   0.087116106642-0.118525913788i     ]

Matrix C (Complex)(Returning Complex):
[4.231+0i  1.231+0i   -2+0i       2+0i ;
 0.2+0i    2.123+0i   0.123+0i    1+0i ;
 -2+0i     0+0i       3.2342+0i   -2+0i ;
 2+0i      1.123+0i   -2+0i       -1+0i    ]

Power 2.3 of RtoC (Complex) :(Returning Complex):
[45.838678514107+0.169775413935i   17.742959200373+0.161031522602i   -34.171717545211-0.197707158111i   21.747239017473-0.736155327746i ;
 5.305558503066+0.226654530546i    7.956412853986+0.214662481618i    -3.250319277898-0.269157940066i    2.896908866498-0.991541991122i ;
 -34.286807921094-0.29897575455i   -9.401289761907-0.283325377572i   31.426477980265+0.352291237071i    -16.514548930544+1.303306378186i ;
 20.154382187923-0.977877699058i   7.445288232186-0.925880463016i    -16.016128083623+1.165487318762i   13.70514168836+4.285014783298i      ]

Power 2.321 of RtoC (Complex) :(Returning Complex):
[47.684836040719+0.179221398751i    18.45644354046+0.170225496086i    -35.618210732026-0.211957060969i   22.718138185801-0.78268105156i ;
 5.513807592596+0.239291960417i     8.127742778003+0.226910276465i    -3.386603510994-0.28848272153i     3.079352561955-1.054214994208i ;
 -35.727518537664-0.315631417779i   -9.849199238957-0.299495136828i   32.626695658117+0.377623543577i    -17.316940676086+1.385681815491i ;
 21.075218391652-1.032422349502i    7.823016219271-0.978700886153i    -16.782748021406+1.249106316017i   13.900545197578+4.555865650765i     ]

Cannot currently invert complex matrices.

Cannot currently invert complex matrices.

Schur from R (Real) T:(Returning Complex):
[6.918170761219+0i  -0.177733774328+0i   0.229750015047+0i    0.822144392543+0i ;
 0+0i               -2.211758934055+0i   -0.144669221501+0i   -0.091411104246+0i ;
 0+0i               0+0i                 1.54099680782+-0i    -0.551033628947+0i ;
 0+0i               0+0i                 0+0i                 2.340791365016+0i     ]

Schur from  R (Real)  Q:(Returning Complex):
[0.730416039626+0i    -0.172698423202+0i   -0.659739455333+0i   0.037570131199+0i ;
 0.087286842406+0i    -0.215159326643+0i   0.207080163209+0i    0.950371126091+0i ;
 -0.583418659378+0i   0.291933889439+0i    -0.706753108242+0i   0.273673740202+0i ;
 0.344232601724+0i    0.915781809701+0i    0.149538292996+-0i   0.143128934387+0i    ]

Hessenberg from R (Real)  P:(Returning Complex):
[1+0i   0+0i                 0+0i                 0+0i ;
 0+0i   -0.070534561586+0i   0.254801962739+0i    -0.964417355405+0i ;
 0+0i   0.705345615859+0i    -0.670908693123+0i   -0.228842932337+0i ;
 0+0i   -0.705345615859+0i   -0.696388889397+0i   -0.132401196797+0i    ]

Hessenberg from R (Real) H:(Returning Complex):
[4.231+0i             -2.908210508747+0i   0.262700823584+0i    -0.994314293422+0i ;
 -2.835489375752+0i   3.211656716418+0i    -2.170790984758+0i   0.16817760265+0i ;
 0+0i                 -2.126357956874+0i   -1.157938511347+0i   0.099580203736+0i ;
 0+0i                 0+0i                 0.099580203736+0i    2.303481794929+0i    ]

Schur from C (Complex)T:(Returning Complex):
[-1.369541273118-3.630649654831i  1.480427293737-2.887514535151i   -0.1436734564-2.660113678297i    0.284955171018+1.082287790957i ;
 0+0i                             0.766375596527-1.405351483231i   -0.21915307655+0.92422057499i    -4.503619478031-1.295390116421i ;
 0+0i                             0+0i                             7.276766473642+6.934991630237i   -0.089439252864+1.182148186924i ;
 0+0i                             0+0i                             0+0i                             1.91459920295+6.101009507825i      ]

Schur from C (Complex)Q:(Returning Complex):
[0.384750605342-0.078682553439i    0.541459497873-0.377106447743i    -0.086616214504-0.618393271707i  -0.140719364616+0.025958043492i ;
 -0.013966566326-0.127744614314i   -0.058692955362+0.134287220472i   0.186276649166-0.194413775531i   0.348867236721+0.876244413934i ;
 -0.055069662012+0.30031348547i    0.433476299791-0.540659092255i    0.158253566363+0.579807919987i   0.185403440598+0.175981064664i ;
 -0.520929041398+0.681670266779i   -0.171514554231-0.18303884782i    -0.13204307889-0.398666397785i   -0.123862906304+0.096570327374i   ]

Hessenberg from C (Complex) P:(Returning Complex):
[1+0i   0+0i                              0+0i                              0+0i ;
 0+0i   -0.032427221756-0.162136108781i   -0.282848373817+0.314979563102i   -0.29342059255-0.841040656355i ;
 0+0i   0.324272217563+0.324272217563i    0.030534185261-0.766165620608i    -0.379867080677-0.239695565624i ;
 0+0i   -0.324272217563-0.810680543907i   0.215064619752-0.431960399708i    -0.050153582056-0.047818670725i    ]

Hessenberg from C (Complex) H:(Returning Complex):
[4.231+1i             3.202804265646-4.804255039301i   -1.179739868385+1.259109459572i   1.452495546555-0.299421529467i ;
 -6.167657578044+0i   1.993967402734+1.391161934805i   -1.712172272493-4.040829504898i   0.439899552774-1.306782352999i ;
 0+0i                 -3.914580050889+0i               1.492878495193-0.797800587502i    0.868407106297+3.580810401071i ;
 0+0i                 0+0i                             2.569667745341+0i                 0.870354102073+6.406638652696i    ]

Matrix A:(Returning Complex):
[6+0i   5+0i   0+0i ;
 5+0i   1+0i   4+0i ;
 0+0i   4+0i   3+0i    ]

Rotate Matrix A1:(Returning Complex):
[7.810249675907+0i  4.481290797651+0i    2.560737598658+0i ;
 0+0i               -2.432700718725+0i   3.07288511839+0i ;
 0+0i               4+0i                 3+0i                ]

Jacobi rotation Matrix G1(Returning Complex):
[0.768221279597+0i    0.640184399664+0i   0+0i ;
 -0.640184399664+0i   0.768221279597+0i   0+0i ;
 0+0i                 0+0i                1+0i    ]

Rotate Matrix R1:(Returning Complex):
[7.810249675907+0i  4.481290797651+0i   2.560737598658+0i ;
 0+0i               4.681669871625+0i   0.966447931615+0i ;
 0+0i               0+0i                -4.184328063895+0i   ]

Jacobi rotation Matrix G2(Returning Complex):
[1+0i   0+0i                  0+0i ;
 0+0i   -0.519622439307+0i    0.854395997514+-0i ;
 0+0i   -0.854395997514+-0i   -0.519622439307+0i    ]

Q From Example A(Returning Complex):
[0.768221279597+0i   0.33265417936+0i     0.546970988744+0i ;
 0.640184399664+0i   -0.399185015232+0i   -0.656365186493+0i ;
 0+0i                0.854395997514+0i    -0.519622439307+0i    ]

Rotate Matrix A1:(Returning Complex):
[7.810249675907+0i  4.481290797651+0i    2.560737598658+0i ;
 0+0i               -2.432700718725+0i   3.07288511839+0i ;
 0+0i               4+0i                 3+0i                ]

Jacobi rotation Matrix G1(Returning Complex):
[0.768221279597+0i    0.640184399664+0i   0+0i ;
 -0.640184399664+0i   0.768221279597+0i   0+0i ;
 0+0i                 0+0i                1+0i    ]

Rotate Matrix R1:(Returning Complex):
[7.810249675907+0i  4.481290797651+0i   2.560737598658+0i ;
 0+0i               4.681669871625+0i   0.966447931615+0i ;
 0+0i               0+0i                -4.184328063895+0i   ]

Jacobi rotation Matrix G2(Returning Complex):
[1+0i   0+0i                  0+0i ;
 0+0i   -0.519622439307+0i    0.854395997514+-0i ;
 0+0i   -0.854395997514+-0i   -0.519622439307+0i    ]

Q From Example 1(Returning Complex):
[0.768221279597+0i   0.33265417936+0i     0.546970988744+0i ;
 0.640184399664+0i   -0.399185015232+0i   -0.656365186493+0i ;
 0+0i                0.854395997514+0i    -0.519622439307+0i    ]

Jacobi rotation Matrix G1(Returning Complex):
[0.768221279597+0i    0.640184399664+0i   0+0i ;
 -0.640184399664+0i   0.768221279597+0i   0+0i ;
 0+0i                 0+0i                1+0i    ]

Rotate Matrix A1:(Returning Complex):
[7.810249675907+0i  4.481290797651+0i    2.560737598658+0i ;
 0+0i               -2.432700718725+0i   3.07288511839+0i ;
 0+0i               4+0i                 3+0i                ]

Jacobi rotation Matrix G2(Returning Complex):
[1+0i   0+0i                  0+0i ;
 0+0i   -0.519622439307+0i    0.854395997514+-0i ;
 0+0i   -0.854395997514+-0i   -0.519622439307+0i    ]

Rotate Matrix R1:(Returning Complex):
[7.810249675907+0i  4.481290797651+0i   2.560737598658+0i ;
 0+0i               4.681669871625+0i   0.966447931615+0i ;
 0+0i               0+0i                -4.184328063895+0i   ]

Q From Example 2(Returning Complex):
[0.768221279597+0i   0.33265417936+0i     0.546970988744+0i ;
 0.640184399664+0i   -0.399185015232+0i   -0.656365186493+0i ;
 0+0i                0.854395997514+0i    -0.519622439307+0i    ]

Q From Example 3(Returning Complex):
[0.321633760451+-0i     0.206175487469+-0i     0.251052152752+-0i     -0.889390920295+-0i
0.214422506968+-0i     -0.898925125364+-0i    0.381337511645+-0i     -0.023201502269+-0i
0.536056267419+-0i     -0.214422506968+-0i    -0.8120525794+-0i      -0.085072174985+-0i
0.750478774386+-0i ;   0.321633760451+-0i ;   0.363490202208+-0i ;   0.448562377192+-0i   ]

R From Example 3(Returning Complex):
[9.327379053089+0i   3.537971364965+0i   2.144225069676+0i
 0+0i               4.181238885867+0i  -2.531834986117+0i
 0+0i              0+0i ;               3.315435183147+0i
0+0i                0+0i ;              0+0i               ]

A  From Example 3(Returning Int):
[3    2     1
 2   -3   4
 5  1 ;    -1
7    4 ;   2   ]

Example 4 (A2 = Q2A1Q2): (Returning Complex):
[4+0i      -3+0i                0+0i                 0+0i ;
 -3+0i     3.333333333333+0i    -1.666666666667+0i   0+0i ;
 -0.5+0i   -1.666666666667+0i   -1.32+0i             0.906666666667+0i ;
 0.5+0i    0.5+0i               0.906666666667+0i    1.986666666667+0i    ]

[success] Total time: 8 s, completed 16-Jun-2016 17:30:34
```

## Using Breeze

### Building it yourself.

This project can be built with sbt 0.13

### SBT

For **SBT**, Add these lines to your SBT project definition:

* For SBT versions 0.13.x or later

```scala
libraryDependencies  ++= Seq(
  // other dependencies here
  "org.scalanlp" %% "breeze" % "0.12",
  // native libraries are not included by default. add this if you want them (as of 0.7)
  // native libraries greatly improve performance, but increase jar sizes. 
  // It also packages various blas implementations, which have licenses that may or may not
  // be compatible with the Apache License. No GPL code, as best I know.
  "org.scalanlp" %% "breeze-natives" % "0.12",
  // the visualization library is distributed separately as well. 
  // It depends on LGPL code.
    "org.scalanlp" %% "breeze-viz" % "0.12"
)

resolvers ++= Seq(
  // other resolvers here
  // if you want to use snapshot builds (currently 0.12-SNAPSHOT), use this.
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

// or 2.11.5
scalaVersion := "2.10.4"
```

For more details on the optional `breeze-natives` module, please watch Sam Halliday's talk at Scala eXchange 2014 [High Performance Linear Algebra in Scala](https://skillsmatter.com/skillscasts/5849-high-performance-linear-algebra-in-scala) ([follow along with high-res slides](http://fommil.github.io/scalax14/#/)).


### Maven

Maven looks like this:

```xml
<dependency>
  <groupId>org.scalanlp</groupId>
  <artifactId>breeze_2.10</artifactId> <!-- or 2.11 -->
  <version>0.12</version>
</dependency>
```

### Other build tools

http://mvnrepository.com/artifact/org.scalanlp/breeze_2.10/0.12 (as an example) is a great resource for finding other configuration examples for other build tools.

See documentation (linked above!) for more information on using Breeze.

## History

Breeze is the merger of the ScalaNLP and Scalala projects, because one of the original maintainers is unable to continue development. The Scalala parts are largely rewritten.

(c) David Hall, 2009 -

Portions (c) Daniel Ramage, 2009 - 2011

Contributions from:

* Jason Zaugg (@retronym)
* Alexander Lehmann (@afwlehmann)
* Jonathan Merritt (@lancelet)
* Keith Stevens (@fozziethebeat)
* Jason Baldridge (@jasonbaldridge)
* Timothy Hunter (@tjhunter)
* Dave DeCaprio (@DaveDeCaprio)
* Daniel Duckworth (@duckworthd)
* Eric Christiansen (@emchristiansen)
* Marc Millstone (@splittingfield)
* Mérő László (@laci37)
* Alexey Noskov (@alno)
* Devon Bryant (@devonbryant)
* Kentaroh Takagaki (@ktakagaki)
* Sam Halliday (@fommil)
* Chris Stucchio (@stucchio)
* Xiangrui Meng (@mengxr)
* Gabriel Schubiner (@gabeos)
* Debasish Das (@debasish83)
* Julien Dumazert (@DumazertJulien)
* Matthias Langer (@bashimao)

Corporate (Code) Contributors:
* [Semantic Machines](http://www.semanticmachines.com/) (@semanticmachines)
* [ContentSquare](http://www.contentsquare.com/en/)
* Big Data Analytics, Verizon Lab, Palo Alto
* [crealytics GmbH, Berlin/Passau, Germany](https://crealytics.com/)


And others (contact David Hall if you've contributed code and aren't listed).
