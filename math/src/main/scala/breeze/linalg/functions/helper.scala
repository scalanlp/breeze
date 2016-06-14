package breeze.linalg
/*
 Copyright 2016 @author claydonkey (Anthony Campbell)

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

import breeze.numerics._
import breeze.math._
import DenseMatrix.canMapValues
import DenseMatrix._
import scala.io._
import scala.Console._

object GlobalConsts {

  implicit class MutableInt(var value: Int) {
    def inc() = { value += 1 }
  }
  object printType extends Enumeration {
    type printType = Value
    val REAL, IMAG, BOTH = Value
  }
  def function(s: MutableInt): Boolean = {
    s.inc() // parentheses here to denote that method has side effects
    true
  }
  val EPSILON: Double = 2.22045e-016


}

object Helper {
  import GlobalConsts._

  def abs2(n: Complex): Double = { (n.real * n.real) + (n.imag * n.imag) }
  def conj(n: Complex): Complex = { Complex(n.real, -n.imag) }
  def conj(n: DenseMatrix[Complex]): DenseMatrix[Complex] = (n.mapValues(c => conj(c)))
  def norm1(n: Complex): Double = { abs(n.real) + abs(n.imag) }
  def biggest(M: DenseMatrix[Complex]) = norm1(sum(M(::, *)).t.reduceLeft((x, y) => if (norm1(x) > norm1(y)) x else y))
  val M_PI = 3.14159265358979323846
  def sinh2(c: Complex) = { Complex(sinh(c.real) * cos(c.imag), cosh(c.real) * sin(c.imag)) }
  def isMuchSmallerThan(x: Double, y: Double) = { abs(x) <= abs(y) * EPSILON }
  implicit def Y3[A1, A2, A3, B](f: ((A1, A2, A3) => B) => ((A1, A2, A3) => B)): (A1, A2, A3) => B = f(Y3(f))(_, _, _)
  type fType[A] = (Int, DenseMatrix[Complex]) => A
  def Y[A, B](f: (A => B) => (A => B)): A => B = f(Y(f))(_)
  def atan2h(x: Complex, y: Complex): Complex = { val z = x / y; if ((y == 0) || (abs2(z) > pow(GlobalConsts.EPSILON, 0.5))) (0.5) * log((y + x) / (y - x)) else z + z * z * z / 3 }

/*from https://en.wikipedia.org/wiki/Triangular_matrix */
  def UEvaluate(M: DenseMatrix[Complex]): DenseMatrix[Complex] =
    { // All upper must be non zero
      // Ax = b -> LUx = b. Then y is defined to be Ux
      val n = M.cols
      val U = DenseMatrix.zeros[Complex](n, n)
      // Backward solve Ux = y
      var k = 0
      var j = 0
      var i = 0
      for (j <- (n - 1) to 0 by -1) {
        U(j, j) = 1.0 / M(j, j)
        for (i <- (j - 1) to 0 by -1) {
          for (k <- (i + 1) to j)
            U(i, j) -= 1 / M(i, i) * M(i, k) * U(k, j)
        }
      }
      U
    }
/*from https://en.wikipedia.org/wiki/Triangular_matrix */
  def LEvaluate(M: DenseMatrix[Complex]): DenseMatrix[Complex] =
    { // All lower must be non zero
      // Forward solve Ly = b//
      // Ax = b -> LUx = b. Then y is defined to be Ux
      val n = M.cols
      val L = DenseMatrix.zeros[Complex](n, n)
      // Forward solve Lx = y
      var k = 0
      var j = 0
      var i = 0
      for (j <- 0 to n) {
        L(j, j) = 1.0 / M(j, j)
        for (i <- 0 to j) {
          for (k <- j to i)
            L(i, j) -= 1 / M(i, i) * M(i, k) * L(k, j)
        }
      }
      L
    }

  def UTadj(A: DenseMatrix[Complex]) = UEvaluate(A) * diag(A).reduce(_ * _)
  def Tadj(A: DenseMatrix[Complex]) = A.t.mapValues(i => Complex(i.real, -i.imag))

  def forwardSubC(L: DenseMatrix[Complex], B: DenseVector[Complex]) = {
    val n = B.size
    val x = DenseVector.zeros[Complex](n)
    var i = 0
    for (i <- 0 until n)
      x(i) = ((B(i) - L(i, ::) * x) / L(i, i))
    x
  }

  def backwardSubC(U: DenseMatrix[Complex], B: DenseVector[Complex]) = {
    val n = B.size
    val x = DenseVector.zeros[Complex](n)
    var i = 0
    for (i <- (n - 1) to 0 by -1)
      x(i) = (B(i) - U(i, ::) * x) / U(i, i)
    x
  }

  def backwardSubD(U: DenseMatrix[Double], B: DenseVector[Double]) = {
    val n = B.size
    val x = DenseVector.zeros[Double](n)
    var i = 0
    for (i <- (n - 1) to 0 by -1)
      x(i) = (B(i) - U(i, ::) * x) / U(i, i)
    x
  }
  private def forwardSubD(L: DenseMatrix[Double], B: DenseVector[Double]) = {
    val n = B.size
    val x = DenseVector.zeros[Double](n)
    var i = 0
    for (i <- 0 until n)
      x(i) = (B(i) - L(i, ::) * x) / L(i, i)
    x
  }

}
