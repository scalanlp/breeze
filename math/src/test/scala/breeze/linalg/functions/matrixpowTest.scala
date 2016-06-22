/*
 * The MIT License
 *
 * Copyright 2016 @claydonkey (Anthony Campbell).
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package breeze.linalg.functions

import breeze.linalg.{ SparseVector, DenseVector }
import org.scalatest.FunSuite
import breeze.linalg._
import breeze.math._
import java.text._
import DenseMatrix.canMapValues

class matrixpowTest extends FunSuite {

  val powerA = 0.5
  val power1 = 2.3
  val power2 = 2.321
  val power3 = -2
  val power4 = -2.321
  val R = DenseMatrix((4.231, 1.231, -2.0, 2.0), (0.2, 2.123, 0.123, 1.0), (-2.0, 0.0, 3.2342, -2.0), (2.0, 1.123, -2.0, -1.0))
  val I = DenseMatrix((1, 2, -3, 4), (1, 5, 0, 1), (-2, 6, 3, -2), (5, 1, -2, -1))
  val C = DenseMatrix.tabulate[Complex](R.cols, R.rows)((i, j) => Complex(R(i, j), I(i, j)))

  test("Complex Matrix with Positive Double  Power (Eigen)") {
    val testingResult = C ** power2
    val expectedResult = DenseMatrix( //result from Eigen
      (Complex(-25.48996, 85.05308), Complex(26.66656, 29.15491), Complex(22.82662, -82.67666), Complex(-13.70886, 45.77369)),
      (Complex(-22.69876, 24.18774), Complex(-52.39622, 29.45258), Complex(12.69271, -11.23466), Complex(-21.7223, 12.74252)),
      (Complex(-3.973367, -76.4725), Complex(-103.3028, 8.502425), Complex(-19.32138, 73.56532), Complex(7.523441, -34.40238)),
      (Complex(-14.14099, 41.26098), Complex(5.029781, 14.54831), Complex(37.6544, -49.56061), Complex(-24.46144, 55.50666))
    )
    val e = expectedResult.mapValues((i) => Complex((((i.real * 10000.0).round) / 10000.0), (((i.imag * 10000.0).round) / 10000.0)))
    val t = testingResult.mapValues((i) => Complex((((i.real * 10000.0).round) / 10000.0), (((i.imag * 10000.0).round) / 10000.0)))
    assert(e == t)
  }

  test("Double  Matrix with Negative Double Power (Eigen)") { //FROM EIGEN
    val testingResult = R ** power4
    val expectedResult = DenseMatrix(
      (Complex(0.1726269, -0.005872322), Complex(-0.1640955, -0.001971541), Complex(0.1390832, 0.004303419), Complex(-0.06512977, 0.02025381)),
      (Complex(-0.0374684, -0.008284294), Complex(0.1716692, -0.002294959), Complex(-0.01254684, 0.005453929), Complex(0.01755564, 0.02740364)),
      (Complex(0.1512625, 0.01069316), Complex(-0.1080234, 0.003205036), Complex(0.1697276, -0.007347791), Complex(-0.02493972, -0.03595549)),
      (Complex(-0.04928938, 0.03610269), Complex(0.03205089, 0.00962768), Complex(-0.01213825, -0.02329396), Complex(0.08711606, -0.1185259))
    )

    val e = expectedResult.mapValues((i) => Complex((((i.real * 10000.0).round) / 10000.0), (((i.imag * 10000.0).round) / 10000.0)))
    val t = testingResult.mapValues((i) => Complex((((i.real * 10000.0).round) / 10000.0), (((i.imag * 10000.0).round) / 10000.0)))

    assert(e == t)
  }

  test("Integer  Matrix with  Negative Integer Power (Matlab)") { //MATLAB
    val testingResult = I ** power3
    val expectedResult =     DenseMatrix(
      (7.288396429670672e-01  ,  -1.125884887657741e+00 ,  7.710064635272396e-01  ,   3.000923361034166e-01),
      (  -2.950138504155125e-01   ,  4.958448753462605e-01   ,   -3.144044321329641e-01,    -1.343490304709142e-01),
      (    1.397968605724838e+00   , -2.276084949215143e+00  ,   1.554016620498615e+00   ,  6.288088642659281e-01),
      (       9.041243459526008e-01  ,  -1.484918436441982e+00 ,   1.011542012927055e+00   ,  4.506001846722069e-01)
    )

    val e = expectedResult.mapValues((i) => (((i * 10000000000000.0).round) / 10000000000000.0))
    val t = testingResult.mapValues((i) => (((i.real * 10000000000000.0).round) / 10000000000000.0))

    assert(e == t)
  }

}
