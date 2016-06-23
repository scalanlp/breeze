/* The MIT License
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
import breeze.linalg.householder
import org.scalatest.FunSuite
import breeze.linalg._
import breeze.math._
import java.text._
import DenseMatrix.canMapValues

class householderTest extends FunSuite {

  val requiredresult = DenseMatrix(
    (4.0, -3.0, 0.0, 0.0),
    (-3.0, 3.333333333333, -1.666666666667, 0.0),
    (0.0, -1.666666666667, -1.32, 0.906666666667),
    (0.0, 0.0, 0.906666666667, 1.986666666667)
  )

  test("Householder from Numerical Analysis Book") {
    /*Example 4. This example is taken from the book "Numerical Analysis" by Richard L. Burden (Author), J. Douglas Faires.
     *In this example, the given matrix is transformed to the similar tridiagonal matrix A2 by using the Householder method.
     */

    val A = DenseMatrix((4, 1, -2, 2), (1, 2, 0, 1), (-2, 0, 3, -2), (2, 1, -2, -1)).mapValues(Complex(_, 0))
    val P = householder(A, 0).P
    val A1 = P * A * P
    //shifted 1
    val P1 = householder(A1, 1).P
    val ans = P1 * A1 * P1
    assert(ans.mapValues((i) => ((i.real * 10000.0).round) / 10000.0) == requiredresult.mapValues((i) => ((i * 10000.0).round) / 10000.0))
  }

  test("Householder from Numerical Analysis Book 2") {

    //this is equivalent to ... ( if you simply want the tridiagonal)
    val A = DenseMatrix((4, 1, -2, 2), (1, 2, 0, 1), (-2, 0, 3, -2), (2, 1, -2, -1)).mapValues(Complex(_, 0))
    val ans = householder.triDiagonalize(A)
    assert(ans.mapValues((i) => ((i.real * 10000.0).round) / 10000.0) == requiredresult.mapValues((i) => ((i * 10000.0).round) / 10000.0))
  }
}

