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

class schurTest extends FunSuite {

  test("Schur Test Random Double Matrix") {

    val RndM = DenseMatrix.rand(4, 4)
    val (p1, h1) = schur(RndM)
    val ans = (p1 * h1 * p1.t).mapValues((i) => ((i.real * 100000.0).round) / 100000.0)
    assert(ans == RndM.mapValues((i) => ((i * 100000.0).round) / 100000.0))
  }


 test("Schur Test Random Complex  Matrix") {

    val RndR = DenseMatrix.rand(4, 4)
    val RndI = DenseMatrix.rand(4, 4)

      val C = DenseMatrix.tabulate[Complex](RndI.cols, RndI.rows)((i, j) => Complex(RndI(i, j), RndI(i, j)))

    val (p1, h1) = schur(C)
    val ans = (p1 * h1 * p1.t).mapValues((i) => (Complex((((i.real * 100000.0).round) / 100000.0),(((i.imag * 100000.0).round) / 100000.0))))
    assert(ans == C.mapValues((i) =>  (Complex((((i.real * 100000.0).round) / 100000.0),(((i.imag * 100000.0).round) / 100000.0)))))
  }
}