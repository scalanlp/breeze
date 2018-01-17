package breeze.polynomial

/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
 */

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import breeze.linalg.{DenseVector, DenseMatrix, norm}
import spire.math._
import spire.math.poly._
import spire.algebra._
import spire.implicits._

/**
 *
 * @author dlw
 */
@RunWith(classOf[JUnitRunner])
class DensePolynomialTest extends FunSuite {

  test("PolyDenseUfuncWrapper applied to doubles") {
    // Since polynomial has it's own .apply method, this is not very interesting.
    val p = Polynomial.dense(Array[Double](1, 2, 4))
    assert(p(0.0) == 1.0)
    assert(p(0.5) == 3.0)
    assert(p(1.0) == 7.0)
  }

  test("PolyDenseUfuncWrapper applied to densevector") {
    val M = 10000000
    val p = Polynomial.dense(Array[Double](1, 2, 4, 1, 2))
    val x = DenseVector.zeros[Double](M)
    val result = DenseVector.zeros[Double](M)
    cfor(0)(j => j < M, j => j + 1)(j => {
      val t = j / M.toDouble
      x.update(j, t)
      result.update(j, 1 + 2 * t + 4 * t * t + 1 * t * t * t + 2 * t * t * t * t)
    })
    assert(norm(p(x) - result) < 1e-10)
  }

  test("PolyDenseUfuncWrapper applied to diagonal dense matrix") {
    val x = DenseMatrix.eye[Double](3) * 0.5
    val p = Polynomial.dense(Array[Double](1, 2, 4))
    val diff = p(x) - (DenseMatrix.eye[Double](3) * 3.0)
    assert(norm(diff.toDenseVector) < 1e-10)
  }
  test("PolyDenseUfuncWrapper applied to subdiagonal dense matrix") {
    val p = Polynomial.dense(Array[Double](1, 2, 4))
    var M = 100

    val x = DenseMatrix.zeros[Double](M, M)
    cfor(0)(i => i < M, i => i + 1)(i => { //   x is matrix with 1's just below the diagonal
      cfor(0)(j => j < M, j => j + 1)(j => { // so x*x is matrix with row of 1's 2 below the diagonal, etc
        if (j == i - 1) {
          x.update(i, j, 1.0)
        }
      })
    })

    val expectedResult = DenseMatrix.zeros[Double](M, M) // expected result easy to compute
    cfor(0)(i => i < M, i => i + 1)(i => {
      cfor(0)(j => j < M, j => j + 1)(j => {
        if (j == i) { expectedResult.update(i, j, 1.0) }
        if (j == i - 1) { expectedResult.update(i, j, 2.0) }
        if (j == i - 2) { expectedResult.update(i, j, 4.0) }
      })
    })
    val diff = p(x) - expectedResult
    assert(norm(diff.toDenseVector) < 1e-10)
  }
}
