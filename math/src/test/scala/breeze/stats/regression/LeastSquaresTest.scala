package breeze.stats.regression

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.Matchers
import breeze.linalg._
import breeze.numerics._
import breeze.macros.cforRange

class LeastSquaresTest extends AnyWordSpec {
  "Least squares" should {
    "handle simple case" in {
      val a = DenseMatrix((1.0, 1.0), (2.0, -2.0), (3.0, 3.0), (4.0, 5.0))
      val b = DenseVector(2.0, 0.0, 6.0, 9.0)
      val result = leastSquares(a, b)
      val residual = sum(pow(b - result(a), 2))
      assert(norm(result.coefficients - DenseVector(1.0, 1.0)) < 1e-7)
      assert(math.abs(result.rSquared - residual) < 1e-7)
      assert(result.rSquared < 1e-7)
    }
    "handle trickier case" in {
      val a = new DenseMatrix[Double](100, 2)
      val b = new DenseVector(new Array[Double](100))
      cforRange(0 until 100)(i => {
        a.update(i, 0, i)
        a.update(i, 1, 1)
        b.update(i, 2 * i + 5 + math.random * 0.01)
      })
      val result = leastSquares(a, b)
      val residual = sum(pow(b - result(a), 2))
      assert(norm(result.coefficients - DenseVector(2.0, 5.0)) < 1e-2, "norm is too large")
      assert(math.abs(result.rSquared - residual) < 1e-7)
      assert(result.rSquared < 1e-2, "rsquared too large")
    }
    "handle transpose" in {
      val a = DenseMatrix((1.0, 2.0, 3.0, 4.0), (1.0, -2.0, 3.0, 5.0)).t
      val b = DenseVector(2.0, 0.0, 6.0, 9.0)
      val result = leastSquares(a, b)
      val residual = sum(pow(b - result(a), 2))
      assert(norm(result.coefficients - DenseVector(1.0, 1.0)) < 1e-7)
      assert(math.abs(result.rSquared - residual) < 1e-7)
      assert(result.rSquared < 1e-7)
    }
    "preserve original arrays" in {
      val a = DenseMatrix((1.0, 1.0), (2.0, -2.0), (3.0, 3.0), (4.0, 5.0))
      val aCopy = a.copy
      val b = DenseVector(2.0, 0.0, 6.0, 9.0)
      val bCopy = b.copy
      leastSquares(a, b)
      assert(a == aCopy)
      assert(b == bCopy)
    }
    "correctly compute rSquared" in {
      val a = DenseMatrix(1.0, 2.0, -1.0, -2.0, 1.0, 2.0, -1.0, -2.0)
      val b = DenseVector(1.0, 2.0, 1.0, 2.0, -1.0, -2.0, -1.0, -2.0)
      val result = leastSquares(a, b)
      val residual = sum(pow(b - result(a), 2))
      assert(norm(result.coefficients - DenseVector(0.0)) < 1e-7)
      assert(math.abs(result.rSquared - residual) < 1e-7)
    }
  }
}
