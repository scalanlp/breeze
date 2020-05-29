package breeze.stats.regression

import org.scalatest.{FunSuite, WordSpec}
import org.scalatest.Matchers
import scala.util.Random
import breeze.linalg._
import spire.implicits.cfor

class LassoTest extends WordSpec with Matchers {
  "Lasso least squares" should {
    "handle simple case" in {
      val a = DenseMatrix((1.0, 1.0), (2.0, -2.0), (3.0, 3.0), (4.0, 5.0))
      val b = DenseVector(1.01, 1.98, 3.03, 4.05)
      val result = lasso(a, b, 0.05)
      assert(
        norm(result.coefficients - DenseVector(0.9553, 0.0)) < 1e-2,
        "coefficients disagreed"
      ) //Agrees with numpy to 2 decimal places
      assert(
        result.coefficients(1) == 0,
        "failed to be sparse"
      ) //Small coefficient on second coefficient should be thresholded away
      assert(0 < result.rSquared, "rsquared will not be zero, that's crazy")
    }
    "handle trickier case" in {
      val a = new DenseMatrix[Double](100, 5)
      val b = new DenseVector(new Array[Double](100))
      cfor(0)(i => i < 100, i => i + 1)(i => {
        a.update(i, 0, 1)
        a.update(i, 1, i)
        a.update(i, 2, i * i)
        a.update(i, 3, i * i * i)
        a.update(i, 4, i * i * i * i)
        b.update(i, 5.0 + 2 * i + math.random * 0.01 + 0.00001 * i * i)
      })
      val result = lasso(a, b, 0.0001)
      assert(norm(result.coefficients - DenseVector(5.0, 2.0, 0.0, 0.0, 0.0)) < 1e-2, "norm is too large")
    }
    "handle transpose" in {
      val a = DenseMatrix((1.0, 2.0, 3.0, 4.0), (1.0, -2.0, 3.0, 5.0)).t
      val b = DenseVector(2.0, 0.0, 6.0, 9.0)
      val result = lasso(a, b, 0.0001)
      assert(norm(result.coefficients - DenseVector(1.0, 1.0)) < 1e-4, "coefficients incorrect")
    }
    "preserve original arrays" in {
      val a = DenseMatrix((1.0, 1.0), (2.0, -2.0), (3.0, 3.0), (4.0, 5.0))
      val aCopy = a.copy
      val b = DenseVector(2.0, 0.0, 6.0, 9.0)
      val bCopy = b.copy
      val result = lasso(a, b, 0.01)
      assert(a == aCopy)
      assert(b == bCopy)
    }
  }
}
