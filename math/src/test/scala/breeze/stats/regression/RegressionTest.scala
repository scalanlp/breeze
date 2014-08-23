package breeze.stats.regression

import org.scalatest.{FunSuite, WordSpec}
import org.scalatest.Matchers
import scala.util.Random
import breeze.linalg._
import spire.implicits.cfor

class RegressionTest extends WordSpec with Matchers {
  "Regression" should {
    "do least squares for simple case" in {
      val a = DenseMatrix((1.0,1.0), (2.0, -2.0),(3.0, 3.0), (4.0, 5.0))
      val b = DenseVector(2.0, 0.0, 6.0, 9.0)
      val result = leastSquares(a,b)
      assert(norm(result.coefficients - DenseVector(1.0, 1.0)) < 1e-7)
      assert(result.rSquared < 1e-7)
    }
    "do least squares for trickier case" in {
      val a = new DenseMatrix[Double](100,2)
      val b = new DenseVector(new Array[Double](100))
      cfor(0)(i => i < 100, i => i+1)(i => {
        a.update(i,0, i)
        a.update(i,1, 1)
        b.update(i,2*i+5+math.random*0.01)
      })
      val result = leastSquares(a,b)
      assert(norm(result.coefficients - DenseVector(2.0, 5.0)) < 1e-2, "norm is too large")
      assert(result.rSquared < 1e-2, "rsquared too large")
    }
    "handle transpose" in {
      val a = DenseMatrix((1.0, 2.0, 3.0, 4.0), (1.0, -2.0, 3.0, 5.0)).t
      val b = DenseVector(2.0, 0.0, 6.0, 9.0)
      val result = leastSquares(a,b)
      assert(norm(result.coefficients - DenseVector(1.0, 1.0)) < 1e-7)
      assert(result.rSquared < 1e-7)
    }
    "preserve original arrays" in {
      val a = DenseMatrix((1.0,1.0), (2.0, -2.0),(3.0, 3.0), (4.0, 5.0))
      val aCopy = a.copy
      val b = DenseVector(2.0, 0.0, 6.0, 9.0)
      val bCopy = b.copy
      val result = leastSquares(a,b)
      assert(a == aCopy)
      assert(b == bCopy)
    }
  }
}
