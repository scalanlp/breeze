package breeze.optimize

import breeze.linalg.{DenseMatrix, DenseVector, sum}
import breeze.numerics.exp
import breeze.util.DoubleImplicits
import org.scalatest.matchers.should.Matchers._

class EmpiricalHessianTest extends OptimizeTestBase with DoubleImplicits {

  test("hessian matrix of rosenbrook function") {
    val x = DenseVector(1d, 1d)
    val A = DenseMatrix((802d, -400d), (-400d, 200d))

    val f = new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]) = {
        val n = x.length
        val value = 100 * Math.pow(x(1) - x(0) * x(0), 2) + Math.pow(1 - x(0), 2)
        val grad = DenseVector(-400 * x(0) * (x(1) - x(0) * x(0)) - 2 * (1 - x(0)), 200 * (x(1) - x(0) * x(0)))
        (value, grad)
      }
    }

    val H = EmpiricalHessian.hessian(f, x)

    assert(H.size === A.size)
    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      assert(H(i, j).closeTo(A(i, j)))
    }
  }

  test("hessian matrix of an exponential function") {
    val x = DenseVector(3.56, -1.09, -0.31, 1.12, -1.52)
    val A = DenseMatrix(
      (35.1632d, 0d, 0d, 0d, 0d),
      (0d, 0.3362165d, 0d, 0d, 0d),
      (0d, 0d, 0.733447d, 0d, 0d),
      (0d, 0d, 0d, 3.064854d, 0d),
      (0d, 0d, 0d, 0d, 0.2187119d))

    val f = new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]) = {
        val n = x.length
        val value = sum(exp(x) - x) / n
        val grad = exp(x) - 1d
        (value, grad)
      }
    }

    val H = EmpiricalHessian.hessian(f, x)

    assert(H.size === A.size)
    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      assert(H(i, j).closeTo(A(i, j)))
    }
  }

}
