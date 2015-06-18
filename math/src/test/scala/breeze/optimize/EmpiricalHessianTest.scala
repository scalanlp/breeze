package breeze.optimize

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.util.DoubleImplicits
import org.junit.runner.RunWith
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EmpiricalHessianTest extends OptimizeTestBase  with Matchers with DoubleImplicits {

  test("hessian using central difference") {
    val x = DenseVector(-1.2d, 1d)
    val A  = DenseMatrix((1330d, 480d), (480d, 200d))

    val f = new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]) = {
        val value = 100 * Math.pow(x(1) - x(0) * x(0), 2) + Math.pow(1 - x(0), 2)
        val grad = DenseVector(-400 * x(0) * (x(1) - x(0) * x(0)) - 2 * (1 - x(0)),
          200 * (x(1) - x(0) * x(0)))
        (value, grad)
      }
    }

    val y = f.calculate(x)
    val H = EmpiricalHessian.hessian(f, x)

    assert(H.size === A.size)
    for(i <- 0 until A.rows; j <- 0 until A.cols) {
      assert(H(i,j).closeTo(A(i,j)))
    }

  }
}
