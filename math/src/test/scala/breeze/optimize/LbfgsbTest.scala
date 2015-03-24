package breeze.optimize

import breeze.linalg.DenseVector
import breeze.numerics.pow
import breeze.optimize.DiffFunction
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Administrator on 2015/3/14 0014.
 */
class LBFGSBTest extends FlatSpec with Matchers{

  "Lbfgsb solve with bound constraint"  {
    val solver = new LBFGSB()
    val nearX0 = DenseVector[Double](-1.2, 1.0)

    val f = new DiffFunction[DenseVector[Double]] {
      override def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
        val cost = (1-x(0))*(1-x(0)) + 100*pow(x(1) - x(0)*x(0), 2)
        val grad = DenseVector.zeros[Double](x.length)
        grad(0) = -2.0*(1-x(0)) + 200*(x(1) - x(0)*x(0))*(-2.0*x(0))
        grad(1) = 200*(x(1) - x(0)*x(0))
        (cost, grad)
      }
    }

    /*val f = new DiffFunction[DenseVector[Double]] {
      override def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
        val cost = pow((x(0) - x(1)*x(1)), 2)/2.0 + (x(1) - 2.0)*(x(1) - 2.0) / 2.0;
        val grad = DenseVector.zeros[Double](x.length)
        grad(0) = x(0) - x(1)*x(1)
        grad(1) = -2.0*x(1)*(x(0) -x(1)*x(1)) + x(1) - 2.0
        (cost, grad)
      }
    }*/
    solver.hasLowerBound  = true
    solver.hasUpperBound = true
    solver.upperBounds = DenseVector[Double](100, 100);
    solver.lowerBounds = DenseVector[Double](-100, -100);
    var optX = solver.internalSolve(nearX0, f)
    val expectFx = 0.0
    f(optX) should equal (expectFx +- 1E-4)

    val farX0 = DenseVector[Double](12.0, 8.2)
    optX = solver.internalSolve(farX0, f)
    f(optX) should equal (expectFx +- 1E-4)

    0
  }

}
