package breeze.optimize

import breeze.linalg.{norm, DenseVector}
import breeze.numerics.pow
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by fanming.chen on 2015/3/14 0014.
 */
@RunWith(classOf[JUnitRunner])
class LBFGSBTest extends OptimizeTestBase{
  val EPS = 1E-4;

  test("L-BFGS-B should solve with bound constraint") {
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

    solver.upperBounds = DenseVector[Double](1200, 100);
    solver.lowerBounds = DenseVector[Double](-100, -100);
    var optX = solver.minimize(f, nearX0)

    val expectFx = 0.0
    assert(norm(f(optX)) < EPS)

    val farX0 = DenseVector[Double](1200.0, 8.2)
    assert(solver.minimizeAndReturnState(f, nearX0).convergedReason == Some(FirstOrderMinimizer.FunctionValuesConverged))
    assert(norm(f(optX)) < EPS)
  }

  test("L-BFGS-B should solve with unbound constraint"){
    val solver = new LBFGSB()
    val nearX0 = DenseVector[Double](-1.2, 1.0)

    val f = new DiffFunction[DenseVector[Double]] {
      override def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
        val cost = pow((x(0) - x(1)*x(1)), 2)/2.0 + (x(1) - 2.0)*(x(1) - 2.0) / 2.0;
        val grad = DenseVector.zeros[Double](x.length)
        grad(0) = x(0) - x(1)*x(1)
        grad(1) = -2.0*x(1)*(x(0) -x(1)*x(1)) + x(1) - 2.0
        (cost, grad)
      }
    }

    var optX = solver.minimize(f, nearX0)
    val expectFx = 0.0
    assert(norm(f(optX)) < EPS)

    val farX0 = DenseVector[Double](12.0, 8.2)
    optX = solver.minimize(f, farX0)
    assert(norm(f(optX)) < EPS)
  }

}
