package breezetest

import breeze.linalg.DenseVector
import breeze.numerics.pow
import breeze.optimize.DiffFunction
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Administrator on 2015/3/14 0014.
 */
class LbfgsbTest extends FlatSpec with Matchers{

  "LbfgsbSolver" should "get the result with precision" in {
    val solver = new LbfgsbSolver()
    val x0 = DenseVector[Double](-1.2, 1.0)

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
    val (fv, gg) = f.calculate(DenseVector[Double](-1.056347, 1.122971));
    solver.hasLowerBound  = true
    solver.hasUpperBound = true
    solver.upperBounds = DenseVector[Double](100, 100);
    solver.lowerBounds = DenseVector[Double](-100, -100);
    val optX = solver.internalSolve(x0, f)
    val expectFx = 0.0
    f(optX) should equal (expectFx +- 1E-4)

  }

}
