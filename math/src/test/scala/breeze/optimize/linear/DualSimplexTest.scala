package breeze.optimize.linear

import breeze.linalg.{norm, DenseMatrix, DenseVector}
import breeze.optimize.linear.DualSimplex.ComputationalForm
import org.scalatest.FunSuite
import breeze.numerics._

/**
  * Created by dlwh on 12/24/15.
  */
class DualSimplexTest extends FunSuite {

  test("basic test") {
    val input = ComputationalForm(
      DenseVector(0.5, 3.0, 1.0, 4.0, 0.0, 0.0, 0.0),
      DenseMatrix(
        ( 1.0,  1.0,  1.0,  1.0, 1.0, 0.0, 0.0),
        (-2.0, -1.0,  1.0,  1.0, 0.0, 1.0, 0.0),
        ( 0.0,  1.0,  0.0, -1.0, 0.0, 0.0, 1.0)
      ),
      DenseVector(40.0, -10.0, -10.0),
      DenseVector.zeros(7),
      DenseVector.fill(7)(inf)
    )

    val target = DenseVector(10.0, 0.0, 0.0, 10.0, 20.0, 0.0, 0.0)

    val result  = DualSimplex.minimize(input)
    assert(norm(result.x - target) < 1E-4)
  }

  test("second") {
    val input = ComputationalForm(
      DenseVector(-1.0, -2.0, -3.0, 0.0, 0.0),
      DenseMatrix(
        ( -1.0,  1.0,  1.0,  1.0, 0.0),
        ( 1.0, -3.0,  1.0,  0.0, 1.0)
      ),
      DenseVector(20.0, 30.0),
      DenseVector.zeros(5),
      DenseVector(40.0, inf, inf, inf, inf)
    )
    val target = DenseVector(40.0, 17.5, 42.5)
    val result = DualSimplex.minimize(input)
    assert(norm(result.x(0 until 3) - target) < 1E-4)
  }

}
