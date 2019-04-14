package breeze.optimize.linear

import breeze.linalg._
import org.scalatest._

class ConjugateGradientTest extends FunSuite {

  test("we can recover the same matrix as inversion, more or less") {

    // note that min a dot x + x dot (B * x) has the same solution as B \ a

    val a = DenseVector(10.0, 7.0)
    val b = DenseMatrix((4.0, -3.0), (-3.0, 2.0))
    val answer = b \ a
    val cg = new ConjugateGradient[DenseVector[Double], DenseMatrix[Double]]()
    val result = cg.minimize(a, b, DenseVector.zeros[Double](2))

    assert(norm(answer - result) <= 1E-4)
  }

  test("norm constraint is obeyed") {
    val a = DenseVector(10.0, 7.0)
    val b = DenseMatrix((4.0, -3.0), (-3.0, 2.0))
    val answer = b \ a
    val cg = new ConjugateGradient[DenseVector[Double], DenseMatrix[Double]](maxNormValue = 10)
    val result = cg.minimize(a, b, DenseVector.zeros[Double](2))

    assert(norm(result) <= 10.00001)

  }

}
