package breeze.linalg.functions

import breeze.linalg.{ DenseMatrix, LU, norm, sum }
import breeze.numerics.abs
import breeze.stats.distributions.RandBasis
import org.scalatest.FunSuite

class LUTest extends FunSuite {

  test("Square LU decompose results in original matrix") {
    implicit val basis = RandBasis.mt0
    val a: DenseMatrix[Double] = DenseMatrix.rand(50, 50)
    val LU.LU(p, l, u) = LU(a)
    val recon = p * l * u
    val diff = abs(a - recon)
    assert(norm(sum(diff)) <= 1E-10)
  }

  test("Square LU decompose results in original matrix - float") {
    implicit val basis = RandBasis.mt0
    val a: DenseMatrix[Float] = DenseMatrix.rand(50, 50).mapValues(_.toFloat)
    val LU.LU(p, l, u) = LU(a)
    val recon = p * l * u
    val diff = abs(a - recon)
    assert(norm(sum(diff)) <= 1E-3)
  }

}
