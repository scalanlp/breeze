package breeze.linalg.functions

import breeze.linalg.{DenseMatrix, LU, sum, norm}
import org.scalatest.FunSuite

class LUTest extends FunSuite {

  test("Square LU decompose results in original matrix") {
    val a: DenseMatrix[Double] = DenseMatrix.rand(50, 50)
    var (lu, ipiv) = LU(a)
    var (p, l, u) = LU.decompose(lu, ipiv)
    val recon = p * l * u
    val diff = a - recon
    assert(norm(sum(diff)) <= 1E-10)

  }
}
