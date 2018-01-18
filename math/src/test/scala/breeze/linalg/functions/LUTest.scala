package breeze.linalg.functions

import breeze.linalg.LU.lu
import breeze.linalg.{DenseMatrix, LU, norm, sum}
import breeze.numerics.abs
import org.scalatest.FunSuite

class LUTest extends FunSuite {

  test("Square LU decompose results in original matrix") {
    val a: DenseMatrix[Double] = DenseMatrix.rand(50, 50)
    var (lum, ipiv) = LU(a)
    var lu(p, l, u) = LU.decompose(lum, ipiv)
    val recon = p * l * u
    val diff = abs(a - recon)
    assert(norm(sum(diff)) <= 1E-10)

  }

}
