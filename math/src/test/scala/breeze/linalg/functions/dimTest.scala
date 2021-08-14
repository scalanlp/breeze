package breeze.linalg.functions

import breeze.linalg.{DenseVector, dim, DenseMatrix}
import org.scalatest.funsuite.AnyFunSuite

/**
 * breeze
 * 7/19/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 *
 */
class dimTest extends AnyFunSuite {

  test("dim ( DenseMatrix )") {
    val mbig = DenseMatrix((0, 1, 2, 3, 4, 5), (3, 4, 5, 6, 7, 8), (3, 4, 5, 6, 7, 8), (5, 4, 5, 9, 7, 8))
    assert(dim(mbig) == (4, 6))
  }

  test("dim ( DenseVector )") {
    val dv = DenseVector(1, 2, 3, 4, 5, 6, 7, 8)
    assert(dim(dv) == 8)
  }
}
