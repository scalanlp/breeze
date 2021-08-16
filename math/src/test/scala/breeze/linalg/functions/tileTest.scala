package breeze.linalg

import breeze.linalg.{DenseVector, tile, DenseMatrix}
import org.scalatest.funsuite.AnyFunSuite

/**
 * 2/21/15.
 * @author Rakesh Chalasani
 */
class tileTest extends AnyFunSuite {
  test("tile ( DenseMatrix , Int)") {
    val m = new DenseMatrix(2, 2, Array.range(0, 4))
    assert(tile(m, 2) == new DenseMatrix[Int](4, 2, Array(0, 1, 0, 1, 2, 3, 2, 3)))
  }

  test("tile ( DenseMatrix , Int, Int)") {
    val m = new DenseMatrix(2, 2, Array.range(0, 4))
    assert(tile(m, 2, 2) == new DenseMatrix[Int](4, 4, Array(0, 1, 0, 1, 2, 3, 2, 3, 0, 1, 0, 1, 2, 3, 2, 3)))
  }

  test("tile ( DenseMatrix , Int, Int) non-square matrix.") {
    val m = new DenseMatrix(1, 2, Array.range(0, 2))
    assert(tile(m, 2, 3) == new DenseMatrix[Int](2, 6, Array(0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1)))
  }

  test("tile (DenseVector, Int) ") {
    val v = DenseVector(1, 2, 3, 4)
    assert(tile(v, 2) == DenseVector[Int](1, 2, 3, 4, 1, 2, 3, 4))
  }

  test("tile (DenseVector, Int, Int) ") {
    val v = DenseVector(1, 2, 3, 4)
    assert(
      tile(v, 2, 3) == new DenseMatrix[Int](
        8,
        3,
        Array(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4)))
  }
}
