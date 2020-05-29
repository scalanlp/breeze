package breeze.linalg

import org.scalatest._
import org.scalatestplus.scalacheck._
import spire.implicits._

/**
 *
 * @author stucchio
 */
class splitTest extends FunSuite {
  test("split works on arrays with even multiple") {
    val start = DenseVector[Double](1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    val expectedResult =
      Seq(DenseVector[Double](1, 2, 3, 4), DenseVector[Double](5, 6, 7, 8), DenseVector[Double](9, 10, 11, 12))
    assert(split(start, 3) == expectedResult)
  }

  test("throws exception when vector.size is not divisible by number of elements in split") {
    intercept[IllegalArgumentException] {
      split(DenseVector[Double](1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), 3)
    }
  }

  test("split works on arrays with sequence argument multiple") {
    val start = DenseVector[Double](1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    val expectedResult =
      Seq(DenseVector[Double](1, 2, 3), DenseVector[Double](4, 5, 6, 7, 8), DenseVector[Double](9, 10, 11, 12))
    assert(split(start, Seq(3, 8)) == expectedResult)
  }

  test("hsplit works on dense matrix") {
    val mbig = DenseMatrix(
      (0, 1, 2, 3, 4, 5),
      (3, 4, 5, 6, 7, 8),
      (3, 4, 5, 6, 7, 8),
      (5, 4, 5, 9, 7, 8)
    )
    val expectedResult = List(
      DenseMatrix(
        (0, 1, 2),
        (3, 4, 5),
        (3, 4, 5),
        (5, 4, 5)
      ),
      DenseMatrix(
        (3, 4, 5),
        (6, 7, 8),
        (6, 7, 8),
        (9, 7, 8)
      )
    )
    assert(hsplit(mbig, 2) == expectedResult)
  }
  test("vsplit works on dense matrix") {
    val mbig = DenseMatrix(
      (0, 1, 2, 3, 4, 5),
      (3, 4, 5, 6, 7, 8),
      (3, 4, 5, 6, 7, 8),
      (5, 4, 5, 9, 7, 8)
    )
    val expectedResult = List(
      DenseMatrix(
        (0, 1, 2, 3, 4, 5),
        (3, 4, 5, 6, 7, 8)
      ),
      DenseMatrix(
        (3, 4, 5, 6, 7, 8),
        (5, 4, 5, 9, 7, 8)
      )
    )
    assert(vsplit(mbig, 2) == expectedResult)
  }

  test("#459") {
    assert(split(DenseVector(0.0, 1.0), 2) === IndexedSeq(DenseVector(0.0), DenseVector(1.0)))
  }
}
