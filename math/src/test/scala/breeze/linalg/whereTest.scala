package breeze.linalg

import org.scalatest.FunSuite

class whereTest extends FunSuite {
  test("DenseVector") {
    assert(where(DenseVector(1, 2, 0, 4, 5, 0)) === IndexedSeq(0, 1, 3, 4))
  }

  test("SparseVector") {
    assert(where(SparseVector(100)(0 -> 1, 1 -> 2, 3 -> 4, 4 -> 5)) === IndexedSeq(0, 1, 3, 4))
  }

  test("BitVector") {
    assert(where(DenseVector(1, 2, 0, 4, 5, 0) :== 0) === IndexedSeq(2, 5))
  }

  test("DenseVector 3 arg") {
    assert(
      where(DenseVector(1, 2, 0, 4, 5, 0), DenseVector(1, 2, 3, 4, 5, 6), DenseVector(-1, -2, -3, -4, -5, -6)) === DenseVector(
          1, 2, -3, 4, 5, -6))
  }

  test("BitVector 3 arg") {
    assert(
      where(DenseVector(1, 2, 0, 4, 5, 0) :!= 0, DenseVector(1, 2, 3, 4, 5, 6), DenseVector(-1, -2, -3, -4, -5, -6))(
        where.where3ArgFromTraverseKeyValuePairs) === DenseVector(1, 2, -3, 4, 5, -6))
  }

}
