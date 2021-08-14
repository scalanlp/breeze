package breeze.linalg

import org.scalatest.funsuite.AnyFunSuite

class convertTest extends AnyFunSuite {
  test("Int -> {Double, Short}") {
    assert(convert(Array(1, 2, 3), Double).toIndexedSeq == IndexedSeq(1.0, 2.0, 3.0))
    assert(convert(Array(1, 2, 3), Short).toIndexedSeq == IndexedSeq[Short](1, 2, 3))
    assert(convert(DenseVector(1, 2, 3), Long) == DenseVector(1L, 2L, 3L))
  }

  test("Double -> {Double, Short, Int}") {
    assert(convert(Array(1.0, 2.0, 3.0), Double).toIndexedSeq == IndexedSeq(1.0, 2.0, 3.0))
    assert(convert(Array(1.0, 2.0, 3.0), Short).toIndexedSeq == IndexedSeq[Short](1, 2, 3))
    assert(convert(DenseVector(1.0, 2.0, 3.0), Int) == DenseVector(1, 2, 3))
  }

}
