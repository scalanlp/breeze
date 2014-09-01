package breeze.linalg.functions

import breeze.linalg.{SparseVector, DenseVector}
import org.scalatest.FunSuite


class tanimotoDistanceTest extends FunSuite {

  test("it should be correct") {
    val v1 = DenseVector(1.0, 2.0)
    val v2 = DenseVector(3.0, 4.0)

    val result = tanimotoDistance(v1, v2)
    val expected = 0.42105263157894735
    assert(result == expected, s"the result should be ${expected}, but actual ${result}")
  }

  test("it should be correct with sparse vectors") {
    val v1 = SparseVector(Array(1.0, 2.0))
    val v2 = SparseVector(Array(3.0, 4.0))

    val result = tanimotoDistance(v1, v2)
    val expected = 0.42105263157894735
    assert(result == expected, s"the result should be ${expected}, but actual ${result}")
  }

  test("it should be correct with dense vector and sparse vector") {
    val v1 = DenseVector(Array(1, 2.0))
    val v2 = SparseVector(Array(3.0, 4))

    val result = tanimotoDistance(v1, v2)
    val expected = 0.42105263157894735
    assert(result == expected, s"the result should be ${expected}, but actual ${result}")
  }
}
