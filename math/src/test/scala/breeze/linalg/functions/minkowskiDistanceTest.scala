package breeze.linalg.functions

import breeze.linalg.{SparseVector, DenseVector}
import org.scalatest.FunSuite

class minkowskiDistanceTest extends FunSuite {

  test("it should be correct with dense vectors") {
    val v1 = DenseVector(1.0, 1.0)
    val v2 = DenseVector(9.0, -9.0)

    val result = minkowskiDistance(v1, v2, 3.0)
    val expected = 7.872994366204345
    assert(result == expected, s"the result should be ${expected}, but actual ${result}")
  }

  test("it should be correct with sparse vectors") {
    val v1 = SparseVector(1.0, 1.0)
    val v2 = SparseVector(9.0, -9.0)

    val result = minkowskiDistance(v1, v2, 3.0)
    val expected = 7.872994366204345
    assert(result == expected, s"the result should be ${expected}, but actual ${result}")
  }

  test("it should be correct with dense vector and sparse vector") {
    val v1 = DenseVector(1, 1.0)
    val v2 = SparseVector(9.0, -9)

    val result = minkowskiDistance(v1, v2, 3.0)
    val expected = 7.872994366204345
    assert(result == expected, s"the result should be ${expected}, but actual ${result}")
  }
}
