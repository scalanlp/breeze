package breeze.linalg.functions

import breeze.linalg.{SparseVector, DenseVector}
import org.scalatest.FunSuite

class euclideanDistanceTest extends FunSuite {

  test("it should be correct with dense vectors") {
    val v1 = DenseVector(1.0, 1.0)
    val v2 = DenseVector(9.0, -9.0)

    val result = euclideanDistance(v1, v2)
    val expected = 12.806248474865697
    assert(result == expected, s"the result should be ${expected}, but actual ${result}")
  }

  test("it should be correct with sparse vectors") {
    val v1 = SparseVector(Array(1.0, 1.0))
    val v2 = SparseVector(Array(9.0, -9.0))

    val result = euclideanDistance(v1, v2)
    val expected = 12.806248474865697
    assert(result == expected, s"the result should be ${expected}, but actual ${result}")
  }

  test("it should be correct with dense vector and sparse vector") {
    val v1 = DenseVector(1, 1.0)
    val v2 = SparseVector(Array(9.0, -9))

    val result = euclideanDistance(v1, v2)
    val expected = 12.806248474865697
    assert(result == expected, s"the result should be ${expected}, but actual ${result}")
  }
}
