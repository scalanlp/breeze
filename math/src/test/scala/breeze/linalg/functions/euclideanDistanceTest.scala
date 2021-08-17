package breeze.linalg


import org.scalatest.funsuite.AnyFunSuite

class euclideanDistanceTest extends AnyFunSuite {

  test("it should be correct with dense vectors") {
    val v1 = DenseVector(1.0, 1.0)
    val v2 = DenseVector(9.0, -9.0)
    assert(euclideanDistance(v1, v2) === 12.806248474865697)
  }

  test("it should be correct with sparse vectors") {
    val v1 = SparseVector(Array(1.0, 1.0))
    val v2 = SparseVector(Array(9.0, -9.0))
    assert(euclideanDistance(v1, v2) === 12.806248474865697)
  }

  test("it should be correct with dense vector and sparse vector") {
    val v1 = DenseVector(1, 1.0)
    val v2 = SparseVector(Array(9.0, -9))
    assert(euclideanDistance(v1, v2) === 12.806248474865697)
  }
}
