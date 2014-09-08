package breeze.linalg.functions

import breeze.linalg.{SparseVector, DenseVector}
import org.scalatest.FunSuite

class chebyshevDistanceTest extends FunSuite {

  test("it should be correct with dense vectors") {
    val v1 = DenseVector(1.0, 1.0)
    val v2 = DenseVector(9.0, -9.0)
    assert(chebyshevDistance(v1, v2) === 10.0)
  }

  test("it should be correct with sparse vectors") {
    val v1 = SparseVector(Array(1.0, 1.0))
    val v2 = SparseVector(Array(9.0, -9.0))
    assert(chebyshevDistance(v1, v2) === 10.0)
  }

  test("it should be correct with dense vector and sparse vector") {
    val v1 = DenseVector(1, 1.0)
    val v2 = SparseVector(Array(9.0, -9))
    assert(chebyshevDistance(v1, v2) === 10.0)
  }
}
