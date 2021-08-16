package breeze.linalg

import breeze.numerics._
import org.scalatest.funsuite.AnyFunSuite

class cosineDistanceTest extends AnyFunSuite {

  test("it should be correct with dense vectors") {
    val v1 = DenseVector(1.0, 2.0)
    val v2 = DenseVector(3.0, 4.0)
    assert(closeTo(cosineDistance(v1, v2), 0.01613))
  }

  test("it should be correct with sparse vectors") {
    val v1 = SparseVector(Array(1.0, 2.0))
    val v2 = SparseVector(Array(3.0, 4.0))
    assert(closeTo(cosineDistance(v1, v2), 0.01613))
  }

  test("it should be correct with dense vector and sparse vector") {
    val v1 = DenseVector(1.0, 2.0)
    val v2 = SparseVector(Array(3.0, 4.0))
    assert(closeTo(cosineDistance(v1, v2), 0.01613))
  }

  test("DV/DV Float") {
    val v1 = DenseVector(1.0f, 2.0f)
    val v2 = DenseVector(3.0f, 4.0f)
    assert(closeTo(cosineDistance(v1, v2), 0.01613))
  }

  test("SV/SV Float") {
    val v1 = SparseVector(Array(1.0f, 2.0f))
    val v2 = SparseVector(Array(3.0f, 4.0f))
    assert(closeTo(cosineDistance(v1, v2), 0.01613))
  }

  test("DV / SV Float") {
    val v1 = DenseVector(1.0f, 2.0f)
    val v2 = SparseVector(Array(3.0f, 4.0f))
    assert(closeTo(cosineDistance(v1, v2), 0.01613))
  }
}
