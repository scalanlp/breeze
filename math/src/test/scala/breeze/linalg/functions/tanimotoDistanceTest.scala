package breeze.linalg

import breeze.linalg.{SparseVector, DenseVector}
import org.scalatest.funsuite.AnyFunSuite

class tanimotoDistanceTest extends AnyFunSuite {

  test("it should be correct") {
    val v1 = DenseVector(1.0, 2.0)
    val v2 = DenseVector(3.0, 4.0)
    assert(tanimotoDistance(v1, v2) === 0.42105263157894735)
  }

  test("it should be correct with sparse vectors") {
    val v1 = SparseVector(Array(1.0, 2.0))
    val v2 = SparseVector(Array(3.0, 4.0))
    assert(tanimotoDistance(v1, v2) === 0.42105263157894735)
  }

  test("it should be correct with dense vector and sparse vector") {
    val v1 = DenseVector(Array(1, 2.0))
    val v2 = SparseVector(Array(3.0, 4))
    assert(tanimotoDistance(v1, v2) === 0.42105263157894735)
  }
}
