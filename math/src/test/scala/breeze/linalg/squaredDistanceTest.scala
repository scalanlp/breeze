package breeze.linalg

import org.scalatest.funsuite.AnyFunSuite

/**
 * TODO
 *
 * @author dlwh
 **/
class squaredDistanceTest extends AnyFunSuite {
  test("simple squared distance test") {
    assert(squaredDistance(DenseVector(3.0, 4.0), DenseVector.zeros[Double](2)) === 25.0)
    assert(squaredDistance(DenseVector(3.0, 4.0), SparseVector.zeros[Double](2)) === 25.0)
  }
}
