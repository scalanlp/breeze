package breeze.linalg

import org.scalatest.FunSuite

/**
 * TODO
 *
 * @author dlwh
 **/
class distanceTest extends FunSuite {
  test("simple squared distance test") {
    assert(squaredDistance(DenseVector(3.0, 4.0), DenseVector.zeros[Double](2)) === 25.0)
    assert(squaredDistance(DenseVector(3.0, 4.0), SparseVector.zeros[Double](2)) === 25.0)
  }
}
