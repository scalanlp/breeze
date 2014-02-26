package breeze.linalg

import org.scalatest.FunSuite

/**
 * TODO
 *
 * @author dlwh
 **/
class distanceTest extends FunSuite {
  test("simple distance test") {
    assert(distance(DenseVector(3.0, 4.0), DenseVector.zeros[Double](2)) === 5.0)
    assert(distance(DenseVector(3.0, 4.0), SparseVector.zeros[Double](2)) === 5.0)
  }

}
