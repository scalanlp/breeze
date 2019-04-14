package breeze.linalg

import org.scalatest._

class fliplrTest extends FunSuite {
  test("fliplr is its own inverse") {
    val m = DenseMatrix.rand(10, 8)
    assert(fliplr(m) != m)
    assert(fliplr(fliplr(m)) == m)
  }

}
