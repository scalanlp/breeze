package breeze.linalg

import org.scalatest._

class flipudTest extends FunSuite {
  test("flipud is its own inverse") {
    val m = DenseMatrix.rand(10, 8)
    assert(flipud(m) != m)
    assert(flipud(flipud(m)) == m)
  }

  test("flipud is its own inverse dv") {
    val m = DenseVector.rand(10)
    assert(flipud(m) != m)
    assert(flipud(flipud(m)) == m)
  }
}
