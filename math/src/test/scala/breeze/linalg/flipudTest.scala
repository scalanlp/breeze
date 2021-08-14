package breeze.linalg

import org.scalatest._
import org.scalatest.funsuite._

class flipudTest extends AnyFunSuite {
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
