package breeze.linalg

import org.scalatest.funsuite.AnyFunSuite

class TransposeTest extends AnyFunSuite {
  test("Counter1 inner product") {
    val c = Counter("a" -> 1.0)
    assert((c.dot(c)) === c.t * c)
  }

  test("Counter addition product") {
    val c = Counter("a" -> 1.0)
    assert((c.t + c.t).t === (c + c))
  }

}
