package breeze.linalg

import org.scalatest.FunSuite

/**
 * TODO
 *
 * @author dlwh
 **/
class TransposeTest extends FunSuite {
  test("Counter1 inner product") {

    val c = Counter('a -> 1.0)
    assert((c.dot(c)) === c.t * c)

  }

  test("Counter addition product") {

    val c = Counter('a -> 1.0)
    assert((c.t + c.t).t === (c + c))

  }

}
