package breeze.numerics

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class IntMathTest extends FunSuite {
  test("ipow") {
    import IntMath._
    assert(ipow(3, 4) === 81)
    assert(ipow(3, 1) === 3)
    assert(ipow(3, 0) === 1)
    assert(ipow(3, 3) === 27)
    assert(ipow(3, 12) === 531441)
  }

}
