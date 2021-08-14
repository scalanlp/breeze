package breeze.linalg

import org.scalatest._
import org.scalatest.funsuite._

/**
 *
 *
 * @author dlwh
 */
class rot90Test extends AnyFunSuite {
  test("rot90 numpy docs test") {
    val m = DenseMatrix((1, 2), (3, 4))
    val r90 = DenseMatrix((2, 4), (1, 3))
    val r180 = DenseMatrix((4, 3), (2, 1))
    assert(rot90(m) === r90)
    assert(rot90(m, 2) === r180)
    assert(rot90(m, 4) === m)
    assert(rot90(m, -3) === r90)
    assert(rot90(m, -2) === r180)
  }

}
