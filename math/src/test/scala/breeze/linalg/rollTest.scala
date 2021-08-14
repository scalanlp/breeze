package breeze.linalg

import org.scalatest._
import org.scalatest.funsuite._
import breeze.macros._

/**
 *
 *
 * @author stucchio
 */
class rollTest extends AnyFunSuite {
  test("roll works") {
    val M = 12
    val v = DenseVector.zeros[Double](M)
    val expected = DenseVector.zeros[Double](M)
    cforRange(0 until M)(i => {
      v(i) = i
      expected(i) = if (i - 3 < 0) { M + i - 3 } else { i - 3 }
    })

    assert(roll(v, 3) == expected)
  }

}
