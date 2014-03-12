package breeze.linalg

import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import spire.implicits._
/**
 *
 *
 * @author stucchio
 */
class rollTest extends FunSuite {
  test("roll works") {
    val M = 12
    val v = DenseVector.zeros[Double](M)
    val expected = DenseVector.zeros[Double](M)
    cfor(0)(i => i<M, i => i+1)(i => {
      v.unsafeUpdate(i,i)
      expected.unsafeUpdate(i,if (i - 3 < 0) { M+i-3 } else { i - 3 })
    })

    assert(roll(v,3) == expected)
  }

}
