package breeze.linalg

import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._ /**
 *
 *
 * @author dlwh
 */
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
