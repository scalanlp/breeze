package breeze.numerics

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class BesselTest extends FunSuite {
  import Bessel._
  test("i0") {
    assert((i0(1) - 1.2660658777520083).abs < 1E-8)
    assert((i0(0) - 1.0).abs < 1E-8)
    assert((i0(20) - 4.355828255955353E7).abs < 1E-1)

  }

  test("i1") {
    assert((i1(1) - 0.565159103992485).abs < 1E-8, i1(1))
    assert((i1(0) - 0).abs < 1E-8)
    assert((i1(20) - 4.24549733851277E7).abs < 1E-1)

  }

}
