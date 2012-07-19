package breeze.numerics

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 *
 * @author dlwh
 */

@RunWith(classOf[JUnitRunner])
class ScalingTest extends FunSuite {
  test("Simple Test") {
    val array = Array(math.pow(2,300), math.pow(2, 301), math.pow(2, 150))
    val target = Array(math.pow(2, 10), math.pow(2, 11), math.pow(2, 150 - 290))
    val newScale = Scaling.scaleArray(array, 0)
    assert(newScale === -290)
    assert(array(0) === target(0))
    assert(array(1) === target(1))
    assert(array(2) === target(2))
  }

  test("Convert back to 0 scale") {
    val array = Array(math.pow(2, 10), math.pow(2, 11), math.pow(2, 150 - 290))
    val target = Array(math.pow(2,300), math.pow(2, 301), math.pow(2, 150))
    Scaling.scaleArrayToScale(array, -290, 0)
    assert(array(0) === target(0))
    assert(array(1) === target(1))
    assert(array(2) === target(2))
  }

  test("Unscale value") {
    assert(Scaling.unscaleValue(math.pow(2,10), -290) === math.pow(2, 300))
  }

}
