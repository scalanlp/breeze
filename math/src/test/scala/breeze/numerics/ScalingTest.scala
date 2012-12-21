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
  test("Simple Test, big") {
    val array = Array(math.pow(2,300), math.pow(2, 301), math.pow(2, 150))
    val target = Array(math.pow(2, 10), math.pow(2, 11), math.pow(2, 150 - 290))
    val newScale = Scaling.scaleArray(array, 0)
    assert(newScale === 290)
    assert(array(0) === target(0))
    assert(array(1) === target(1))
    assert(array(2) === target(2))
  }

  test("Convert back to 0 scale, big") {
    val array = Array(math.pow(2, 10), math.pow(2, 11), math.pow(2, 150 - 290))
    val target = Array(math.pow(2,300), math.pow(2, 301), math.pow(2, 150))
    Scaling.scaleArrayToScale(array, 290, 0)
    assert(array(0) === target(0))
    assert(array(1) === target(1))
    assert(array(2) === target(2))
  }

  test("Simple Test, small") {
    val array = Array(math.pow(2,-300), math.pow(2, -301), math.pow(2, -150))
    val target = Array(math.pow(2, -155), math.pow(2, -156), math.pow(2, -5))
    val newScale = Scaling.scaleArray(array, 0)
    assert(newScale === -145)
    assert(array(0) === target(0))
    assert(array(1) === target(1))
    assert(array(2) === target(2))
    val newScale2 = Scaling.scaleArray(array, -290)
    assert(newScale2 === -290)
  }

  test("Convert back to 0 scale, small") {
    val array = Array(math.pow(2, -155), math.pow(2, -156), math.pow(2, -5))
    val target = Array(math.pow(2,-300), math.pow(2, -301), math.pow(2, -150))
    Scaling.scaleArrayToScale(array, -145, 0)
    assert(array(0) === target(0))
    assert(array(1) === target(1))
    assert(array(2) === target(2))
  }

  test("Scale of 0.0 is 0") {
    val array = Array(0.0)
    val newScale = Scaling.scaleArray(array, 0)
    assert(newScale === 0)
  }

  test("Unscale value") {
    assert(Scaling.unscaleValue(math.pow(2,10), 290) === math.pow(2, 300))
    assert(Scaling.toLogSpace(math.pow(2,10), 290) === math.log(2) * 300)
  }


  test("Don't scale mixed values past the max") {
    val array = Array(math.pow(2,-300), math.pow(2, -301), math.pow(2, 100))
    val target = array.toIndexedSeq.toArray
    val newScale = Scaling.scaleArray(array, 0)
    assert(newScale === 0)
    assert(array(0) === target(0))
    assert(array(1) === target(1))
    assert(array(2) === target(2))
  }


}
