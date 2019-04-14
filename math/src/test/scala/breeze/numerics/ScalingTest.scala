package breeze.numerics

import org.scalatest.FunSuite

/**
 *
 * @author dlwh
 */
class ScalingTest extends FunSuite {
  test("Simple Test, big") {
    val array = Array(math.pow(2, 300), math.pow(2, 301), math.pow(2, 150))
    val target = Array(math.pow(2, 10), math.pow(2, 11), math.pow(2, 150 - 290))
    val newScale = Scaling.scaleArray(array, 0)
    assert(newScale === 290)
    assert(array(0) === target(0))
    assert(array(1) === target(1))
    assert(array(2) === target(2))
  }

  test("Convert back to 0 scale, big") {
    val array = Array(math.pow(2, 10), math.pow(2, 11), math.pow(2, 150 - 290))
    val target = Array(math.pow(2, 300), math.pow(2, 301), math.pow(2, 150))
    Scaling.scaleArrayToScale(array, 290, 0)
    assert(array(0) === target(0))
    assert(array(1) === target(1))
    assert(array(2) === target(2))
  }

  test("Simple Test, small") {
    val array = Array(math.pow(2, -300), math.pow(2, -301), math.pow(2, -150))
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
    val target = Array(math.pow(2, -300), math.pow(2, -301), math.pow(2, -150))
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
    assert(Scaling.unscaleValue(math.pow(2, 10), 290) === math.pow(2, 300))
    assert(Scaling.toLogSpace(math.pow(2, 10), 290) === math.log(2) * 300)
  }

  test("Don't scale mixed values past the max") {
    val array = Array(math.pow(2, -300), math.pow(2, -301), math.pow(2, 100))
    val target = array.toIndexedSeq.toArray
    val newScale = Scaling.scaleArray(array, 0)
    assert(newScale === 0)
    assert(array(0) === target(0))
    assert(array(1) === target(1))
    assert(array(2) === target(2))
  }

  test("Addition of arrays of vastly different scales is a noop or a clobber") {
    val arr1 = Array(3.0, 4.0, 5.0)
    val arr2 = Array(1.0, 2.0, 3.0)
    val newScale1 = Scaling.sumArrays(arr1, 0, arr2, 500)
    assert(newScale1 == 500)
    assert(arr2.toIndexedSeq === IndexedSeq(1.0, 2.0, 3.0))
    val newScale2 = Scaling.sumArrays(arr1, 500, arr2, 0)
    assert(newScale2 == 500)
    assert(arr2.toIndexedSeq === IndexedSeq(3.0, 4.0, 5.0))
  }

  test("Addition of arrays of slightly different scales works as expected") {
    val arr1 = Array(3.0, 4.0, 5.0)
    val arr2, arr3 = Array(1.0, 2.0, 3.0)
    val newScale1 = Scaling.sumArrays(arr1, 0, arr2, 2)
    assert(newScale1 == 2)
    Scaling.scaleArrayToScale(arr1, 0, 2)
    val newScale2 = Scaling.sumArrays(arr1, 2, arr3, 2)
    assert(newScale2 == 2)
    assert(arr2.toIndexedSeq === arr3.toIndexedSeq)

    Scaling.sumArrays(arr1, 2, arr2, 0)
    Scaling.scaleArrayToScale(arr3, 0, 2)
    Scaling.sumArrays(arr1, 2, arr3, 2)
    assert(arr2.toIndexedSeq === arr3.toIndexedSeq)
  }

}
