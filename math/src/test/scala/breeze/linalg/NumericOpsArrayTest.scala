package breeze.linalg

import org.scalatest.funsuite.AnyFunSuite

/**
 *
 * @author dlwh
 */
class NumericOpsArrayTest extends AnyFunSuite {
  import NumericOps.Arrays._
  // mostly just for compilation
  test("Array ops, non mutating") {
    val arr = Array(0.0, 1.0, 2.0, 3.0)
    val arr2 = Array(0.0, 1.0, 2.0, 3.0)
    val res: Array[Double] = arr + arr2
    assert(res === Array(0.0, 2.0, 4.0, 6.0))
    val res3 = arr + 3.0
    assert(res3 === Array(3.0, 4.0, 5.0, 6.0))
  }

  test("Array ops, mutating") {
    val arr = Array(0.0, 1.0, 2.0, 3.0)
    val arr2 = Array(0.0, 1.0, 2.0, 3.0)
    arr :+= arr2
    arr :+= 3.0
  }
}
