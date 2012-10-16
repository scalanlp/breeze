package breeze.linalg

import org.scalatest.FunSuite

/**
 * 
 * @author dlwh
 */
class NumericOpsArrayTest extends FunSuite {
  import NumericOps.Arrays._
  // mostly just for compilation
  test("Array ops, non mutating") {
    val arr = Array(0.0,1.0,2.0,3.0)
    val arr2 = Array(0.0,1.0,2.0,3.0)
    arr :+ arr2
    arr :+ 3.0
  }

  test("Array ops, mutating") {
    val arr = Array(0.0,1.0,2.0,3.0)
    val arr2 = Array(0.0,1.0,2.0,3.0)
    arr :+= arr2
    arr :+= 3.0
  }
}
