package breeze.stats.hypothesis

import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.scalacheck._
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TTestTest extends FunSuite with Matchers {
  val threshold = 0.01
  test("T Test two sample") {
    tTest(List(1.0, 1, 2, 3), List(9.0, 9, 8, 9)) should be(4.29E-5 +- threshold)
  }
  test("T Test one sample") {
    tTest(Array(1.0, 1, 2, 3)) should be(0.0336 +- threshold)
  }
  test("T Test one sample for Traversable") {
    //This test is designed to detect this bug, just in case a refactoring re-introduces it: https://github.com/scalanlp/breeze/issues/486
    tTest(List(1.0, 1, 2, 3)) should be(0.0336 +- threshold)
  }
  test("T Test one sample should throw error when given vector of length 1") {
    intercept[IllegalArgumentException] {
      tTest(Array(1.0))
    }
  }
  test("T Test two sample should throw error when given vector of length 1") {
    intercept[IllegalArgumentException] {
      tTest(Array(1.0, 2), Array(9.0))
    }
  }
}
