package breeze.stats.hypothesis

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit._
import org.scalatest.prop._
import org.scalacheck._
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TTestTest extends FunSuite with ShouldMatchers{
  val threshold = 0.01
  test("T Test two sample"){
      tTest(List(1.0,1,2,3), List(9.0,9,8,9)) should be (4.29E-5 plusOrMinus threshold)
  }
  test("T Test one sample"){
      tTest(Array(1.0,1,2,3)) should be (0.0336 plusOrMinus threshold)
  }
  test("T Test one sample should throw error when given vector of length 1"){
    intercept[IllegalArgumentException]{
      tTest(Array(1.0))
    }
  }
  test("T Test two sample should throw error when given vector of length 1"){
    intercept[IllegalArgumentException]{
      tTest(Array(1.0,2),Array(9.0))
    }
  }
}

