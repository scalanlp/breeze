package breeze.stats.hypothesis

import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.scalacheck._
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TTestTest extends FunSuite{
  test("T Test two sample"){
    expectResult(4.294044745759096E-5){
      tTest(List(1.0,1,2,3), Option(List(9.,9,8,9)))
    }
  }
  test("T Test one sample"){
    expectResult(0.03361111111111112){
      tTest(Array(1.0,1,2,3))
    }
  }
  test("T Test one sample should throw error when given vector of length 1"){
    intercept[IllegalArgumentException]{
      tTest(Array(1.0))
    }
  }
  test("T Test two sample should throw error when given vector of length 1"){
    intercept[IllegalArgumentException]{
      tTest(Array(1.0,2),Option(Array(9.).toTraversable))
    }
  }
}

