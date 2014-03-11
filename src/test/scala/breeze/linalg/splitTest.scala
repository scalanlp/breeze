package breeze.linalg

import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import spire.implicits._
/**
 *
 *
 * @author stucchio
 */
class splitTest extends FunSuite {
  test("split works on arrays with even multiple") {
    val start = DenseVector[Double](1,2,3,4,5,6,7,8,9,10,11,12)
    val expectedResult = Seq(DenseVector[Double](1,2,3,4), DenseVector[Double](5,6,7,8), DenseVector[Double](9,10,11,12))
    assert(split(start, 3) == expectedResult)
  }

  test("throws exception when vector.size is not divisible by number of elements in split") {
    intercept[IllegalArgumentException]{
      split(DenseVector[Double](1,2,3,4,5,6,7,8,9,10,11), 3)
    }
  }
}
