package breeze.stats

import org.scalatest.FunSuite
import breeze.linalg._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**Tests for breeze.linalg.max.scala
 * Test for clip is currently located in "DenseVectorTest.scala"
 * @author ktakagaki
 * @date 3/13/14.
 */
@RunWith(classOf[JUnitRunner])
class histogramTest extends FunSuite {

  val testDV = DenseVector(0.0, 0.1, 2.8, 2.9, 5)

  test("histogram returns correct values") {
    assert( hist(testDV, 3) == DenseVector(2.0,2.0,1.0) )
  }

  test("histogram respects range argument") {
    assert( hist(testDV, 3, (0.0, 3.0)) == DenseVector(2.0,0.0,2.0) )
  }

  test("fails for empty array") {
    intercept[IllegalArgumentException] {
      hist( DenseVector[Int]())
    }
  }

}
