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
  val testWeights = DenseVector(0.5, 0.5, 1.0, 3.0, 7.0)

  test("histogram returns correct values") {
    val result = hist(testDV, 3)
    assert( result.hist == DenseVector(2.0,2.0,1.0) )
    assert( result.binEdges == DenseVector(0.0, 5.0/3.0, 2*5.0/3.0, 5.0) )
  }

  test("histogram respects range argument") {
    val result = hist(testDV, 3, (0.0, 3.0))
    assert( result.hist == DenseVector(2.0,0.0,2.0) )
    assert( result.binEdges == DenseVector(0.0, 1.0, 2.0, 3.0) )
  }

  test("histogram handles weights") {
    val result = hist(testDV, 3, testWeights)
    assert( result.hist == DenseVector(1.0,4.0,7.0) )
    assert( result.binEdges == DenseVector(0.0, 5.0/3.0, 2*5.0/3.0, 5.0) )
  }

  test("fails for empty array") {
    intercept[IllegalArgumentException] {
      hist( DenseVector[Int]())
    }
  }

}
