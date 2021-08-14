package breeze.stats

import org.scalatest.funsuite.AnyFunSuite
import breeze.linalg._

/**Tests for breeze.linalg.max.scala
 * Test for clip is currently located in "DenseVectorTest.scala"
 * @author ktakagaki
 * @date 3/13/14.
 */
class histogramTest extends AnyFunSuite {

  val testDV = DenseVector(0.0, 0.1, 2.8, 2.9, 5)
  val testWeights = DenseVector(0.5, 0.5, 1.0, 3.0, 7.0)

  test("histogram returns correct values") {
    val result = hist(testDV, 3)
    assert(result.hist == DenseVector(2, 2, 1))
    assert(result.binEdges == DenseVector(0.0, 5.0 / 3.0, 2 * 5.0 / 3.0, 5.0))
  }

  test("histogram respects range argument") {
    val result = hist(testDV, 3, (0.0, 3.0))
    assert(result.hist == DenseVector(2, 0, 2))
    assert(result.binEdges == DenseVector(0.0, 1.0, 2.0, 3.0))
  }

  test("histogram handles weights") {
    val result = hist(testDV, 3, testWeights)(hist.defaultHistBinsWeights)
    assert(result.hist == DenseVector(1.0, 4.0, 7.0))
    assert(result.binEdges == DenseVector(0.0, 5.0 / 3.0, 2 * 5.0 / 3.0, 5.0))
  }

  test("fails for empty array") {
    intercept[IllegalArgumentException] {
      hist(DenseVector[Int]())(hist.defaultHist)
    }
  }

  test("negative values") {
    val v_neg = DenseVector(-4, -3, -4, 1, 1, 1, 4, 3, 4)
    val h_neg = hist(v_neg, 3)

    assert(h_neg.hist == DenseVector(3, 3, 3))

    val v_ok = v_neg + 4

    val h_ok = hist(v_ok, 3)
    assert(h_ok.hist == DenseVector(3, 3, 3))

  }

}
