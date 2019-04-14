package breeze.linalg

import org.scalatest.FunSuite

/**Tests for breeze.linalg.max.scala
 * Test for clip is currently located in "DenseVectorTest.scala"
 * @author ktakagaki
 * @date 3/13/14.
 */
class maxTest extends FunSuite {

  val testDV = DenseVector(1, 10000000, 30, -500, 6)

  val testDM = DenseMatrix((1, 10000000, 30, -500, 6), (1, 100, 30, -500, 6), (1, 10000000, 30, -500, 6))

  test("max, min, minMax, ptp") {
    assert(max(-1, 10) == 10)
    assert(min(-1, 10) == -1)

    assert(max(testDV) == 10000000)
    assert(min(testDV) == -500)
    assert(ptp(testDV) == 500 + 10000000)
    assert(minMax(testDV) == (-500, 10000000))

    assert(max(testDM) == 10000000)
    assert(max(testDM(*, ::)) == DenseVector(10000000, 100, 10000000))
    assert(max(testDM(::, *)) == DenseVector(1, 10000000, 30, -500, 6).t)
    assert(min(testDM(*, ::)) == DenseVector(-500, -500, -500))
    assert(min(testDM(::, *)) == DenseVector(1, 100, 30, -500, 6).t)
    assert(min(testDM) == -500)
    assert(minMax(testDM) == (-500, 10000000))
    assert(ptp(testDM) == 500 + 10000000)

  }

}
