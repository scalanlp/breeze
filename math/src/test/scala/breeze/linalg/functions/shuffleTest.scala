package breeze.linalg.functions

import org.scalatest._
import breeze.linalg._
import breeze.stats.hist

import scala.util.Random

/**
  * Created by huaminli on 7/25/16.
  */
class shuffleTest extends FunSuite with Matchers{
  test("The shuffle is uniformly random") {
    // Create a histogram to record the first entry of array that has been
    // shuffled many times.
    val n = 10
    val m = 100000
    val testArray = Array.tabulate(n)(i => i + 1)
    val histCount = DenseVector.zeros[Int](m)

    for (i <- 0 until m) {
      val shuffleArray = shuffle(testArray)
      histCount(i) = shuffleArray(0)
    }
    // The frequency for each number to show up in the first entry
    // should be close within some tolerance.
    val result = hist(histCount, n)
    for (i <- 0 until n) {
      result.hist(i) should ===(m / n +- Math.sqrt(m).toInt)
    }
  }
  test("Shuffle and shuffle back") {
    // Shuffle array testArr at random,
    // and should be able to shuffle it back to its original state.
    val n = 10
    val testIndex = shuffle(Array.tabulate(n)(i => i))
    val testArr = Array.fill[Double](n) {Random.nextGaussian()}

    // shuffles randomly
    val result1 = shuffle(testArr, testIndex)
    // also shuffles randomly
    val result2 = shuffle(testArr, testIndex, 0)
    // inverse shuffles
    val result3 = shuffle(result2, testIndex, 1)
    assert(result1 sameElements result2)
    assert(testArr sameElements result3)
  }
}
