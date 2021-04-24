package breeze.util

import org.scalatest.funsuite.AnyFunSuite
import breeze.linalg.{min, max, shuffle, DenseVector}

import scala.collection.mutable.ArrayBuffer

/**
 * @author ktakagaki
 * @date 05/10/2014.
 */
class SelectTest extends AnyFunSuite {

  test("quickSelect") {
    var testArray = Array(7, 3, 2, 5, 1, 4, 2, -1)
    assert(DenseVector(Range(0, 6).map(quickSelect(testArray, _)).toArray) == DenseVector(-1, 1, 2, 2, 3, 4))

    testArray = shuffle(testArray)
    quickSelect.inPlace(testArray, 3)
    assert(testArray(3) == 2)
    assert(2 >= max(DenseVector(testArray.slice(0, 3))))
    assert(2 <= min(DenseVector(testArray.slice(4, testArray.length))))

  }

  test("quickSelect, collection") {
    var testArray = ArrayBuffer(7, 3, 2, 5, 1, 4, 2, -1)
    val impl =quickSelect.implFromQSInPlaceColl[ArrayBuffer[Int], Int]
    assert(DenseVector(Range(0, 6).map(quickSelect(testArray, _)(impl)).toArray) == DenseVector(-1, 1, 2, 2, 3, 4))

    testArray = shuffle(testArray)
    quickSelect.inPlace(testArray, 3)
    assert(testArray(3) == 2)
    assert(2 >= max(testArray.slice(0, 3)))
    assert(2 <= min(testArray.slice(4, testArray.length)))

  }

}
