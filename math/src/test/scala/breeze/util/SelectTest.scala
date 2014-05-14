package breeze.util

import breeze.util.quickSelect
import org.scalatest.FunSuite
import breeze.linalg.{shuffle, DenseVector}

/**
 * @author ktakagaki
 * @date 05/10/2014.
 */
class SelectTest extends FunSuite {

  test("quickSelect"){
    val testArray = Array(7, 3, 2, 5, 1, 4, 0, -1)
    assert( DenseVector( Range(0, 6).map( quickSelect(testArray, _ ) ).toArray ) == DenseVector(-1, 0, 1, 2, 3, 4) )

    println(quickSelectImpl(testArray, 0) + " " + DenseVector(testArray))
    println(quickSelectImpl(testArray, 1) + " " + DenseVector(testArray))
    println(quickSelectImpl(testArray, 2) + " " + DenseVector(testArray))
    println(quickSelectImpl(testArray, 3) + " " + DenseVector(testArray))
    println(quickSelectImpl(testArray, 4) + " " + DenseVector(testArray))
    println(quickSelectImpl(testArray, 5) + " " + DenseVector(testArray))
    println(quickSelectImpl(testArray, 6) + " " + DenseVector(testArray))
  }

}
