package breeze.signal

import org.scalatest.FunSuite
import breeze.linalg.DenseVector
import breeze.signal.rootMeanSquare
import breeze.signal

/**
 * @author ktakagaki
 * @date 2/18/14.
 */
class rootMeanSquareTest extends FunSuite {

  test("rootMeanSquare Double"){
    val temp = DenseVector[Double](3.0, 5.0, 1.0, 1.0)
    assert( rootMeanSquare(temp) == 3.0 )
  }

  test("rootMeanSquare Float"){
    val temp = DenseVector[Float](3.0f, 5.0f, 1.0f, 1.0f)
    assert( rootMeanSquare(temp) == 3.0f )
  }

  test("rootMeanSquare Int"){
    val temp = DenseVector[Int](3, 5, 1, 1)
    println(rootMeanSquare(temp))
    assert( rootMeanSquare(temp) == 3d )
  }

  test("rootMeanSquare Long"){
    val temp = DenseVector[Long](3L, 5L, 1L, 1L)
    assert( rootMeanSquare(temp) == 3d )
  }

}
