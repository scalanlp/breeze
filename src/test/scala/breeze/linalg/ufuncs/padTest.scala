package breeze.linalg

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

/**
 * @author ktakagaki
 * @date 04/16/2014.
 */
@RunWith(classOf[JUnitRunner])
class padTest extends FunSuite {

  import OptPadMode._
  val testDVI = DenseVector(1,2,3,4,5)

  test("padRight") {
    assert( padRight(testDVI, 7) == DenseVector(1,2,3,4,5,  0,0) )
    assert( padRight(testDVI, 7, 100) == DenseVector(1,2,3,4,5,  100,100) )
    assert( padRight(testDVI, 7, Min) == DenseVector(1,2,3,4,5,  1,1) )
    assert( padRight(testDVI, 7, Max) == DenseVector(1,2,3,4,5,  1,1) )
    assert( padRight(testDVI, 7, Mean) == DenseVector(2,3,4,5,6,  4,4) )
    assert( padRight(testDVI, 7, Median) == DenseVector(1,2,3,4,5,  3,3) )
    assert( padRight(testDVI, 7, Wrap) == DenseVector(1,2,3,4,5,  3,3) )
    assert( padRight(testDVI, 7, Reflect) == DenseVector(1,2,3,4,5,  5,4) )

    //assert( padRight(testDVI, (7, 2)) == DenseMatrix( (1,0),(2,0),(3,0),(4,0),(5,0),  (0,0), (0,0) )


  }


}
