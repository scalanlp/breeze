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

  val testDVD = DenseVector(1d,2d,3d,4d,5d)

  test("padRight") {
    assert( padRight(testDVD, OptPadDimensions.Dim1( 7 ), OptPadMode.Value(0d)) == DenseVector(1d,2d,3d,4d,5d,  0d,0d) )
    assert( padRight(testDVD, 7, OptPadMode.Value(0d)) == DenseVector(1d,2d,3d,4d,5d,  0d,0d) )
//    assert( padRight(testDVI, OptPadDimensions.T1( Tuple1(7) ), Value(100) ) == DenseVector(1,2,3,4,5,  100,100) )
//    assert( padRight(testDVI, 7, Min) == DenseVector(1,2,3,4,5,  1,1) )
//    assert( padRight(testDVI, 7, Max) == DenseVector(1,2,3,4,5,  1,1) )
//    assert( padRight(testDVI, 7, Mean) == DenseVector(2,3,4,5,6,  4,4) )
//    assert( padRight(testDVI, 7, Median) == DenseVector(1,2,3,4,5,  3,3) )
//    assert( padRight(testDVI, 7, Wrap) == DenseVector(1,2,3,4,5,  3,3) )
//    assert( padRight(testDVI, 7, Reflect) == DenseVector(1,2,3,4,5,  5,4) )

    //assert( padRight(testDVI, (7, 2)) == DenseMatrix( (1,0),(2,0),(3,0),(4,0),(5,0),  (0,0), (0,0) )


  }


}
