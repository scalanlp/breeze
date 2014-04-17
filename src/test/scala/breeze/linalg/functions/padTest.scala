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

  val testDVI = DenseVector(1,2,3,4,5)
  val testDMI = DenseVector(1,2,3,4,5).t
  val testDVD = DenseVector(1d,2d,3d,4d,5d)

  test("padRight") {

    import breeze.linalg.Options._

    assert( padRight(testDVI, dimensions = 3                  ) == DenseVector(1,2,3) )
    assert( padRight(testDVD, dimensions = 7    , mode = 11d  ) == DenseVector(1d,2d,3d,4d,5d,  11d,11d) )
    assert( padRight(testDVD, dimensions = ( 7 ), mode = 11d  ) == DenseVector(1d,2d,3d,4d,5d,  11d,11d) )
    assert( padRight(testDVD, 7                 , Value(10d)  ) == DenseVector(1d,2d,3d,4d,5d,  10d,10d) )
    assert( padRight(testDVI, 7                 , Min         ) == DenseVector(1,2,3,4,5,  1,1) )
    assert( padRight(testDVI, 7                 , Max         ) == DenseVector(1,2,3,4,5,  5,5) )
    assert( padRight(testDVI+1, 7               , Mean        ) == DenseVector(2,3,4,5,6,  4,4) )
    assert( padRight(testDVI, 7                 , Median      ) == DenseVector(1,2,3,4,5,  3,3) )
    assert( padRight(testDVI, 7                 , Wrap        ) == DenseVector(1,2,3,4,5,  1,2) )
    assert( padRight(testDVI, 7                 , Reflect     ) == DenseVector(1,2,3,4,5,  5,4) )

    //This is a bit tricky to implement vis-a-vis BroadcastedRows
    //assert( padRight(testDMI, OptPadDimensions.T2( Tuple2(7, 2) ), Value(100) ) == DenseVector(1,2,3,4,5,  100,100) )

    //This is very tricky
    //assert( padRight(testDVI, (7, 2)) == DenseMatrix( (1,0),(2,0),(3,0),(4,0),(5,0),  (0,0), (0,0) )


  }


}
