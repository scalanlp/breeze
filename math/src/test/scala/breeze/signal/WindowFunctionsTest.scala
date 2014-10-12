package breeze.signal

import breeze.signal.filter.WindowFunctions
import org.scalatest._
import breeze._
import breeze.linalg._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

//{norm, DenseVector}
import WindowFunctions.hammingWindow

/**
 * Created with IntelliJ IDEA.
 * User: takagaki
 * Date: 14.05.13
 * Time: 02:31
 * To change this template use File | Settings | File Templates.
 */
@RunWith(classOf[JUnitRunner])
class WindowFunctionsTest extends FunSuite {

  val testThreshold = 1.0E-15

  // <editor-fold desc="hammingWindow">

  test("hammingWindow without parameters") {
    val windowEven = DenseVector( 0.0800, 0.3978521825875242, 0.9121478174124757, 0.9121478174124757, 0.3978521825875242, 0.0800 )
    assert(  norm( hammingWindow(6) - windowEven ) < testThreshold  )
    val windowOdd = DenseVector( 0.0800, 0.54, 1, 0.54, 0.0800 )
    assert(  norm( hammingWindow(5) - windowOdd ) < testThreshold  )
    assert( hammingWindow(1)(0) == 1d )
  }

  // </editor-fold>
}
