package breeze.interpolation

import org.scalatest.FunSuite

import breeze.linalg._
import breeze.interpolation._

/**
 *
 * @author chrismedrela
 **/

class LinearInterpolatorTest extends FunSuite {
  test("basics") {
    val x = DenseVector(1.0, 2.0, 3.0)
    val y = DenseVector(1.0, 5.0, 3.0)
    val f = new LinearInterpolator(x, y)
    assert(f(1.5) == 3.0)
    assert(f(2.5) == 4.0)
    assert(f(2.25) == 4.5)
    assert(f(DenseVector(1.5, 2.25)) == DenseVector(3.0, 4.5))
  }

  test("unordered nodes") {
    val x = DenseVector(3.0, 1.0, 2.0)
    val y = DenseVector(6.0, 2.0, 5.0)
    val f = new LinearInterpolator(x, y)
    assert(f(1.5) == 3.5)
    assert(f(2.5) == 5.5)
  }

  test("out of bounds") {
    val x = DenseVector(1.0, 2.0)
    val y = DenseVector(1.0, 2.0)
    val f = new LinearInterpolator(x, y)
    intercept[IndexOutOfBoundsException] {
      f(0.5)
    }
  }

  /*
  test("MyTest2") {
    assert(new MyInterpolatorUFuncWrapper(2.0)(2.0) == 4.0)
    assert(new MyInterpolatorUFuncWrapper(2.0)(DenseVector(2.0, 3.0)) == DenseVector(4.0, 5.0))
    assert(new MyInterpolatorUFuncWrapper2(1.0)(DenseVector(2.0, 3.0)) == DenseVector(4.0, 5.0))
  }
  */
}
