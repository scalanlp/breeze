package breeze.interpolation

import org.scalatest.FunSuite

import breeze.linalg._
import breeze.interpolation._

/**
 *
 * @author chrismedrela
 **/

class CubicInterpolatorTest extends FunSuite {
  val x = DenseVector(1.0, 2.0, 3.0, 5.0)
  val y = DenseVector(1.0, 5.0, 3.0, 7.0)
  val f = CubicInterpolator(x, y)

  test("edge cases") {
    //assert(f(1.0) == 1.0)
    //assert(f(2.0) == 5.0)
    //assert(f(3.0) == 3.0)
    //assert(f(5.0) == 7.0)
  }

  /*
  test("basics") {
    val x = DenseVector(1.0, 2.0, 3.0)
    val y = DenseVector(1.0, 5.0, 3.0)
    val f = CubicInterpolator(x, y)
    assert(f(1.5) == 3.0)
    assert(f(2.5) == 4.0)
    assert(f(2.25) == 4.5)
  }

  test("unordered nodes") {
    val x = DenseVector(3.0, 1.0, 2.0)
    val y = DenseVector(6.0, 2.0, 5.0)
    val f = CubicInterpolator(x, y)
    assert(f(1.5) == 3.5)
    assert(f(2.5) == 5.5)
  }

  test("ufunc") {
    val x = DenseVector(1.0, 2.0, 3.0)
    val y = DenseVector(2.0, 5.0, 6.0)
    val f = CubicInterpolator(x, y)
    assert(f(DenseVector(1.5, 2.5)) == DenseVector(3.5, 5.5))
    assert(f(DenseMatrix((1.5, 2.5), (2.0, 1.0))) == DenseMatrix((3.5, 5.5), (5.0, 2.0)))
  }

  test("edge cases") {
    val x = DenseVector(1.0, 2.0, 3.0)
    val y = DenseVector(2.0, 5.0, 6.0)
    val f = CubicInterpolator(x, y)
    assert(f(1.0) == 2.0)
    assert(f(2.0) == 5.0)
    assert(f(3.0) == 6.0)
    assert(f(x) == y)
  }

  test("extrapolation") {
    val x = DenseVector(1.0, 2.0, 3.0)
    val y = DenseVector(2.0, 5.0, 4.0)
    val f = CubicInterpolator(x, y)
    intercept[IndexOutOfBoundsException] {
      f(0.0)
    }
  }
  */
}