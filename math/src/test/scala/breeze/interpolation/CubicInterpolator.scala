package breeze.interpolation

import org.scalatest.FunSuite

import breeze.linalg._
import breeze.interpolation._
import breeze.numerics._

/**
 *
 * @author chrismedrela
 **/

class CubicInterpolatorTest extends FunSuite {
  val x = DenseVector(1.0, 2.0, 3.0, 5.0)
  val y = DenseVector(1.0, 5.0, 3.0, 7.0)
  val f = CubicInterpolator(x, y)

  test("edge cases") {
    assert(closeTo(f(1.0), 1.0))
    assert(closeTo(f(2.0), 5.0))
    assert(closeTo(f(3.0), 3.0))
    assert(closeTo(f(5.0), 7.0))
  }

  test("basics") {
    assert(closeTo(f(1.5), 3.65217391304347))
    assert(closeTo(f(2.25), 4.846467391304348))
    assert(closeTo(f(2.5), 4.2934782608695645))
    assert(closeTo(f(3.75), 3.042798913043478))
  }

  test("ufunc") {
    assert(equal(f(DenseVector(1.0, 2.0)), DenseVector(1.0, 5.0)))
    assert(equal(f(DenseMatrix((1.0, 2.0), (3.0, 5.0))), DenseMatrix((1.0, 5.0), (3.0, 7.0))))
  }

  test("extrapolation") {
    intercept[IndexOutOfBoundsException] {
      f(0.0)
    }
  }
  test("interpolation for two points") {
    val x = DenseVector(1.0, 2.0)
    val y = DenseVector(1.0, 5.0)
    intercept[Exception] {
      val f = CubicInterpolator(x, y)
    }
  }

  test("interpolation for three points") {
    val x = DenseVector(1.0, 2.0, 3.0)
    val y = DenseVector(1.0, 5.0, 3.0)
    val f = CubicInterpolator(x, y)
    assert(closeTo(f(1.0), 1.0))
    assert(closeTo(f(2.0), 5.0))
    assert(closeTo(f(3.0), 3.0))
    assert(closeTo(f(1.75), 4.4921875))
  }

  def equal(v1: Vector[Double], v2: Vector[Double]): Boolean = sum((v1 - v2) ^:^2.0) < 1e-9
  def equal(m1: DenseMatrix[Double], m2: DenseMatrix[Double]): Boolean = sum((m1 - m2) ^:^2.0) < 1e-9
}