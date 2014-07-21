package breeze.integrate

import org.scalatest.FunSuite

import breeze.integrate._
import breeze.linalg._
import breeze.numerics._

/**
 *
 * @author chrismedrela
 **/

class TrapezoidInterpolation extends FunSuite {
  val f = (x: Double) => 2*x
  val f2 = (x: Double) => x*x

  test("basics") {
    assert(closeTo(trapezoid_integrate(f, 0, 1, 2), 1))
    assert(closeTo(trapezoid_integrate(f, 0, 1, 3), 1))
    assert(closeTo(trapezoid_integrate(f2, 0, 1, 2), 0.5))
    assert(closeTo(trapezoid_integrate(f2, 0, 1, 3), 0.375))
  }

  test("not enough nodes") {
    intercept[Exception] {
      trapezoid_integrate(f, 0, 1, 1)
    }
  }
}
