package breeze.integrate

import org.scalatest.FunSuite

import breeze.integrate
import breeze.linalg._
import breeze.numerics._
import scala.math.Pi

/**
 *
 * @author jaketimothy
 **/

class NonstiffOdeTest extends FunSuite {

  // Euler Equations vs. Matlab ode45
  // http://www.mathworks.com/help/matlab/math/ordinary-differential-equations.html#f1-40077
  val f = (y: DenseVector[Double], t: Double) => DenseVector(y(2) * y(3), -y(1) * y(3), -0.51 * y(1) * y(2))
  val y0 = DenseVector(0.0, 1.0, 1.0)
  val t = Array(0.0, 12.0)

  test("hall54") {

    val integrator = new HighamHall54Integrator(0.0, 1.0)

    val steps = integrator.integrate(f, y0, t)
    assert(closeTo(steps(1)(0), 0.0))
    assert(closeTo(steps(1)(1), 0.0))
    assert(closeTo(steps(1)(2), 0.0))
  }

  test("dorpri54") {

    val integrator = new DormandPrince54Integrator(0.0, 1.0)

    val steps = integrator.integrate(f, y0, t)
    assert(closeTo(steps(1)(0), 0.0))
    assert(closeTo(steps(1)(1), -1.0))
  }

  test("dorpri853") {

    val integrator = new DormandPrince853Integrator(0.0, 1.0)

    val steps = integrator.integrate(f, y0, t)
    assert(closeTo(steps(1)(0), 0.0))
    assert(closeTo(steps(1)(1), -1.0))
  }

  test("stoer") {

    val integrator = new GraggBulirschStoerIntegrator(0.0, 1.0)

    val steps = integrator.integrate(f, y0, t)
    assert(closeTo(steps(1)(0), 0.0))
    assert(closeTo(steps(1)(1), -1.0))
  }

  test("bashforth5") {

    val integrator = new AdamsBashforthIntegrator(5, 0.0, 1.0)

    val steps = integrator.integrate(f, y0, t)
    assert(closeTo(steps(1)(0), 0.0))
    assert(closeTo(steps(1)(1), -1.0))
  }

  test("moulton5") {

    val integrator = new AdamsMoultonIntegrator(5, 0.0, 1.0)

    val steps = integrator.integrate(f, y0, t)
    assert(closeTo(steps(1)(0), 0.0))
    assert(closeTo(steps(1)(1), -1.0))
  }

  // test("incompatible dimensions") {

  //   intercept[Exception] {
  //     integrate.RungeKuttaOdeSolver(DormandPrinceTableau, f, DenseVector(1.0, 0.0), Array(0.0, scala.math.Pi), relTol = DenseVector(1.0))
  //   }
  //   intercept[Exception] {
  //     integrate.RungeKuttaOdeSolver(DormandPrinceTableau, f, DenseVector(1.0, 0.0), Array(0.0, scala.math.Pi), absTol = DenseVector(1.0))
  //   }
  // }
}
