package breeze.integrate

import org.scalatest.funsuite.AnyFunSuite

import breeze.integrate
import breeze.linalg._
import breeze.numerics._
import scala.math.Pi

/**
 *
 * @author jaketimothy
 **/
class NonstiffOdeTest extends AnyFunSuite {

  // allowable percent difference
  val limit = 0.005

  // Euler Equations vs. Matlab ode113
  // http://www.mathworks.com/help/matlab/math/ordinary-differential-equations.html#f1-40077
  val f = (y: DenseVector[Double], t: Double) => DenseVector(y(1) * y(2), -y(0) * y(2), -0.51 * y(0) * y(1))
  val y0 = DenseVector(0.0, 1.0, 1.0)
  val t = Array(0.0, 12.0)
  val ans = DenseVector(-0.707186602982020, -0.709046793058523, 0.863898186330983)

  test("hall54") {

    val integrator = new HighamHall54Integrator(0.0, 1.0)

    val steps = integrator.integrate(f, y0, t)
    assert(abs((steps(1)(0) - ans(0)) / ans(0)) < limit)
    assert(abs((steps(1)(1) - ans(1)) / ans(1)) < limit)
    assert(abs((steps(1)(2) - ans(2)) / ans(2)) < limit)
  }

  test("dorpri54") {

    val integrator = new DormandPrince54Integrator(0.0, 1.0)

    val steps = integrator.integrate(f, y0, t)
    assert(abs((steps(1)(0) - ans(0)) / ans(0)) < limit)
    assert(abs((steps(1)(1) - ans(1)) / ans(1)) < limit)
    assert(abs((steps(1)(2) - ans(2)) / ans(2)) < limit)
  }

  test("dorpri853") {

    val integrator = new DormandPrince853Integrator(0.0, 1.0)

    val steps = integrator.integrate(f, y0, t)
    assert(abs((steps(1)(0) - ans(0)) / ans(0)) < limit)
    assert(abs((steps(1)(1) - ans(1)) / ans(1)) < limit)
    assert(abs((steps(1)(2) - ans(2)) / ans(2)) < limit)
  }

  test("stoer") {

    val integrator = new GraggBulirschStoerIntegrator(0.0, 1.0)

    val steps = integrator.integrate(f, y0, t)
    assert(abs((steps(1)(0) - ans(0)) / ans(0)) < limit)
    assert(abs((steps(1)(1) - ans(1)) / ans(1)) < limit)
    assert(abs((steps(1)(2) - ans(2)) / ans(2)) < limit)
  }

  test("bashforth5") {

    val integrator = new AdamsBashforthIntegrator(5, 0.0, 1.0)

    val steps = integrator.integrate(f, y0, t)
    assert(abs((steps(1)(0) - ans(0)) / ans(0)) < limit)
    assert(abs((steps(1)(1) - ans(1)) / ans(1)) < limit)
    assert(abs((steps(1)(2) - ans(2)) / ans(2)) < limit)
  }

  test("moulton5") {

    val integrator = new AdamsMoultonIntegrator(5, 0.0, 1.0)

    val steps = integrator.integrate(f, y0, t)
    assert(abs((steps(1)(0) - ans(0)) / ans(0)) < limit)
    assert(abs((steps(1)(1) - ans(1)) / ans(1)) < limit)
    assert(abs((steps(1)(2) - ans(2)) / ans(2)) < limit)
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
