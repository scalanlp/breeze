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

class RungeKuttaOdeTest extends FunSuite {

  // simple 1-D oscillator
  val f = (y: DenseVector[Double], t: Double) => DenseVector(y(1), -y(0))

  test("ode45") {

    val steps = integrate.ode45(f, DenseVector(1.0, 0.0), Array(0.0, scala.math.Pi / 2.0))
    assert(closeTo(steps.last(0), 0.0))
    assert(closeTo(steps.last(1), -1.0))
  }

  test("incompatible dimensions") {

    intercept[Exception] {
      integrate.RungeKuttaOdeSolver(DormandPrinceTableau, f, DenseVector(1.0, 0.0), Array(0.0, scala.math.Pi), Array(1.0), Array.empty)
    }
    intercept[Exception] {
      integrate.RungeKuttaOdeSolver(DormandPrinceTableau, f, DenseVector(1.0, 0.0), Array(0.0, scala.math.Pi), Array.empty, Array(1.0))
    }
  }
}
