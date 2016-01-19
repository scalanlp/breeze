package breeze.integrate

import breeze.linalg._
import breeze.numerics._
import scala.annotation.tailrec

/**
* Runge-Kutta ODE solver
* Returns array of states corresponding to array of independent variable t
* TODO: Integrate support for native LSODE implementation
* TODO: Test integrating backwards through time
*
* @author jaketimothy
*/
object RungeKuttaOdeSolver {
  
  val defaultRelTol = 1.49012e-8
  val defaultAbsTol = 1.49012e-8
  
  def apply(
    tableau: RungeKuttaButcherTableau,
    f: (DenseVector[Double], Double) => DenseVector[Double],
    y0: DenseVector[Double],
    t: Array[Double],
    relTol: DenseVector[Double] = null,
    absTol: DenseVector[Double] = null
  ) : Array[DenseVector[Double]] = {

    // implicit Option allows user to input DenseVector[Double] rather than Some(DenseVector[Double])
    val someRelTol = Option(relTol)
    val someAbsTol = Option(absTol)
    
    if (someRelTol.map(_.length != y0.length).getOrElse(false)) {
      throw new Exception("Incompatible dimensions between relTol and y0.")
    }
    if (someAbsTol.map(_.length != y0.length).getOrElse(false)) {
      throw new Exception("Incompatible dimensions between absTol and y0.")
    }

    // If error tolerances are not specified, fill with default.
    val rTol = someRelTol getOrElse DenseVector.fill(y0.length)(defaultRelTol)
    val aTol = someAbsTol getOrElse DenseVector.fill(y0.length)(defaultAbsTol)
    
    // returns (state, step increment, last function evaluation)
    @tailrec def computeStep(
      yInit: DenseVector[Double],
      t0: Double,
      h: Double,
      maxStepSize: Double,
      tLimit: Double,
      lastEvaluation: Option[DenseVector[Double]] = None
      ) : (DenseVector[Double], Double, Option[DenseVector[Double]]) = {

      val k: Array[DenseVector[Double]] = new Array(tableau.stages)
      
      k(0) = if (tableau.hasFirstSameAsLast)
        lastEvaluation getOrElse f(yInit, t0)
      else
        f(yInit, t0)

      for (i <- 0 until (tableau.stages - 1)) {
        k(i + 1) = f(yInit + h * sum(for (j <- 0 to i) yield tableau.a(i)(j) * k(j)), t0 + h * tableau.c(i))
      }

      val dy = h * sum(for (i <- 0 until tableau.stages) yield tableau.b(i) * k(i))
      val dyStar = h * sum(for (i <- 0 until tableau.stages) yield tableau.bStar(i) * k(i))
      val y = yInit + dy
      val error = dy - dyStar

      // updates step size from error estimate
      val errorLimits = for (i <- 0 until y.length) yield max(rTol(i) * abs(y(i)), aTol(i))
      val errorChecks = for (i <- 0 until y.length) yield abs(error(i)) <= errorLimits(i)
      val errorFactor = min(for (i <- 0 until y.length) yield errorLimits(i) / abs(error(i)))
      val hUpdate = min(h * pow(errorFactor, 0.2), maxStepSize)
      val hNext = if (t0 + hUpdate <= tLimit) hUpdate else tLimit - t0
      if (errorChecks.reduce(_ & _)) {
        return (y, hNext, if (tableau.hasFirstSameAsLast) Some(k.last) else None)
      }

      computeStep(yInit, t0, hNext, maxStepSize, tLimit, lastEvaluation)
    }

    // TODO: Capture intermediate states and provide option for returning them
    @tailrec def integrateToNext(
      yInit: DenseVector[Double],
      t0: Double,
      h: Double,
      maxStepSize: Double,
      tNext: Double,
      lastEvaluation: Option[DenseVector[Double]] = None
      ) : DenseVector[Double] = {

      val (state, dt, lastEval) = computeStep(yInit, t0, h, maxStepSize, tNext, lastEvaluation)
      // TODO: Are there cases where this comparison of Doubles becomes problematic? Tolerance needed?
      if (t0 + dt >= tNext) {
        return state
      }

      integrateToNext(state, t0 + dt, dt, maxStepSize, tNext, lastEval)
    }

    val finalStates: Array[DenseVector[Double]] = new Array(t.length)

    finalStates(0) = y0
    for (i <- 1 until t.length) {
      val h0 = t(i) - t(i - 1)
      val maxStepSize = 0.1 * h0
      finalStates(i) = integrateToNext(finalStates(i - 1), t(i - 1), h0, maxStepSize, t(i))
    }

    finalStates
  }
}
