package breeze.integrate

import breeze.linalg._
import breeze.numerics._
import scala.annotation.tailrec

/**
* Runge-Kutta ODE solver
* Returns array of states corresponding to array of independent variable t
* TODO: Integrate support for native LSODE implementation
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
    relTol: Array[Double] = Array.empty,
    absTol: Array[Double] = Array.empty
  ) : Array[DenseVector[Double]] = {
    
    if (!relTol.isEmpty && relTol.length != y0.length) {
      throw new Exception("Incompatible dimensions between relTol and y0.")
    }
    if (!absTol.isEmpty && absTol.length != y0.length) {
      throw new Exception("Incompatible dimensions between absTol and y0.")
    }

    // If error tolerances are empty, fill with default.
    val rTol = if (!relTol.isEmpty) relTol else Array.fill(y0.length)(defaultRelTol)
    val aTol = if (!absTol.isEmpty) absTol else Array.fill(y0.length)(defaultAbsTol)
    
    // returns (state, step increment, last function evaluation)
    @tailrec def computeStep(
      yInit: DenseVector[Double],
      t0: Double,
      h: Double,
      tLimit: Double,
      lastEvaluation: Option[DenseVector[Double]] = None
      ) : (DenseVector[Double], Double, Option[DenseVector[Double]]) = {

      val k = Array.fill[DenseVector[Double]](tableau.stages)(DenseVector.zeros(yInit.length))
      
      k(0) = if (tableau.hasFirstSameAsLast)
        lastEvaluation getOrElse f(yInit, t0)
      else
        f(yInit, t0)

      for (i <- 0 until (tableau.stages - 1)) {
        k(i) = f(yInit + h * sum(for (j <- 0 to i) yield tableau.a(i)(j) * k(j)), t0 + h * tableau.c(i))
      }

      val dy = h * sum(for (i <- 0 until tableau.stages) yield tableau.b(i) * k(i))
      val dyStar = h * sum(for (i <- 0 until tableau.stages) yield tableau.bStar(i) * k(i))
      val y = yInit + dy
      val error = dy - dyStar

      // updates step size from error estimate
      val errorWeights = for (i <- 0 until y.length) yield rTol(i) * abs(y(i)) + aTol(i)
      val errorFactor = max(for (i <- 0 until error.length) yield abs(error(i) / errorWeights(i)))
      val hUpdate = 0.9 * h / errorFactor
      val hNext = if (t0 + hUpdate <= tLimit) hUpdate else tLimit - t0
      if (errorFactor <= 1.0) {
        return (y, hNext, if (tableau.hasFirstSameAsLast) Some(k.last) else None)
      }

      computeStep(yInit, t0, hNext, tLimit, lastEvaluation)
    }

    val finalStates = Array.fill[DenseVector[Double]](t.length)(DenseVector.zeros(y0.length))

    finalStates(0) = y0
    for (i <- 1 until t.length) {
      // TODO: Capture intermediate states and provide option for returning them
      var state = finalStates(i - 1)
      var time = t(i - 1)
      var stepSize = t(i) - t(i - 1)
      var lastEval: Option[DenseVector[Double]] = None
      do {
        val (s, dt, lastF) = computeStep(state, time, stepSize, t(i), lastEval)
        state = s
        time = time + dt
        stepSize = dt
        lastEval = lastF
        // TODO: Are there cases where this comparison of Doubles becomes problematic? Tolerance needed?
      } while (time < t(i))

      finalStates(i) = state
    }

    finalStates
  }
}
