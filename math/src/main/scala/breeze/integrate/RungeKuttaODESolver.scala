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
object RungeKuttaODESolver {
  
  val defaultRelTol = 1.0e-3
  val defaultAbsTol = 1.0e-6
  
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

      val (y, error) = stepper(yInit, t0, t0 + h)
      val k = Array.fill[DenseVector[Double]](tableau.stages)(DenseVector.zeros(yInit.length))
      
      k(0) = if (tableau.hasFirstSameAsLast)
        lastEvaluation getOrElse f(yInit, start)
      else
        f(yInit, start)

      for (i <- 0 until (tableau.stages - 1)) {
        k(i) = f(yInit + h * sum(for (j <- 0 until i) yield tableau.a(i)(j) * k(j)), start + h * tableau.c(i))
      }

      val y = yInit + h * sum(for (i <- 0 until tableau.stages) yield tableau.b(i) * k(i))
      val error = y - (yInit + h * sum(for (i <- 0 until tableau.stages) yield tableau.bStar(i) * k(i)))
      
      val errorLimits = for (i <- 0 until y.length) yield max(rTol(i) * abs(y(i)), aTol(i))
      if ((for (i <- 0 until y.length) yield abs(error(i)) < errorLimits(i)).foldLeft(true)(_ & _)) {
        return (y, h, if (tableau.hasFirstSameAsLast) Some(k.last) else None)
      }

      // updates step size from error estimate
      val hUpdate = h * pow(max(for (i <- 0 until error.length) yield errorLimits(i) / abs(error(i))), 0.2)
      computeStep(yInit, t0, if (t0 + hUpdate <= tLimit) { hUpdate } else { tLimit - t0 }, tLimit, lastEvaluation)
    }

    val finalStates = Array.fill[DenseVector[Double]](t.length)(DenseVector.zeros(y0.length))

    finalStates(0) = y0
    for (i <- 1 until t.length) {
      // TODO: Capture intermediate states and provide option for returning them
      var state = finalStates(i - 1)
      var time = t(i - 1)
      // TODO: Is there a more intelligent step size initialization?
      var stepSize = t(i) - t(i - 1)
      do {
        val (s, dt) = computeStep(state, time, stepSize, t(i))
        state = s
        time = time + dt
        stepSize = dt
        // TODO: Are there cases where this comparison of Doubles becomes problematic? Tolerance needed?
      } while (time < t(i))

      finalStates(i) = state
    }
    
    finalStates
  }
}
