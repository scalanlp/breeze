package breeze.ode

import breeze.linalg._
import breeze.numerics._
import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec

/**
* Ordinary differential equation solvers.
* Designed to be used similarly to matlab's ode functions.
* TODO: A more intelligent step initialization and control algorithm.
*
* @author jaketimothy
*/
object AdaptiveStepODESolver {
  
  val defaultRelTol = 1.0e-3
  val defaultAbsTol = 1.0e-6
  
  def apply(
    stepper: AdaptiveODEStepper,
    f: (Double, DenseVector[Double]) => DenseVector[Double],
    start: Double,
    end: Double,
    y0: DenseVector[Double],
    relTol: Array[Double] = Array.empty,
    absTol: Array[Double] = Array.empty
  ) : (Array[Double], Array[DenseVector[Double]]) = {
    
    if (!relTol.isEmpty && relTol.length != y0.length)
      throw new Exception("Incompatible dimensions between relTol and y0.")
    if (!absTol.isEmpty && absTol.length != y0.length)
      throw new Exception("Incompatible dimensions between absTol and y0.")
    
    // If error tolerances are empty, fill with default.
    val rTol = if (relTol.isEmpty) {
      Array.fill(y0.length)(defaultRelTol)
    } else {
      relTol
    }
    val aTol = if (absTol.isEmpty) {
      Array.fill(y0.length)(defaultAbsTol)
    } else {
      absTol
    }
    
    val initialStepSize = (end-start)/100.0
    
    val times = ArrayBuffer[Double](start)
    val states = ArrayBuffer[DenseVector[Double]](y0)
    
    var h = initialStepSize
    @tailrec def computeStep() : (Double, DenseVector[Double]) = {
      val (y, error) = stepper(f, times.last, times.last+h, states.last)
      val errorLimits = for (i <- 0 until y.length) yield max(rTol(i)*abs(y(i)), aTol(i))
      if ((for (i <- 0 until y.length) yield abs(error(i)) < errorLimits(i)).foldLeft(true)(_&_)) {
        return (times.last+h, y)
      }
      // update step size from error estimate
      h = h * pow(max(for (i <- 0 until error.length) yield errorLimits(i)/abs(error(i))), 0.2)
      computeStep()
    }

    var lastStep = false
    while (!lastStep) {
      if (times.last + h > end) {
        h = end - times.last
        lastStep = true
      }
      val (time, state) = computeStep()
      times += time
      states += state
    }
    
    (times.toArray, states.toArray)
  }
}
