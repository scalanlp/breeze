package breeze

import breeze.linalg._

package object ode {
  
  def ode45(
    f: (Double, DenseVector[Double]) => DenseVector[Double],
    start: Double,
    end: Double,
    y0: DenseVector[Double],
    relTol: Array[Double] = Array.empty,
    absTol: Array[Double] = Array.empty
  ) : (Array[Double], Array[DenseVector[Double]]) = {
    
    AdaptiveStepODESolver(RKDPStepper, f, start, end, y0, relTol, absTol)
  }
}
