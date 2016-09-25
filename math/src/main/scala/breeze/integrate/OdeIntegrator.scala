package breeze.integrate

import breeze.linalg._

trait OdeIntegrator {
  def integrate(
    f: (DenseVector[Double], Double) => DenseVector[Double],
    y0: DenseVector[Double],
    t: Array[Double]): Array[DenseVector[Double]]
}
