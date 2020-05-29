package breeze.integrate

import breeze.linalg._
import org.apache.commons.math3.ode.{AbstractIntegrator, FirstOrderDifferentialEquations}

trait ApacheOdeIntegrator extends OdeIntegrator {

  type T <: AbstractIntegrator

  protected def create: T

  protected final val inner: T = create

  override def integrate(
      f: (DenseVector[Double], Double) => DenseVector[Double],
      y0: DenseVector[Double],
      t: Array[Double]
  ): Array[DenseVector[Double]] = {

    object equations extends FirstOrderDifferentialEquations {

      override val getDimension = y0.length

      override def computeDerivatives(t: Double, y: Array[Double], yDot: Array[Double]): Unit =
        f(DenseVector(y), t).toArray.copyToArray(yDot)
    }

    val finalStates: Array[DenseVector[Double]] = new Array(t.length)
    finalStates(0) = y0
    for (i <- 1 until t.length) {
      val result: Array[Double] = new Array(y0.length)
      inner.integrate(equations, t(i - 1), finalStates(i - 1).toArray, t(i), result)
      finalStates(i) = DenseVector(result)
    }

    finalStates
  }
}
