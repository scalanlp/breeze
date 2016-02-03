package breeze.integrate

import breeze.linalg._
import org.apache.commons.math3.ode.nonstiff.{AdamsMoultonIntegrator => ApacheAdamsMoultonIntegrator}

class AdamsMoultonIntegrator(
	order: Int,
    minStep: Double,
    maxStep: Double,
    relTol: DenseVector[Double] = null,
    absTol: DenseVector[Double] = null)
  extends ApacheAdamsIntegrator(order, minStep, maxStep, relTol, absTol) {

  protected final val inner = new ApacheAdamsMoultonIntegrator(order - 1, minStep, maxStep, ApacheAdaptiveStepIntegrator.defaultAbsTol, ApacheAdaptiveStepIntegrator.defaultRelTol)
}