package breeze.integrate

import breeze.linalg._
import org.apache.commons.math3.ode.nonstiff.{AdamsBashforthIntegrator => ApacheAdamsBashforthIntegrator}

class AdamsBashforthIntegrator(
	order: Int,
    minStep: Double,
    maxStep: Double,
    relTol: DenseVector[Double] = null,
    absTol: DenseVector[Double] = null)
  extends ApacheAdamsIntegrator[ApacheAdamsBashforthIntegrator](relTol, absTol) {

  protected final def create: ApacheAdamsBashforthIntegrator =
    new ApacheAdamsBashforthIntegrator(order, minStep, maxStep, ApacheAdaptiveStepIntegrator.defaultAbsTol, ApacheAdaptiveStepIntegrator.defaultRelTol)
}
