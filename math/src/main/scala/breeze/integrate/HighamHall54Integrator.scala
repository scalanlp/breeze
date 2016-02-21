package breeze.integrate

import breeze.linalg._
import org.apache.commons.math3.ode.nonstiff.{HighamHall54Integrator => ApacheHighamHall54Integrator}

class HighamHall54Integrator(
    minStep: Double,
    maxStep: Double,
    relTol: DenseVector[Double] = null,
    absTol: DenseVector[Double] = null)
  extends ApacheAdaptiveStepIntegrator[ApacheHighamHall54Integrator](relTol, absTol) {

  protected final def create: ApacheHighamHall54Integrator =
    new ApacheHighamHall54Integrator(minStep, maxStep, ApacheAdaptiveStepIntegrator.defaultAbsTol, ApacheAdaptiveStepIntegrator.defaultRelTol)
}
