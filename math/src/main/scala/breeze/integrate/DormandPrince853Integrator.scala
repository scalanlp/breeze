package breeze.integrate

import breeze.linalg._
import org.apache.commons.math3.ode.nonstiff.{DormandPrince853Integrator => ApacheDormandPrince853Integrator}

class DormandPrince853Integrator(
    minStep: Double,
    maxStep: Double,
    relTol: DenseVector[Double] = null,
    absTol: DenseVector[Double] = null)
  extends ApacheAdaptiveStepIntegrator(minStep, maxStep, relTol, absTol) {

  protected final val inner = new ApacheDormandPrince853Integrator(minStep, maxStep, ApacheAdaptiveStepIntegrator.defaultAbsTol, ApacheAdaptiveStepIntegrator.defaultRelTol)
}