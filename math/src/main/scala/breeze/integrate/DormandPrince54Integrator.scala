package breeze.integrate

import breeze.linalg._
import org.apache.commons.math3.ode.nonstiff.{DormandPrince54Integrator => ApacheDormandPrince54Integrator}

class DormandPrince54Integrator(
    minStep: Double,
    maxStep: Double,
    relTol: DenseVector[Double] = null,
    absTol: DenseVector[Double] = null)
  extends ApacheAdaptiveStepIntegrator[ApacheDormandPrince54Integrator](relTol, absTol) {

  protected final def create: ApacheDormandPrince54Integrator =
    new ApacheDormandPrince54Integrator(minStep, maxStep, ApacheAdaptiveStepIntegrator.defaultAbsTol, ApacheAdaptiveStepIntegrator.defaultRelTol)
}
