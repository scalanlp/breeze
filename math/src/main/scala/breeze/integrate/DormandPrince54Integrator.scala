package breeze.integrate

import breeze.linalg._
import org.apache.commons.math3.ode.nonstiff.{DormandPrince54Integrator => ApacheDormandPrince54Integrator}

class DormandPrince54Integrator(
    minStep: Double,
    maxStep: Double,
    relTol: DenseVector[Double] = null,
    absTol: DenseVector[Double] = null
) extends ApacheAdaptiveStepIntegrator(relTol, absTol) {

  type T = ApacheDormandPrince54Integrator

  protected final def create: ApacheDormandPrince54Integrator =
    new ApacheDormandPrince54Integrator(
      minStep,
      maxStep,
      ApacheAdaptiveStepIntegrator.defaultAbsTol,
      ApacheAdaptiveStepIntegrator.defaultRelTol
    )
}
