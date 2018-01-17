package breeze.integrate

import breeze.linalg._
import org.apache.commons.math3.ode.nonstiff.{AdamsMoultonIntegrator => ApacheAdamsMoultonIntegrator}

class AdamsMoultonIntegrator(
    order: Int,
    minStep: Double,
    maxStep: Double,
    relTol: DenseVector[Double] = null,
    absTol: DenseVector[Double] = null)
    extends ApacheAdamsIntegrator(relTol, absTol) {

  type T = ApacheAdamsMoultonIntegrator

  protected final def create: ApacheAdamsMoultonIntegrator =
    new ApacheAdamsMoultonIntegrator(
      order - 1,
      minStep,
      maxStep,
      ApacheAdaptiveStepIntegrator.defaultAbsTol,
      ApacheAdaptiveStepIntegrator.defaultRelTol)
}
