package breeze.integrate

import breeze.linalg._
import org.apache.commons.math3.ode.nonstiff.{GraggBulirschStoerIntegrator => ApacheGraggBulirschStoerIntegrator}

class GraggBulirschStoerIntegrator(
    minStep: Double,
    maxStep: Double,
    relTol: DenseVector[Double] = null,
    absTol: DenseVector[Double] = null)
  extends ApacheAdaptiveStepIntegrator[ApacheGraggBulirschStoerIntegrator](relTol, absTol) {

  protected final def create: ApacheGraggBulirschStoerIntegrator =
    new ApacheGraggBulirschStoerIntegrator(minStep, maxStep, ApacheAdaptiveStepIntegrator.defaultAbsTol, ApacheAdaptiveStepIntegrator.defaultRelTol)
}
