package breeze.integrate

import breeze.linalg._
import org.apache.commons.math3.ode.nonstiff.{GraggBulirschStoerIntegrator => ApacheGraggBulirschStoerIntegrator}

class GraggBulirschStoerIntegrator(
    minStep: Double,
    maxStep: Double,
    relTol: DenseVector[Double] = null,
    absTol: DenseVector[Double] = null)
  extends ApacheAdaptiveStepIntegrator(minStep, maxStep, relTol, absTol) {

  protected final val inner = new ApacheGraggBulirschStoerIntegrator(minStep, maxStep, ApacheAdaptiveStepIntegrator.defaultAbsTol, ApacheAdaptiveStepIntegrator.defaultRelTol)
}