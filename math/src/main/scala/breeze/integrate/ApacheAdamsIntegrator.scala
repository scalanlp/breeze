package breeze.integrate

import breeze.linalg._
import org.apache.commons.math3.ode.nonstiff.AdamsIntegrator

abstract class ApacheAdamsIntegrator(
    order: Int,
    minStep: Double,
    maxStep: Double,
    relTol: DenseVector[Double] = null,
    absTol: DenseVector[Double] = null)
  extends ApacheAdaptiveStepIntegrator(minStep, maxStep, relTol, absTol) {

  protected val inner: AdamsIntegrator
}
