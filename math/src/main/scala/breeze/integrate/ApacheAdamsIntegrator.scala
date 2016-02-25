package breeze.integrate

import breeze.linalg._
import org.apache.commons.math3.ode.nonstiff.AdamsIntegrator

abstract class ApacheAdamsIntegrator(
    relTol: DenseVector[Double] = null,
    absTol: DenseVector[Double] = null)
  extends ApacheAdaptiveStepIntegrator(relTol, absTol) {

  type T <: AdamsIntegrator
}
