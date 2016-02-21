package breeze.integrate

import breeze.linalg._
import org.apache.commons.math3.ode.nonstiff.AdamsIntegrator

abstract class ApacheAdamsIntegrator[T <: AdamsIntegrator](
    relTol: DenseVector[Double] = null,
    absTol: DenseVector[Double] = null)
  extends ApacheAdaptiveStepIntegrator[T](relTol, absTol)
