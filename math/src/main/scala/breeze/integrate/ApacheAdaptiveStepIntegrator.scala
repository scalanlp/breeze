package breeze.integrate

import breeze.linalg._
import org.apache.commons.math3.ode.{AbstractIntegrator, FirstOrderDifferentialEquations}
import org.apache.commons.math3.ode.nonstiff.AdaptiveStepsizeIntegrator

abstract class ApacheAdaptiveStepIntegrator(
    relTol: DenseVector[Double] = null,
    absTol: DenseVector[Double] = null)
  extends ApacheOdeIntegrator {

  type T <: AdaptiveStepsizeIntegrator

  // implicit Option allows user to input DenseVector[Double] rather than Some(DenseVector[Double])
  private val someRelTol = Option(relTol)
  private val someAbsTol = Option(absTol)
  
  // If error tolerances are not specified, fill with default.
  protected val (aTol, rTol) : (Array[Double], Array[Double]) = if (someRelTol.isEmpty && someAbsTol.isEmpty)
    (Array.empty, Array.empty)
  else if (!someRelTol.isEmpty && !someAbsTol.isEmpty)
    (someAbsTol.get.toArray, someRelTol.get.toArray)
  else if (someRelTol.isEmpty)
    (someAbsTol.get.toArray, Array.fill(someAbsTol.get.length)(ApacheAdaptiveStepIntegrator.defaultRelTol))
  else
    (Array.fill(someRelTol.get.length)(ApacheAdaptiveStepIntegrator.defaultAbsTol), someRelTol.get.toArray)

  if (!aTol.isEmpty && !rTol.isEmpty)
    inner.setStepSizeControl(inner.getMinStep, inner.getMaxStep, aTol, rTol)
  else
    inner.setStepSizeControl(inner.getMinStep, inner.getMaxStep, ApacheAdaptiveStepIntegrator.defaultAbsTol, ApacheAdaptiveStepIntegrator.defaultRelTol)
}

object ApacheAdaptiveStepIntegrator {
  val defaultRelTol = 1.49012e-8
  val defaultAbsTol = 1.49012e-8
}
