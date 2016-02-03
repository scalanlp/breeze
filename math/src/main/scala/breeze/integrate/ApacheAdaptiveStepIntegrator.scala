package breeze.integrate

import breeze.linalg._
import org.apache.commons.math3.ode.{AbstractIntegrator, FirstOrderDifferentialEquations}
import org.apache.commons.math3.ode.nonstiff.AdaptiveStepsizeIntegrator

abstract class ApacheAdaptiveStepIntegrator(
    minStep: Double,
    maxStep: Double,
    relTol: DenseVector[Double] = null,
    absTol: DenseVector[Double] = null)
  extends ApacheOdeIntegrator {

  protected val inner: AdaptiveStepsizeIntegrator

  // implicit Option allows user to input DenseVector[Double] rather than Some(DenseVector[Double])
  private val someRelTol = Option(relTol)
  private val someAbsTol = Option(absTol)
  
  override def integrate(
    f: (DenseVector[Double], Double) => DenseVector[Double],
    y0: DenseVector[Double],
    t: Array[Double]): Array[DenseVector[Double]] = {

    // If error tolerances are not specified, fill with default.
    val relativeTolerance = someRelTol getOrElse DenseVector.fill(y0.length)(ApacheAdaptiveStepIntegrator.defaultRelTol)
    val absoluteTolerance = someAbsTol getOrElse DenseVector.fill(y0.length)(ApacheAdaptiveStepIntegrator.defaultAbsTol)

    inner.setStepSizeControl(minStep, maxStep, absoluteTolerance.toArray, relativeTolerance.toArray)
    super.integrate(f, y0, t)
  }
}

object ApacheAdaptiveStepIntegrator {
  val defaultRelTol = 1.49012e-8
  val defaultAbsTol = 1.49012e-8
}
