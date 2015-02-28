package breeze.optimize.proximal

import breeze.linalg.{DenseVector, norm}
import breeze.math.MutableVectorField
import breeze.numerics.pow
import breeze.optimize._
import breeze.util.{LazyLogger, SerializableLogging}


/**
 * SPG is a Spectral Projected Gradient minimizer; it minimizes a differentiable
 * function subject to the optimum being in some set, given by the projection operator  projection
 * @tparam T vector type
 * @param optTol termination criterion: tolerance for norm of projected gradient
 * @param gamma  sufficient decrease parameter
 * @param M number of history entries for linesearch
 * @param alphaMax longest step
 * @param alphaMin shortest step
 * @param maxNumIt maximum number of iterations
 * @param testOpt perform optimality check based on projected gradient at each iteration
 * @param initFeas is the initial guess feasible, or should it be projected?
 * @param maxSrchIt maximum number of line search attempts
 * @param proximal proximal operations
 */
class SpectralProximalGradient[T, DF <: DiffFunction[T]](
  val proximal: Proximal,
  tolerance: Double = 1e-6,
  val suffDec: Double = 1e-4,
  minImprovementWindow: Int = 10,
  val alphaMax: Double = 1e10,
  val alphaMin: Double = 1e-10,
  maxIter: Int = 500,
  val testOpt: Boolean = true,
  val initFeas: Boolean = false,
  val maxSrchIt: Int = 30,
  val gamma: Double = 1e-4)(implicit space: MutableVectorField[T, Double]) extends FirstOrderMinimizer[T, DF](minImprovementWindow = minImprovementWindow, maxIter = maxIter, tolerance = tolerance) with SerializableLogging {

  import space._

  type History = Double

  protected def initialHistory(f: DF, init: T): History = 1.0

  protected def chooseDescentDirection(state: State, f: DF): T = -state.grad

  override protected def adjust(newX: T, newGrad: T, newVal: Double): (Double, T) = (newVal, -newGrad)

  protected def takeStep(state: State, dir: T, stepSize: Double): T = state.x + dir :* stepSize

  protected def updateHistory(newX: T, newGrad: T, newVal: Double, f: DF, oldState: State): History = {
    val y = newGrad - oldState.grad
    val s = newX - oldState.x
    val alpha = s.dot(s) / s.dot(y)
    if (alpha.isNaN())
      0.0
    else if (alpha < alphaMin || alpha > alphaMax)
      1
    else
      alpha
  }

  //TO DO: Define a generic curvtrack line search that can be used from NonlinearMinimizer line search as well
  protected def determineStepSize(state: State, f: DF, direction: T): Double = {
    import state._

    val lambda = state.history

    val funRef = if (fVals.isEmpty) Double.PositiveInfinity else fVals.max
    val compyOld = proximal.valueAt(x.asInstanceOf[DenseVector[Double]], lambda) + funRef

    SpectralProximalGradient.curvtrack(f, proximal, x, grad, direction, history, funRef, compyOld, gamma, maxSrchIt, testOpt, logger)
  }
}

object SpectralProximalGradient {
  def curvtrack[T, DF <: DiffFunction[T]](f: DF, proximal: Proximal,
                                          x: T,
                                          grad: T,
                                          direction: T,
                                          t: Double,
                                          fOld: Double,
                                          compyOld: Double,
                                          gamma: Double,
                                          maxSrchIt: Int,
                                          testOpt: Boolean,
                                          logger: LazyLogger)(implicit space: MutableVectorField[T, Double]) = {
    import space._

    var accepted = false
    var srchit = 0
    var lambda = t

    do {
      val y = x + direction * lambda
      val (fy, gradfy) = f.calculate(y)

      val gy = proximal.valueAt(y.asInstanceOf[DenseVector[Double]], lambda)
      proximal.prox(y.asInstanceOf[DenseVector[Double]], lambda)

      val proxgy = y.asInstanceOf[T]

      val compy = fy + gy

      val desc = 0.5 * pow(norm(proxgy - x), 2)

      if (testOpt && srchit > 0) {
        logger.debug(f"SpectralProximalGradient: SrchIt $srchit%4d: f $compy%-10.4f t $lambda%-10.4f\n")
      }

      if (compy < compyOld + gamma * lambda * desc) {
        accepted = true
      } else if (srchit >= maxSrchIt) {
        accepted = true
      } else {
        lambda *= 0.5
        srchit = srchit + 1
      }
    } while (!accepted)

    if (srchit >= maxSrchIt) {
      logger.info("SpectralProximalGradient: Line search cannot make further progress")
      throw new LineSearchFailed(norm(grad), norm(direction))
    }
    lambda
  }
}
