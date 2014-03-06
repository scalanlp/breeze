package breeze.optimize

import breeze.math.MutableCoordinateSpace
import com.typesafe.scalalogging.slf4j.Logging
import breeze.collection.mutable.RingBuffer
import breeze.linalg.norm


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
 * @param projection projection operations
 */
class SpectralProjectedGradient[T, -DF <: DiffFunction[T]](
  val projection: T => T = { (t: T) => t },
  tolerance: Double = 1e-6,
  val suffDec: Double = 1e-4,
  minImprovementWindow: Int = 10,
  val alphaMax: Double = 1e10,
  val alphaMin: Double = 1e-10,
  maxIter: Int = 500,
  val testOpt: Boolean = true,
  val initFeas: Boolean = false,
  val maxSrchIt: Int = 30)(implicit coord: MutableCoordinateSpace[T, Double]) extends FirstOrderMinimizer[T, DF](minImprovementWindow = minImprovementWindow, maxIter = maxIter, tolerance = tolerance) with Projecting[T] with Logging {
  import coord._
  type History = Double
  protected def initialHistory(f: DF, init: T): History = 1.0
  protected def chooseDescentDirection(state: State, f: DF): T = projectedVector(state.x, state.grad * -state.history)
  override protected def adjust(newX: T, newGrad: T, newVal: Double):(Double,T) = (newVal,-projectedVector(newX, - newGrad))
  protected def takeStep(state: State, dir: T, stepSize: Double): T = projection(state.x + dir * stepSize)
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

  protected def determineStepSize(state: State, f: DF, direction: T): Double = {
    import state._
    val funRef = if (fVals.isEmpty) Double.PositiveInfinity else fVals.max
    val t = if (iter == 0) {
      scala.math.min(1.0, (1.0 / norm(grad, 1)))
    } else {
      1.0
    }
    val searchStep = direction * t
    val sufficientDecrease = grad.dot(searchStep) * suffDec
    val requiredValue = funRef + sufficientDecrease

    val lineSearchFunction = LineSearch.functionFromSearchDirection(f, x, direction)
    val ls = new SimpleLineSearch(requiredValue, maxSrchIt)
    ls.minimize(lineSearchFunction, t)
  }
  class SimpleLineSearch(requiredValue: Double, maxIterations: Int) extends ApproximateLineSearch {
    def iterations(f: DiffFunction[Double], init: Double = 1.0): Iterator[State] = {
      val (initfval, initfderiv) = f.calculate(init)
      Iterator.iterate((State(init, initfval, initfderiv), 0)) {
        case (State(alpha, fval, fderiv), iter) =>
          val newAlpha = alpha / 2.0
          val (fvalnew, fderivnew) = f.calculate(newAlpha)
          (State(newAlpha, fvalnew, fderivnew), iter + 1)
      }.takeWhile {
        case (state, iterations) =>
          (iterations == 0) ||
            (iterations < maxIterations &&
              state.value > requiredValue)
      }.map(_._1)
    }
  }
}
