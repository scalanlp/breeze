package breeze.optimize

import breeze.math.MutableFiniteCoordinateField
import breeze.numerics.sqrt
import breeze.stats.distributions.{Rand, RandBasis}

/**
 * Created by jda on 3/17/15.
 */
class AdaDeltaGradientDescent[T](
    rho: Double,
    maxIter: Int,
    tolerance: Double = 1e-5,
    improvementTolerance: Double = 1e-4,
    minImprovementWindow: Int = 50)(implicit vspace: MutableFiniteCoordinateField[T, _, Double], rand: RandBasis = Rand)
    extends StochasticGradientDescent[T](1d, maxIter, tolerance, minImprovementWindow) {

  val epsilon = 1e-6
  import vspace._

  case class History(avgSqGradient: T, avgSqDelta: T)

  override protected def initialHistory(f: StochasticDiffFunction[T], init: T): History = {
    History(zeroLike(init), zeroLike(init))
  }

  override protected def updateHistory(
      newX: T,
      newGrad: T,
      newVal: Double,
      f: StochasticDiffFunction[T],
      oldState: State): History = {
    val oldAvgSqGradient = oldState.history.avgSqGradient
    val newAvgSqGradient = (oldAvgSqGradient * rho) + ((newGrad *:* newGrad) * (1 - rho))

    val oldAvgSqDelta = oldState.history.avgSqDelta
    val delta = newX - oldState.x
    val newAvgSqDelta = (oldAvgSqDelta * rho) + ((delta *:* delta) * (1 - rho))

    History(newAvgSqGradient, newAvgSqDelta)
  }

  override protected def takeStep(state: State, dir: T, stepSize: Double): T = {
    val newAvgSqGradient = (state.history.avgSqGradient * rho) +:+ ((state.grad *:* state.grad) * (1 - rho))
    val rmsGradient = sqrt(newAvgSqGradient + epsilon)
    val rmsDelta = sqrt(state.history.avgSqDelta + epsilon)
    val delta = dir *:* rmsDelta /:/ rmsGradient
    state.x + delta
  }

  override def determineStepSize(state: State, f: StochasticDiffFunction[T], dir: T) = {
    defaultStepSize
  }

  override protected def adjust(newX: T, newGrad: T, newVal: Double) = {
    newVal -> newGrad
  }

}
