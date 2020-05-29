package breeze.optimize

import breeze.math.{MutableInnerProductModule, MutableInnerProductVectorSpace}
import breeze.stats.distributions.Rand
import breeze.linalg._

/**
 *
 *
 * @author dlwh
 */
class StochasticAveragedGradient[T](
    maxIter: Int = -1,
    initialStepSize: Double = 0.25,
    tuneStepFrequency: Int = 10,
    l2Regularization: Double = 0.0
)(implicit vs: MutableInnerProductModule[T, Double])
    extends FirstOrderMinimizer[T, BatchDiffFunction[T]](maxIter) {
  import vs._

  case class History(
      stepSize: Double,
      range: IndexedSeq[Int],
      currentSum: T,
      previousGradients: IndexedSeq[T],
      nextPos: Int
  )

  protected def initialHistory(f: BatchDiffFunction[T], init: T): History = {
    val zero = zeroLike(init)
    History(initialStepSize, f.fullRange, zeroLike(init), IndexedSeq.fill(f.fullRange.length)(zero), 0)
  }

  protected def chooseDescentDirection(state: State, f: BatchDiffFunction[T]): T = {
    state.history.currentSum * (-1.0 / f.fullRange.size)
  }

  protected def determineStepSize(state: State, f: BatchDiffFunction[T], direction: T): Double = state.history.stepSize

  override protected def calculateObjective(f: BatchDiffFunction[T], x: T, history: History): (Double, T) = {
    f.calculate(x, IndexedSeq(history.nextPos))
  }

  override protected def adjust(newX: T, newGrad: T, newVal: Double) = {
    val av = newVal + (newX.dot(newX)) * l2Regularization / 2.0
    val ag = newGrad + newX * l2Regularization
    (av -> ag)
  }

  protected def takeStep(state: State, dir: T, stepSize: Double): T = {
    val newx = state.x * (1 - stepSize * l2Regularization)
    axpy(stepSize, dir, newx)
    newx
  }

  protected def updateHistory(
      newX: T,
      newGrad: T,
      newVal: Double,
      f: BatchDiffFunction[T],
      oldState: State
  ): History = {
    import oldState.history._
    val d = currentSum - previousGradients(nextPos)
    val newStepSize = if (tuneStepFrequency > 0 && (oldState.iter % tuneStepFrequency) == 0) {
      val xdiff = newX - oldState.x
      if (
        (f.valueAt(newX, IndexedSeq(nextPos)) + l2Regularization / 2 * norm(
          newX
        ) - oldState.adjustedValue) > (oldState.adjustedGradient
          .dot(xdiff)) + (xdiff.dot(xdiff)) / (2 * stepSize)
      ) {
        stepSize / 2
      } else {
        stepSize * 1.5
      }
    } else {
      stepSize
    }
    d += newGrad
    History(
      newStepSize,
      range,
      d,
      previousGradients.updated(nextPos, newGrad),
      if (oldState.iter < previousGradients.length - 1) oldState.iter + 1 else Rand.choose(range).draw()
    )
  }
}
