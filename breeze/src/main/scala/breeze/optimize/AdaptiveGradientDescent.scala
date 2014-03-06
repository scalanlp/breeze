package breeze.optimize

import breeze.linalg._
import breeze.numerics._
import breeze.math.MutableCoordinateSpace
import breeze.stats.distributions.{Rand, RandBasis}

/**
 * Implements the L2^2 and L1 updates from
 * Duchi et al 2010 Adaptive Subgradient Methods for Online Learning and Stochastic Optimization.
 *
 * Basically, we use "forward regularization" and an adaptive step size based
 * on the previous gradients.
 *
 * @author dlwh
 */
object AdaptiveGradientDescent {

  /**
   * Implements the L2 regularization update.
   *
   * Each step is:
   *
   * x_{t+1}i = (s_{ti} * x_{ti} - \eta * g_ti) / (eta * regularization + delta + s_ti)
   *
   * where g_ti is the gradient and s_ti = \sqrt(\sum_t'^{t} g_ti^2)
   */
  class L2Regularization[T](val regularizationConstant: Double = 1.0,
                            stepSize: Double, maxIter: Int,
                            tolerance: Double = 1E-5,
                            improvementTolerance: Double= 1E-4,
                            minImprovementWindow: Int = 50)(implicit vspace: MutableCoordinateSpace[T, Double], rand: RandBasis = Rand)
    extends StochasticGradientDescent[T](stepSize, maxIter, tolerance, improvementTolerance, minImprovementWindow) {

    val delta = 1E-4
    import vspace._



    case class History(sumOfSquaredGradients: T)
    override def initialHistory(f: StochasticDiffFunction[T],init: T) = History(zeros(init))

    override def updateHistory(newX: T, newGrad: T, newValue: Double, f: StochasticDiffFunction[T], oldState: State) = {
      val oldHistory = oldState.history
      val newG = (oldState.grad :* oldState.grad)
      val maxAge = 1000.0
      if(oldState.iter > maxAge) {
        newG *= 1/maxAge
        axpy((maxAge - 1)/maxAge, oldHistory.sumOfSquaredGradients, newG)
      } else {
        newG += oldHistory.sumOfSquaredGradients
      }
      new History(newG)
    }

    override protected def takeStep(state: State, dir: T, stepSize: Double) = {
      import state._
      val s = sqrt(state.history.sumOfSquaredGradients :+ (state.grad :* state.grad))
      val newx = x :* s
      axpy(stepSize, dir, newx)
      s += (delta + regularizationConstant * stepSize)
      newx :/= s
      newx
    }

    override def determineStepSize(state: State, f: StochasticDiffFunction[T], dir: T) = {
      defaultStepSize
    }

    override protected def adjust(newX: T, newGrad: T, newVal: Double) = {
      val av = newVal + (newX dot newX) * regularizationConstant / 2.0
      val ag = newGrad + newX * regularizationConstant
      (av -> ag)
    }

  }


  /**
   * Implements the L1 regularization update.
   *
   * Each step is:
   *
   * x_{t+1}i = sign(x_{t,i} - eta/s_i * g_ti) * (abs(x_ti - eta/s_ti * g_ti) - lambda * eta /s_ti))_+
   *
   * where g_ti is the gradient and s_ti = \sqrt(\sum_t'^{t} g_ti^2)
   */
  class L1Regularization[T](val lambda: Double=1.0,
                            delta: Double = 1E-5,
                            eta: Double=4,
                            maxIter: Int=100)(implicit vspace: MutableCoordinateSpace[T, Double], rand: RandBasis = Rand) extends StochasticGradientDescent[T](eta,maxIter) {



    import vspace._
    case class History(sumOfSquaredGradients: T)
    def initialHistory(f: StochasticDiffFunction[T],init: T)= History(zeros(init))
    /*
    override def updateHistory(newX: T, newGrad: T, newValue: Double, oldState: State) = {
      val oldHistory = oldState.history
      val newG = oldHistory.sumOfSquaredGradients :+ (oldState.grad :* oldState.grad)
      new History(newG)
    }
    */

    override def updateHistory(newX: T, newGrad: T, newValue: Double,  f: StochasticDiffFunction[T], oldState: State) = {
      val oldHistory = oldState.history
      val newG = (oldState.grad :* oldState.grad)
      val maxAge = 200.0
      if(oldState.iter > maxAge) {
        newG *= 1/maxAge
        axpy((maxAge - 1)/maxAge, oldHistory.sumOfSquaredGradients, newG)
      } else {
        newG += oldHistory.sumOfSquaredGradients
      }
      new History(newG)
    }

    override protected def takeStep(state: State, dir: T, stepSize: Double) = {
      import state._
      val s:T = sqrt(state.history.sumOfSquaredGradients :+ (grad :* grad) :+ delta)
      val res:T = x + (dir * stepSize :/ s)
      val tlambda = lambda * stepSize
      vspace.zipMapValues.map(res, s, { case (x_half ,s_i) =>
        if(x_half.abs < tlambda / s_i) {
          0.0
        } else {
          (x_half - math.signum(x_half) * tlambda / s_i)
        }
      })
    }

    override def determineStepSize(state: State, f: StochasticDiffFunction[T], dir: T) = {
      defaultStepSize
    }

    override protected def adjust(newX: T, newGrad: T, newVal: Double) = {
      val av = newVal + norm(newX, 1.0) * lambda
      val ag = newGrad + signum(newX) * lambda
      (av -> ag)
    }

  }
}