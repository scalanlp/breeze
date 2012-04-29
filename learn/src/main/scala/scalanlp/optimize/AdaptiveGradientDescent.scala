package scalanlp.optimize

import scalala._
import operators._
import bundles.MutableInnerProductSpace
import scalala.tensor._
import scalala.library.Library._
import scalala.generic.math.CanNorm
import scalala.generic.collection.{CanMapKeyValuePairs, CanCreateZerosLike, CanMapValues, CanViewAsTensor1}

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
  trait L2Regularization[T] extends StochasticGradientDescent[T] {

    val lambda: Double = 1.0
    val delta = 1E-4
    import vspace._


    case class History(sumOfSquaredGradients: T)
    override def initialHistory(f: StochasticDiffFunction[T],init: T) = History(zeros(init))
    override def updateHistory(newX: T, newGrad: T, newValue: Double, oldState: State) = {
      val oldHistory = oldState.history
      val newG = oldHistory.sumOfSquaredGradients :+ (oldState.grad :* oldState.grad)
      new History(newG)
    }

    override protected def takeStep(state: State, dir: T, stepSize: Double) = {
      import state._
      val s = (state.history.sumOfSquaredGradients :+ (state.grad :* state.grad)).values.map(math.sqrt)
      val res = (( (s :* x) + dir * stepSize) :/ (s + (delta + lambda * stepSize)))
      res
    }

    override def determineStepSize(state: State, f: StochasticDiffFunction[T], dir: T) = {
      if(state.iter < 8) 0.001 * defaultStepSize else defaultStepSize
    }

    override protected def adjust(newX: T, newGrad: T, newVal: Double) = {
      val av = newVal + (newX dot newX) * lambda / 2.0
      val ag = newGrad + newX * lambda
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
                            maxIter: Int=100)(implicit vspace: MutableInnerProductSpace[Double,T],
                                              canNorm: CanNorm[T]) extends StochasticGradientDescent[T](eta,maxIter) {
    import vspace._
    case class History(sumOfSquaredGradients: T)
    def initialHistory(f: StochasticDiffFunction[T],init: T)= History(zeros(init))
    override def updateHistory(newX: T, newGrad: T, newValue: Double, oldState: State) = {
      val oldHistory = oldState.history
      val newG = oldHistory.sumOfSquaredGradients :+ (oldState.grad :* oldState.grad)
      new History(newG)
    }

    override protected def takeStep(state: State, dir: T, stepSize: Double) = {
      import state._
      val s:T = sqrt(state.history.sumOfSquaredGradients :+ (grad :* grad) :+ delta)
      val res:T = x + (dir * stepSize :/ s)
      val tlambda = lambda * stepSize
      vspace.zipMapValues.map(res, s, { case (v,v2) =>
        if(v.abs < tlambda / v2) {
          0.
        } else {
          (v - math.signum(v) * tlambda / v2)
        }
      })
    }

    override def determineStepSize(state: State, f: StochasticDiffFunction[T], dir: T) = {
      if(state.iter < 8) 0.001 * defaultStepSize else defaultStepSize
    }

    override protected def adjust(newX: T, newGrad: T, newVal: Double) = {
      val av = newVal + norm(newX,1) * lambda
      val ag = newGrad + newX.values.map(math.signum) * lambda
      (av -> ag)
    }

  }
}