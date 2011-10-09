package scalanlp.optimize

import scalala._;
import operators._
import bundles.MutableInnerProductSpace
import scalala.tensor._
import scalala.library.Library._
import scalala.generic.math.CanNorm
import scalala.generic.collection.{CanMapKeyValuePairs, CanCreateZerosLike, CanMapValues, CanViewAsTensor1}
;

/**
 *
 * @author dlwh
 */
object AdaptiveGradientDescent {
  trait L2Regularization[T] extends StochasticGradientDescent[T] {

    val lambda: Double = 1.0;
    val delta = 1E-4
    import vspace._;


    case class History(sumOfSquaredGradients: T);
    override def initialHistory(f: StochasticDiffFunction[T],init: T) = History(zeros(init));
    override def updateHistory(newX: T, newGrad: T, newValue: Double, oldState: State) = {
      val oldHistory = oldState.history;
      val newG = oldHistory.sumOfSquaredGradients :+ (oldState.grad :* oldState.grad);
      new History(newG);
    }

    override protected def takeStep(state: State, dir: T, stepSize: Double) = {
      import state._
      val s = (state.history.sumOfSquaredGradients :+ (state.grad :* state.grad)).values.map(math.sqrt);
      val res = (( (s :* x) + dir * stepSize) :/ (s + (delta + lambda * stepSize)));
      res
    }

    override def determineStepSize(state: State, f: StochasticDiffFunction[T], dir: T) = {
      if(state.iter < 2) 0.001 * defaultStepSize else defaultStepSize;
    }

    override protected def adjust(newX: T, newGrad: T, newVal: Double) = {
      val av = newVal + (newX dot newX) * lambda / 2.0
      val ag = newGrad + newX * lambda
      (av -> ag)
    }

  }

  class L1Regularization[K,T](val lambda: Double=1.0,
                              delta: Double = 1E-5,
                              eta: Double=4,
                              maxIter: Int=100)(implicit vspace: MutableInnerProductSpace[Double,T],
                                                   TisTensor: CanViewAsTensor1[T,K,Double],
                                                   TKVPairs: CanMapKeyValuePairs[T,K,Double,Double,T],
                                                   canNorm: CanNorm[T]) extends StochasticGradientDescent[T](eta,maxIter) {
    import vspace._;
    case class History(sumOfSquaredGradients: T);
    def initialHistory(f: StochasticDiffFunction[T],init: T)= History(zeros(init));
    override def updateHistory(newX: T, newGrad: T, newValue: Double, oldState: State) = {
      val oldHistory = oldState.history;
      val newG = oldHistory.sumOfSquaredGradients :+ (oldState.grad :* oldState.grad)
      new History(newG);
    }

    override protected def takeStep(state: State, dir: T, stepSize: Double) = {
      import state._
      val s:T = sqrt(state.history.sumOfSquaredGradients :+ (grad :* grad) :+ delta);
      val res:T = x + (dir * stepSize :/ s)
      val tlambda = lambda * stepSize;
      TKVPairs.mapNonZero(res, { case (k,v) =>
        if(v.abs < tlambda / TisTensor(s)(k)) {
          0.;
        } else {
          (v - math.signum(v) * tlambda / TisTensor(s)(k));
        }
      });
    }

    override def determineStepSize(state: State, f: StochasticDiffFunction[T], dir: T) = {
      if(state.iter < 2) 0.001 * defaultStepSize else defaultStepSize;
    }

    override protected def adjust(newX: T, newGrad: T, newVal: Double) = {
      val av = newVal + norm(newX,1) * lambda
      val ag = newGrad + newX.values.map(math.signum) * lambda
      (av -> ag)
    }

  }
}