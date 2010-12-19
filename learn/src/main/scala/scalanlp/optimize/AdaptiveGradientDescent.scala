package scalanlp.optimize

import scalala._;
import scalala.tensor._
import scalala.library.Library._
import scalala.generic.collection.CanViewAsTensor1
;

/**
 *
 * @author dlwh
 */
object AdaptiveGradientDescent {
  trait L2Regularization[T] extends StochasticGradientDescent[T] {

    val lambda: Double = 1.0;
    val delta = 1E-4;

    case class History(sumOfSquaredGradients: T);
    def initialHistory(f: BatchDiffFunction[T],init: T) = History(zeros(init));
    def updateHistory(oldState: State,newX: T,curValue: Double,curGrad: T) = {
      val oldHistory = oldState.history;
      val newG = oldHistory.sumOfSquaredGradients :+ (curGrad :* curGrad);
      new History(newG);
    }

    override def projectVector(state: State, oldX: T, gradient: T, stepSize: Double):T = {
      val s = (state.history.sumOfSquaredGradients :+ (gradient :* gradient)).values.map(math.sqrt);
      val res = (( (s :* oldX) - gradient * stepSize) :/ (s + (delta + lambda * stepSize)));
      res
    }


    override def chooseStepSize(state: State) = if(state.iter < 2) 0.001 * eta else eta;

  }

  trait L1Regularization[K,T] extends StochasticGradientDescent[T] {
    implicit protected val TisTensor: CanViewAsTensor1[T,K,Double];
    implicit protected val TKVPairs: scalala.generic.collection.CanMapKeyValuePairs[T,K,Double,(K,Double),T];
    val lambda: Double = 1.0;
    val delta = 1E-5;

    case class History(sumOfSquaredGradients: T);
    def initialHistory(f: BatchDiffFunction[T],init: T)= History(zeros(init));
    def updateHistory(oldState: State,newX: T,curValue: Double,curGrad: T) = {
      val oldHistory = oldState.history;
      val newG = view(oldHistory.sumOfSquaredGradients) :+ (view(curGrad) :* curGrad)
      new History(newG);
    }

    override def projectVector(state: State, oldX: T, gradient: T, stepSize: Double):T = {
      val s:T = sqrt(state.history.sumOfSquaredGradients :+ (gradient :* gradient) :+ delta);
      val res:T = oldX - (gradient * stepSize :/ s)
      val tlambda = lambda * stepSize;
      TKVPairs.mapNonZero(res, { case (k,v) =>
        if(v.abs < tlambda / TisTensor(s)(k)) {
          k -> 0.;
        } else {
          k -> (v - math.signum(v) * tlambda / TisTensor(s)(k));
        }
      });
    }

    override def chooseStepSize(state: State) = eta;

  }
}