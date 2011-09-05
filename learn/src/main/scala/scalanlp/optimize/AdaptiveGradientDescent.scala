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
    def initialHistory(f: StochasticDiffFunction[T],init: T) = History(zeros(init));
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

    override protected def adjustGradient(grad: T, x: T) = grad + x * lambda

    override protected def adjustValue(value: Double, x: T) = value + (x dot x) * lambda / 2
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
    def updateHistory(oldState: State,newX: T,curValue: Double,curGrad: T) = {
      val oldHistory = oldState.history;
      val newG = oldHistory.sumOfSquaredGradients :+ (curGrad :* curGrad)
      new History(newG);
    }

    override def projectVector(state: State, oldX: T, gradient: T, stepSize: Double):T = {
      val s:T = sqrt(state.history.sumOfSquaredGradients :+ (gradient :* gradient) :+ delta);
      val res:T = oldX - (gradient * stepSize :/ s)
      val tlambda = lambda * stepSize;
      TKVPairs.mapNonZero(res, { case (k,v) =>
        if(v.abs < tlambda / TisTensor(s)(k)) {
          0.;
        } else {
          (v - math.signum(v) * tlambda / TisTensor(s)(k));
        }
      });
    }

    override def chooseStepSize(state: State) = eta;

    override protected def adjustGradient(grad: T, x: T) = grad + x.values.map(math.signum) * lambda

    override protected def adjustValue(value: Double, x: T) = value + norm(x,1) * lambda


  }
}