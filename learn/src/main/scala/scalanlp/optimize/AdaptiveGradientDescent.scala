package scalanlp.optimize

import scalala.Scalala._;
import scalala.tensor.Tensor1
import scalala.tensor.operators.{TensorSelfOp, Tensor1Arith}
import scalala.tensor.operators.TensorShapes._;

/**
 *
 * @author dlwh
 */
object AdaptiveGradientDescent {
  trait L2Regularization[K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col]] extends StochasticGradientDescent[K,T] {

    val lambda: Double = 1.0;

    case class History(sumOfSquaredGradients: T);
    def initialHistory(f: BatchDiffFunction[K,T],init: T)= History(init.like);
    def updateHistory(oldState: State,newX: T,curValue: Double,curGrad: T) = {
      val oldHistory = oldState.history;
      val newG = oldHistory.sumOfSquaredGradients :+ (curGrad :* curGrad) value;
      new History(newG);
    }

    override def projectVector(state: State, oldX: T, gradient: T, stepSize: Double):T = {
      val s = root(state.history.sumOfSquaredGradients :+ (gradient :* gradient));
      val res = (( (s :* oldX) - gradient * stepSize) :/ (s + (lambda * stepSize))) value;
      res
    }

    private def root(x: T) = {
      val res = x.like;
      for( (k,v) <- x.activeElements) {
        res(k) = math.sqrt(v);
      }

      res;
    }

    override def chooseStepSize(state: State) = eta;

  }

  trait L1Regularization[K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col]] extends StochasticGradientDescent[K,T] {

    val lambda: Double = 1.0;

    case class History(sumOfSquaredGradients: T);
    def initialHistory(f: BatchDiffFunction[K,T],init: T)= History(init.like);
    def updateHistory(oldState: State,newX: T,curValue: Double,curGrad: T) = {
      val oldHistory = oldState.history;
      val newG = oldHistory.sumOfSquaredGradients :+ (curGrad :* curGrad) value;
      new History(newG);
    }

    override def projectVector(state: State, oldX: T, gradient: T, stepSize: Double):T = {
      val s = root(state.history.sumOfSquaredGradients :+ (gradient :* gradient));
      val res:T = oldX - (gradient * stepSize :/ s) value;
      val tlambda = lambda * stepSize;
      for( (k,v) <- res.activeElements) {
        if(v.abs < tlambda / s(k)) {
          res(k) = 0;
        } else {
          res(k) = v - tlambda / s(k);
        }
      }
      res
    }

    override def chooseStepSize(state: State) = eta;

    private def root(x: T) = {
      val res = x.like;
      for( (k,v) <- x.activeElements) {
        res(k) = math.sqrt(v);
      }

      res;
    }

  }
}