package scalanlp.optimize

import scalala._;
import operators._
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

  class L1Regularization[K,T](val lambda: Double=1.0,
                              delta: Double = 1E-5,
                              eta: Double=4,
                              maxIter: Int=100,
                              batchSize: Int = 50)(implicit view: T=>MutableNumericOps[T],
                                                   view2: T=>HasValuesMonadic[T,Double],
                                                   canNormT: CanNorm[T],
                                                   canMapValues: CanMapValues[T,Double,Double,T],
                                                   canAddScalar: BinaryOp[T,Double,OpAdd,T],
                                                   canAdd: BinaryOp[T,T,OpAdd,T],
                                                   canSub: BinaryOp[T,T,OpSub,T],
                                                   canMulScalar: BinaryOp[T,Double,OpMul,T],
                                                   canDivScalar: BinaryOp[T,Double,OpDiv,T],
                                                   canMulPiece: BinaryOp[T,T,OpMul,T],
                                                   canDivPiece: BinaryOp[T,T,OpDiv,T],
                                                   TisTensor: CanViewAsTensor1[T,K,Double],
                                                   TKVPairs: CanMapKeyValuePairs[T,K,Double,Double,T],
                                                   zeros: CanCreateZerosLike[T,T]) extends StochasticGradientDescent[T](eta,maxIter,batchSize) {
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
          0.;
        } else {
          (v - math.signum(v) * tlambda / TisTensor(s)(k));
        }
      });
    }

    override def chooseStepSize(state: State) = eta;

  }
}