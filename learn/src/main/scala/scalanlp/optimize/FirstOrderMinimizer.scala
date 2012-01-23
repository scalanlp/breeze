package scalanlp.optimize

import scalala.tensor._
import scalala.operators.bundles.MutableInnerProductSpace
import scalala.generic.math.CanNorm
import scalala.generic.collection.{CanMapKeyValuePairs, CanViewAsTensor1}
import scalanlp.util.logging.Logged

/**
 * 
 * @author dlwh
 */

abstract class FirstOrderMinimizer[T,-DF<:StochasticDiffFunction[T]](maxIter: Int = -1)(implicit norm: CanNorm[T]) extends Minimizer[T,DF] with Logged {

  type History;
  case class State(x: T,
                   value: Double, grad: T,
                   adjustedValue: Double, adjustedGradient: T,
                   iter: Int,
                   history: History);

  protected def initialHistory(f: DF, init: T): History
  protected def adjust(newX: T, newGrad: T, newVal: Double):(Double,T) = (newVal,newGrad)
  protected def chooseDescentDirection(state: State):T
  protected def determineStepSize(state: State, f: DF, direction: T):Double
  protected def takeStep(state: State, dir: T, stepSize:Double):T
  protected def updateHistory(newX: T, newGrad: T, newVal: Double, oldState: State):History

  protected def initialState(f: DF, init: T) = {
    val x = init
    val (value,grad) = f.calculate(x)
    val (adjValue,adjGrad) = adjust(x,grad,value)
    val history = initialHistory(f,init)
    State(x,value,grad,adjValue,adjGrad,0,history)
  }

  def iterations(f: DF,init: T): Iterator[State] = {
    var failedOnce = false
    val it = Iterator.iterate(initialState(f,init)) { state => try {
        val dir = chooseDescentDirection(state)
        val stepSize = determineStepSize(state, f, dir)
        log.info("Step Size:" + stepSize)
        val x = takeStep(state,dir,stepSize)
        val (value,grad) = f.calculate(x)
        log.info("Val and Grad Norm:" + value + " " + norm(grad,2))
        val (adjValue,adjGrad) = adjust(x,grad,value)
        log.info("Adj Val and Grad Norm:" + adjValue + " " + norm(adjGrad,2))
        val history = updateHistory(x,grad,value,state)
        failedOnce = false
        State(x,value,grad,adjValue,adjGrad,state.iter + 1,history)
    } catch {
      case x: FirstOrderException if !failedOnce =>
        failedOnce = true
        log.error("Failure! Resetting history: " + x)
        state.copy(history = initialHistory(f,state.x))
    }
    }
    it:Iterator[State]
  }

  def minimize(f: DF, init: T):T = {
    iterations(f,init).find(state =>
      (state.iter >= maxIter && maxIter >= 0)
        || norm(state.adjustedGradient,2) <= math.max(1E-6 * state.adjustedValue.abs,1E-9)
    ).get.x
  }
}

sealed trait FirstOrderException extends RuntimeException
class NaNHistory extends FirstOrderException
class StepSizeUnderflow extends FirstOrderException
class LineSearchFailed extends FirstOrderException

object FirstOrderMinimizer {
  case class OptParams(batchSize:Int = 512,
                       regularization: Double = 1.0,
                       alpha: Double = 0.5,
                       maxIterations:Int = -1,
                       useL1: Boolean = false,
                       tolerance:Double = 1E-4,
                       useStochastic: Boolean= false) {

    def iterations[K,T](f: BatchDiffFunction[T], init: T)
                      (implicit arith: MutableInnerProductSpace[Double,T], canNorm: CanNorm[T],
                       TisTensor: CanViewAsTensor1[T,K,Double],
                       TKVPairs: CanMapKeyValuePairs[T,K,Double,Double,T],
                       view:  <:<[T,scalala.tensor.mutable.Tensor1[K,Double] with scalala.tensor.mutable.TensorLike[K, Double, _, T with scalala.tensor.mutable.Tensor1[K,Double]]]): Iterator[FirstOrderMinimizer[T, BatchDiffFunction[T]]#State] = {
      val it = if(useStochastic) {
        val adjustedRegularization = regularization * 0.01 * batchSize / f.fullRange.size
         this.copy(regularization=adjustedRegularization).iterations(f.withScanningBatches(batchSize), init)
      } else {
        iterations(f:DiffFunction[T], init)
      }

      it.asInstanceOf[Iterator[FirstOrderMinimizer[T, BatchDiffFunction[T]]#State]]
    }

    def iterations[K,T](f: StochasticDiffFunction[T], init:T)
                      (implicit arith: MutableInnerProductSpace[Double,T], canNorm: CanNorm[T],
                       TisTensor: CanViewAsTensor1[T,K,Double],
                       TKVPairs: CanMapKeyValuePairs[T,K,Double,Double,T],
                       view:  <:<[T,scalala.tensor.mutable.Tensor1[K,Double] with scalala.tensor.mutable.TensorLike[K, Double, _, T with scalala.tensor.mutable.Tensor1[K,Double]]]):Iterator[FirstOrderMinimizer[T, StochasticDiffFunction[T]]#State] = {
      val r = if(regularization == 0.0) {
        new StochasticGradientDescent.SimpleSGD[T](alpha, maxIterations) {
        }
      } else if(useL1) {
        new AdaptiveGradientDescent.L1Regularization[K,T](regularization, eta=alpha, maxIter = maxIterations)(arith,TisTensor,TKVPairs,canNorm) {
        }
      } else { // L2
        new StochasticGradientDescent[T](alpha,  maxIterations) with AdaptiveGradientDescent.L2Regularization[T] {
          override val lambda = regularization;
        }
      }
      r.iterations(f,init)
    }

    def iterations[K,T]
      (f: DiffFunction[T], init:T)(implicit vspace: MutableInnerProductSpace[Double,T],
                             view:  <:<[T,scalala.tensor.mutable.Tensor1[K,Double] with scalala.tensor.mutable.TensorLike[K, Double, _, T with scalala.tensor.mutable.Tensor1[K,Double]]],
                             canNorm: CanNorm[T]): Iterator[LBFGS[T]#State] = {
       if(useL1) new OWLQN[K,T](maxIterations, 5, regularization).iterations(f,init)
      else new LBFGS[T](maxIterations, 5).iterations(DiffFunction.withL2Regularization(f,regularization),init);
    }
  }
}
