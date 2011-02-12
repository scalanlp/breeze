package scalanlp.optimize

import scalanlp.util.Logged
import scalanlp.util.Log._
import scalala.tensor._
import scalala.Scalala._
import scalala.tensor.operators._;
import TensorShapes._;

/**
 * 
 * @author dlwh
 */

trait FirstOrderMinimizer[K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col],-DF<:DiffFunction[K,T]]
  extends Minimizer[T,DF] with Logged with CheckedConvergence[K,T] {

  type History;
  case class State protected[FirstOrderMinimizer](x: T, value: Double,
                                                   grad: T,
                                                   adjustedGradient: T,iter: Int,
                                                   history: History);


  def iterations(f: DF,init: T): Iterator[State];

  def minimize(f: DF, init: T):T = {
    val steps = iterations(f,init);
    val convSteps = for( cur@State(x,v,grad, adjGradient, iter,history)  <- steps) yield {
      log(INFO)("Iteration: " + iter);
      log(INFO)("Current v:" + v);
      log(INFO)("Current grad norm:" + norm(adjGradient,2));
      cur
    }

    convSteps.reduceLeft( (a,b) => b).x;
  }
}

object FirstOrderMinimizer {
  case class OptParams(useStochastic:Boolean = true,
                       batchSize:Int = 512,
                       regularization: Double = 1.0,
                       alpha: Double = 0.5,
                       maxIterations:Int = -1,
                       useL1: Boolean = false) {
    def adjustedRegularization(numInstances: Int) = regularization * 0.01 * batchSize / numInstances;

    def minimizer[K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col]]
        (f: BatchDiffFunction[K,T], numInstances:Int)
        (implicit arith: Tensor1Arith[K,T,Tensor1[K],Shape1Col]): FirstOrderMinimizer[K,T,BatchDiffFunction[K,T]] = {
      if(useStochastic) {
        if(regularization == 0.0) {
          StochasticGradientDescent[K,T](alpha, maxIterations, batchSize);
        } else if(useL1) {
          new StochasticGradientDescent[K,T](alpha, maxIterations, batchSize) with AdaptiveGradientDescent.L1Regularization[K,T] {
            override val lambda = adjustedRegularization(numInstances);
          }
        } else { // L2
          new StochasticGradientDescent[K,T](alpha,  maxIterations, batchSize) with AdaptiveGradientDescent.L2Regularization[K,T] {
            override val lambda = adjustedRegularization(numInstances);
          }
        }
      } else {
        minimizer(f:DiffFunction[K,T]);
      }
    }

    def minimizer[K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col]]
      (f: DiffFunction[K,T])
      (implicit arith: Tensor1Arith[K,T,Tensor1[K],Shape1Col]): FirstOrderMinimizer[K,T,BatchDiffFunction[K,T]] = {
      if(useL1) new OWLQN[K,T](maxIterations, 5, regularization)
      else new LBFGS[K,T](maxIterations, 5) {
        override def iterations(f: DiffFunction[K,T], init: T) = {
          super.iterations(DiffFunction.withL2Regularization(f,regularization),init);
        }
      }
    }
  }
}