package scalanlp.optimize

import scalanlp.util.Logged
import scalanlp.util.Log._
import scalanlp.util.ConsoleLogging
import scalala.tensor._

/**
 * 
 * @author dlwh
 */

trait FirstOrderMinimizer[T,-DF<:DiffFunction[T]]
  extends Minimizer[T,DF] with Logged with CheckedConvergence[T] {

  type History;
  case class State protected[FirstOrderMinimizer](x: T, value: Double,
                                                  grad: T,
                                                  adjustedValue: Double,
                                                  adjustedGradient: T,
                                                  iter: Int,
                                                  history: History, failures: Int = 0);


  def iterations(f: DF,init: T): Iterator[State];

  def minimize(f: DF, init: T):T = {
    val steps = iterations(f,init);
     steps.reduceLeft( (a,b) => b).x;
  }
}

object FirstOrderMinimizer {
  /*
  case class OptParams(useStochastic:Boolean = true,
                       batchSize:Int = 512,
                       regularization: Double = 1.0,
                       alpha: Double = 0.5,
                       maxIterations:Int = -1,
                       useL1: Boolean = false, tolerance:Double = 1E-4) {
    def adjustedRegularization(numInstances: Int) = regularization * 0.01 * batchSize / numInstances;

    def minimizer[K,T]
        (f: BatchDiffFunction[K,T], numInstances:Int)
        (implicit arith: Tensor1Arith[K,T,Tensor1[K],Shape1Col]): FirstOrderMinimizer[K,T,BatchDiffFunction[K,T]] = {
      if(useStochastic) {
        if(regularization == 0.0) {
          new StochasticGradientDescent.SimpleSGD[K,T](alpha, maxIterations, batchSize) with ConsoleLogging {
            override val TOLERANCE = tolerance
          }
        } else if(useL1) {
          new StochasticGradientDescent[K,T](alpha, maxIterations, batchSize) with AdaptiveGradientDescent.L1Regularization[K,T] with ConsoleLogging {
            override val TOLERANCE = tolerance
            override val lambda = adjustedRegularization(numInstances);
          }
        } else { // L2
          new StochasticGradientDescent[K,T](alpha,  maxIterations, batchSize) with AdaptiveGradientDescent.L2Regularization[K,T] with ConsoleLogging {
            override val TOLERANCE = tolerance
            override val lambda = adjustedRegularization(numInstances);
          }
        }
      } else {
        minimizer(f:DiffFunction[K,T]);
      }
    }

    def minimizer[K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col]]
      (f: DiffFunction[K,T])
      (implicit arith: Tensor1Arith[K,T,Tensor1[K],Shape1Col]): FirstOrderMinimizer[K,T,DiffFunction[K,T]] = {
      if(useL1) new OWLQN[K,T](maxIterations, 5, regularization) with ConsoleLogging {
        override val TOLERANCE = tolerance
      }
      else new LBFGS[K,T](maxIterations, 5) with ConsoleLogging {

        override val TOLERANCE = tolerance
        override def iterations(f: DiffFunction[K,T], init: T) = {
          super.iterations(DiffFunction.withL2Regularization(f,regularization),init);
        }
      }
    }
  }
  */
}