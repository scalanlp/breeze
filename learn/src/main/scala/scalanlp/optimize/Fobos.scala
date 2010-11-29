package scalanlp.optimize

import scalala.Scalala._;
import scalala.tensor.Tensor1
import scalala.tensor.operators.{TensorShapes, TensorSelfOp};
import TensorShapes._

/**
 * Traits for adding regularization and such to StochasticGradient in a way
 * that gives nice bounds.
 *
 * Implements algorithms from Duchi and Singer, 2009.
 * Efficient Online and Batch Learning using Forward Backward Splitting
 *
 *
 * @author dlwh
 */
object Fobos {
  /**
   * Implements l2 regularization where r(w) = lambda/2 norm(w,2)^2
   */
  trait L2Regularization[K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col]] { this: StochasticGradientDescent[K,T] =>
    /**
     * Regularization Constant
     */
    val lambda: Double = 1.0;

    override def projectVector(newX: T, stepSize: Double):T = {
      newX / (1 + lambda * stepSize) value;
    }
  }

  /**
   * Implements L1 regularization where r(w) = lambda/2 norm(w,2)^2
   */
  trait L1Regularization[K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col]] { this: StochasticGradientDescent[K,T] =>
    /**
     * Regularization Constant
     */
    val lambda: Double = 1.0;

    override def projectVector(newX: T, stepSize: Double):T = {
      val res = newX.like;
      val tlambda = lambda * stepSize;
      for( (k,v) <- newX.activeElements) {
        if(v.abs < tlambda) {
          res(k) = 0;
        } else {
          res(k) = v - tlambda;
        }
      }
      res
    }
  }
}