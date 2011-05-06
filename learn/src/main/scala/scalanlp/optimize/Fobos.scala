package scalanlp.optimize

import scalala._;
import generic.collection.CanMapValues
import scalala.tensor.Tensor

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
  trait L2Regularization[T] { this: StochasticGradientDescent[T] =>
    import vspace._;
    /**
     * Regularization Constant
     */
    val lambda: Double = 1.0;

    override def projectVector(state: State, oldX: T, gradient: T, stepSize: Double):T = {
      (oldX - gradient * stepSize) / (1 + lambda * stepSize)
    }
  }

  /**
   * Implements L1 regularization where r(w) = lambda norm(w,1)
   */
  trait L1Regularization[K,T] { this: StochasticGradientDescent[T] =>
    import vspace._;
    /**
     * Regularization Constant
     */
    val lambda: Double = 1.0;

    override def projectVector(state: State, oldX: T, gradient: T, stepSize: Double):T = {
      val res:T = oldX - gradient * stepSize;
      val tlambda = lambda * stepSize;
      res.values.map { v =>
        if(v.abs < tlambda) {
          0;
        } else {
          v - math.signum(v) * tlambda;
        }
      };
    }
  }
}