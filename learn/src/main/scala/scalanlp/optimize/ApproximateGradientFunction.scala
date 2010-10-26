package scalanlp.optimize

import scalala.tensor.Tensor1
import scalala.Scalala._;
import scalala.tensor.operators.TensorSelfOp
import scalala.tensor.operators.TensorShapes._;

/**
 * 
 * @author dlwh
 */
class ApproximateGradientFunction[K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col]]
              (f: T=>Double, epsilon: Double = 1E-5) extends DiffFunction[K,T] {
  override def valueAt(x: T) = f(x);

  def calculate(x:T) = {
    val fx = f(x);
    val grad = x.like;
    val xx = x.copy;
    for((k,v) <- x) {
      xx(k) += epsilon;
      grad(k) = (f(xx) - fx) / epsilon;
      xx(k) -= epsilon;
    }
    (fx,grad);
  }
}

object ApproximateGradientTester {
  def apply[K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col]](f:DiffFunction[K,T], x:T,
                                                              epsilons:Seq[Double]=Array(0.01,0.001,1E-4,1E-5,1E-6)) = {
    val predicted = f.gradientAt(x);
    for( eps <- epsilons) yield {
      val fapprox = new ApproximateGradientFunction[K,T](f,eps);
      val empirical = fapprox.gradientAt(x);
      predicted -= empirical
      norm(predicted, 2)
    }
  }
}