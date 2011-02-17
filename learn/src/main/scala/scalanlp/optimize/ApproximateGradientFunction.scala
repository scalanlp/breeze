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

  def calculateAndPrint(x: T, trueGrad: T) = {
    val fx = f(x);
    val grad = x.like;
    val xx = x.copy;
    for((k,v) <- x) {
      xx(k) += epsilon;
      grad(k) = (f(xx) - fx) / epsilon;
      xx(k) -= epsilon;
      println("diff : " + epsilon + " val: " + (grad(k) - trueGrad(k)) + " comp: " + trueGrad(k) + " " + grad(k));
    }
    (fx,grad);

  }
}

class RandomizedGradientCheckingFunction[K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col]]
          (f: DiffFunction[K,T], randFraction:Double = 0.01, epsilons: Seq[Double] = Array(1E-5))
          extends DiffFunction[K,T] {
  val approxes =  for( eps <- epsilons) yield {
    val fapprox = new ApproximateGradientFunction[K,T](f,eps);
    fapprox
  }

  override def valueAt(x: T) = f(x);

  def calculate(x:T) = {
    val (v,predicted) = f.calculate(x);
    for { (fap,eps) <- approxes zip epsilons } {
      calculateAndPrint(eps, x,predicted)._2;
    }
    (v,predicted);
  }

  def calculateAndPrint(epsilon: Double, x: T, trueGrad: T) = {
    val fx = f(x);
    val grad = x.like;
    val xx = x.copy;
    for((k,v) <- x if math.random < randFraction) {
      xx(k) += epsilon;
      grad(k) = (f(xx) - fx) / epsilon;
      xx(k) -= epsilon;
      println(k + "diff : " + epsilon + " val: " + (grad(k) - trueGrad(k)) + " comp: " + trueGrad(k) + " " + grad(k));
    }
    (fx,grad);

  }

}

class GradientCheckingDiffFunction[K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col]]
          (f: DiffFunction[K,T], epsilons: Seq[Double] = Array(1E-5))
          extends DiffFunction[K,T] {
  val approxes =  for( eps <- epsilons) yield {
    val fapprox = new ApproximateGradientFunction[K,T](f,eps);
    fapprox
  }

  override def valueAt(x: T) = f(x);

  def calculate(x:T) = {
    val (v,predicted) = f.calculate(x);
    for { (fap,eps) <- approxes zip epsilons } {
      val empirical = fap.calculateAndPrint(x,predicted)._2;
      empirical -= predicted
      println("diff : " + eps + " norm: " + norm(empirical,2));
    }
    (v,predicted);
  }

}

object ApproximateGradientTester {
  def apply[K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col]](f:DiffFunction[K,T], x:T,
                                                              epsilons:Seq[Double]=Array(0.01,0.001,1E-4,1E-5,1E-6)) = {
    val predicted = f.gradientAt(x);
    for( eps <- epsilons) yield {
      val fapprox = new ApproximateGradientFunction[K,T](f,eps);
      val empirical = fapprox.gradientAt(x);
      val normPredicted = norm(predicted,2);
      predicted -= empirical
      norm(predicted, 2) / normPredicted;
    }
  }
}