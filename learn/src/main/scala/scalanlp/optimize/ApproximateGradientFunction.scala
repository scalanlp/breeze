package scalanlp.optimize

import scalala._;
import generic.collection.{CanCopy, CanCreateZerosLike}
import generic.math.CanNorm
import operators.{NumericOps, BinaryOp, OpSub}
import scalala.tensor.mutable;
import library.Library._
import scalanlp.stats.distributions.Rand
;

/**
 * Approximates a gradient by finite differences.
 * @author dlwh
 */
class ApproximateGradientFunction[K,T](f: T=>Double,
                                     epsilon: Double = 1E-5)
                                    (implicit zeros: CanCreateZerosLike[T,T],
                                     view: T<:< mutable.Tensor1[K,Double],
                                     copy: CanCopy[T]) extends DiffFunction[T] {
  override def valueAt(x: T) = f(x);

  def calculate(x:T): (Double, T) = {
    val fx = f(x);
    val grad: T = zeros(x);
    val xx = copy(x)
    for((k,v) <- x.pairsIterator) {
      xx(k) += epsilon;
      grad(k) = (f(xx) - fx) / epsilon;
      xx(k) -= epsilon;
    }
    (fx,grad);
  }

  def calculateAndPrint(x: T, trueGrad: T) = {
    val fx = f(x);
    val grad = zeros(x)
    val xx = copy(x)
    for((k,v) <- x.pairsIteratorNonZero) {
      xx(k) += epsilon;
      grad(k) = (f(xx) - fx) / epsilon;
      xx(k) -= epsilon;
      println("diff : " + epsilon + " val: " + (grad(k) - trueGrad(k)) + " comp: " + trueGrad(k) + " " + grad(k));
    }
    (fx,grad);

  }
}

class RandomizedGradientCheckingFunction[K,T]
          (f: DiffFunction[T], randFraction:Double = 0.01, epsilons: Seq[Double] = Array(1E-5))
          (implicit zeros: CanCreateZerosLike[T,T],
                                     view2: T <:< NumericOps[T],
                                     view: T<:< mutable.Tensor1[K,Double],
                                     copy: CanCopy[T],
                                     canNorm: CanNorm[T],
                                     opSub: BinaryOp[T,T,OpSub,T])
          extends DiffFunction[T] {
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
    val grad = zeros(x)
    val xx = copy(x);
    val subset = Rand.subsetsOfSize(x.keys.toIndexedSeq, (x.size * randFraction).toInt).get()
    for(k <- subset) {
      xx(k) += epsilon;
      grad(k) = (f(xx) - fx) / epsilon;
      xx(k) -= epsilon;
      println(k + "diff : " + epsilon + " val: " + (grad(k) - trueGrad(k)) + " comp: " + trueGrad(k) + " " + grad(k));
    }
    (fx,grad);

  }

}

class GradientCheckingDiffFunction[K,T](f: DiffFunction[T],
                                      epsilons: Seq[Double] = Array(1E-5))
                                    (implicit zeros: CanCreateZerosLike[T,T],
                                     view2: T <:< NumericOps[T],
                                     view: T<:< mutable.Tensor1[K,Double],
                                     copy: CanCopy[T],
                                     canNorm: CanNorm[T],
                                     opSub: BinaryOp[T,T,OpSub,T]) extends DiffFunction[T] {
  val approxes =  for( eps <- epsilons) yield {
    val fapprox = new ApproximateGradientFunction[K,T](f,eps);
    fapprox
  }

  override def valueAt(x: T) = f(x);

  def calculate(x:T) = {
    val (v,predicted:T) = f.calculate(x);
    for { (fap,eps) <- approxes zip epsilons } {
      val empirical = fap.calculateAndPrint(x,predicted)._2;
      println("diff : " + eps + " norm: " + norm(view2(empirical) - predicted,2));
    }
    (v,predicted);
  }

}

object ApproximateGradientTester {
  def apply[K,T](f:DiffFunction[T], x:T, epsilons:Seq[Double]=Array(1E-4))
              (implicit zeros: CanCreateZerosLike[T,T],
               view: T<:< mutable.Tensor1[K,Double],
               view2: T <:< NumericOps[T],
               copy: CanCopy[T],
               canNorm: CanNorm[T],
              opSub: BinaryOp[T,T,OpSub,T]) = {
    val predicted = f.gradientAt(x);
    for( eps <- epsilons) yield {
      val fapprox = new ApproximateGradientFunction[K,T](f,eps);
      val empirical = fapprox.gradientAt(x);
      norm(view2(predicted) - empirical, 2) / norm(predicted,2);
    }
  }
}