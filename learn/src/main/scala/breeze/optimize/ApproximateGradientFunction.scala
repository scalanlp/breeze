package breeze.optimize

import breeze._
import breeze.stats.distributions.Rand
import linalg.operators.{OpSub, BinaryOp}
import linalg.support.{CanNorm, CanCopy, CanCreateZerosLike}
import linalg.{NumericOps, Tensor}


/**
 * Approximates a gradient by finite differences.
 * @author dlwh
 */
class ApproximateGradientFunction[K,T](f: T=>Double,
                                     epsilon: Double = 1E-5)
                                    (implicit zeros: CanCreateZerosLike[T,T],
                                     view: T<:< Tensor[K,Double],
                                     copy: CanCopy[T]) extends DiffFunction[T] {
  override def valueAt(x: T) = f(x)

  def calculate(x:T): (Double, T) = {
    val fx = f(x)
    val grad: T = zeros(x)
    val xx = copy(x)
    for((k,v) <- x.iterator) {
      xx(k) += epsilon
      grad(k) = (f(xx) - fx) / epsilon
      xx(k) -= epsilon
    }
    (fx,grad)
  }

  def calculateAndPrint(x: T, trueGrad: T) = {
    val fx = f(x)
    val grad = zeros(x)
    val xx = copy(x)
    for((k,v) <- x.activeIterator) {
      xx(k) += epsilon
      grad(k) = (f(xx) - fx) / epsilon
      xx(k) -= epsilon
      println("diff : " + epsilon + " val: " + (grad(k) - trueGrad(k)) + " dp: " + trueGrad(k) + " empirical: " + grad(k))
    }
    (fx,grad)

  }
}

class RandomizedGradientCheckingFunction[K,T](f: DiffFunction[T],
                                              randFraction:Double = 0.01,
                                              epsilons: Seq[Double] = Array(1E-8),
                                              toString: K=>String = {(_:K).toString})
                                             (implicit zeros: CanCreateZerosLike[T,T],
                                              view2: T <:< NumericOps[T],
                                              view: T<:< Tensor[K,Double],
                                              copy: CanCopy[T],
                                              canNorm: CanNorm[T],
                                              opSub: BinaryOp[T,T,OpSub,T]) extends DiffFunction[T] {
  val approxes =  for( eps <- epsilons) yield {
    val fapprox = new ApproximateGradientFunction[K,T](f,eps)
    fapprox
  }

  override def valueAt(x: T) = f(x)

  def calculate(x:T) = {
    val (v,predicted) = f.calculate(x)
    for { (fap,eps) <- approxes zip epsilons } {
      calculateAndPrint(eps, x,predicted)._2
    }
    (v,predicted)
  }

  def calculateAndPrint(epsilon: Double, x: T, trueGrad: T) = {
    val fx = f(x)
    val grad = zeros(x)
    val xx = copy(x)
    val subset = Rand.subsetsOfSize(x.keysIterator.toIndexedSeq, (x.size * randFraction).toInt).get()
    for(k <- subset) {
      xx(k) += epsilon
      grad(k) = (f(xx) - fx) / epsilon
      xx(k) -= epsilon
      println(toString(k) + "diff : " + epsilon + " val: " + (grad(k) - trueGrad(k)) + " dp: " + trueGrad(k) + " empirical:" + grad(k))
    }
    (fx,grad)

  }

}

class GradientCheckingDiffFunction[K,T](f: DiffFunction[T],
                                      epsilons: Seq[Double] = Array(1E-5))
                                    (implicit zeros: CanCreateZerosLike[T,T],
                                     view2: T <:< NumericOps[T],
                                     view: T<:< Tensor[K,Double],
                                     copy: CanCopy[T],
                                     canNorm: CanNorm[T],
                                     opSub: BinaryOp[T,T,OpSub,T]) extends DiffFunction[T] {
  val approxes =  for( eps <- epsilons) yield {
    val fapprox = new ApproximateGradientFunction[K,T](f,eps)
    fapprox
  }

  override def valueAt(x: T) = f(x)

  def calculate(x:T) = {
    val (v,predicted:T) = f.calculate(x)
    for { (fap,eps) <- approxes zip epsilons } {
      val empirical = fap.calculateAndPrint(x,predicted)._2
      println("diff : " + eps + " norm: " + canNorm(view2(empirical) - predicted,2))
    }
    (v,predicted)
  }

}

object ApproximateGradientTester {
  def apply[K,T](f:DiffFunction[T], x:T, epsilons:Seq[Double]=Array(1E-4))
              (implicit zeros: CanCreateZerosLike[T,T],
               view: T<:< Tensor[K,Double],
               view2: T <:< NumericOps[T],
               copy: CanCopy[T],
               canNorm: CanNorm[T],
              opSub: BinaryOp[T,T,OpSub,T]) = {
    val predicted = f.gradientAt(x)
    for( eps <- epsilons) yield {
      val fapprox = new ApproximateGradientFunction[K,T](f,eps)
      val empirical = fapprox.gradientAt(x)
      canNorm(view2(predicted) - empirical, 2) / canNorm(predicted,2)
    }
  }
}