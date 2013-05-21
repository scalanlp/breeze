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
