package breeze.optimize

import breeze.linalg.support.{CanNorm, CanCopy, CanCreateZerosLike}
import breeze.linalg.{Tensor, NumericOps}
import breeze.linalg.operators.{OpSub, BinaryOp}
import breeze.stats.distributions.Rand

/**
 * 
 *
 * @author dlwh
 */
object GradientTester {
  def test[K,T](f: DiffFunction[T], x: T,
                randFraction:Double = 0.01,
                epsilons: Seq[Double] = Array(1E-8),
                toString: K=>String = {(_:K).toString})
               (implicit zeros: CanCreateZerosLike[T,T],
                view2: T <:< NumericOps[T],
                view: T<:< Tensor[K,Double],
                copy: CanCopy[T],
                canNorm: CanNorm[T],
                opSub: BinaryOp[T,T,OpSub,T]) = {

    val (fx,trueGrad) = f.calculate(x)
    val xx = copy(x)
    val subsetOfDimensions = Rand.subsetsOfSize(x.keysIterator.toIndexedSeq, (x.size * randFraction + 1).toInt).get()
    var lastWasOk = false
    for (epsilon <- epsilons; k <- subsetOfDimensions) {
      xx(k) += epsilon
      val grad = (f(xx) - fx) / epsilon
      xx(k) -= epsilon
      val relDif =  (grad - trueGrad(k))/math.max(trueGrad(k).abs, grad.abs).max(1E-4)
      if (relDif < 1E-4) {
        if(!lastWasOk) print("Ok: ")
        print(toString(k) + " ")
        lastWasOk = true
      } else {
        if(lastWasOk) {
          println()
          lastWasOk = false
        }
        println(toString(k) + " relDif: %.4e [eps : %4e, calculated: %4e empirical: %4e]".format(relDif, epsilon, trueGrad(k), grad))
      }
    }
  }
}
