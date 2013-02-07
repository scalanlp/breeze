package breeze.optimize

import breeze.linalg.support.{CanNorm, CanCopy, CanCreateZerosLike}
import breeze.linalg.{Tensor, NumericOps}
import breeze.linalg.operators.{OpSub, BinaryOp}
import breeze.stats.distributions.Rand

/**
 * Class that compares the computed gradient with an empirical gradient based on
 * finite differences. Essential for debugging dynamic programs.
 *
 * @author dlwh
 */
object GradientTester {
  /**
   * Tests a gradient
   * @param f the function to test
   * @param x point to test from
   * @param randFraction what percentage of x's domain to try.
   * @param skipZeros should we skip components of x where the calculated gradient is 0.
   *                  (Sometimes useful with sparse features. You might want to check that 0's are always 0's though!)
   * @param epsilon Difference to try
   * @param tolerance How big a relative difference before we start complaining.
   * @param toString toString function for converting elements of x's domain to a string.
   * @tparam K
   * @tparam T
   * @return
   */
  def test[K,T](f: DiffFunction[T], x: T,
                randFraction:Double = 0.01,
                skipZeros: Boolean = false,
                epsilon: Double = 1E-8,
                tolerance: Double = 1E-3,
                toString: K=>String = {(_:K).toString})
               (implicit  view2: T <:< NumericOps[T],
                view: T<:< Tensor[K,Double],
                copy: CanCopy[T],
                canNorm: CanNorm[T],
                opSub: BinaryOp[T,T,OpSub,T]) = {

    val (fx,trueGrad) = f.calculate(x)
    val xx = copy(x)
    val subsetOfDimensions = Rand.subsetsOfSize(x.keysIterator.toIndexedSeq, (x.size * randFraction + 1).toInt).get()
    var lastWasOk = false
    var lastWasZero = true
    for (k <- subsetOfDimensions) {
      if(skipZeros && trueGrad(k) == 0.0) {
         if (!lastWasZero) {
          println()
          print("Zero Grad: ")
        }
        lastWasZero = true
        lastWasOk = false
        print(toString(k) + " ")
      } else {
        xx(k) += epsilon
        val grad = (f(xx) - fx) / epsilon
        xx(k) -= epsilon
        val relDif =  (grad - trueGrad(k))/math.max(trueGrad(k).abs, grad.abs).max(1E-4)
        if (relDif < tolerance) {
          if(!lastWasOk) print("Ok: ")
          print(toString(k) + " ")
          lastWasOk = true
          lastWasZero = false
        } else {
          if(lastWasOk || lastWasZero) {
            println()
            lastWasOk = false
            lastWasZero = false
          }
          println(toString(k) + " relDif: %.3e [eps : %e, calculated: %4.3e empirical: %4.3e]".format(relDif, epsilon, trueGrad(k), grad))
        }
      }
    }
  }
}
