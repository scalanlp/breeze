package breeze.optimize

import breeze.linalg.support.CanCopy
import breeze.linalg.{norm, Tensor, NumericOps}
import breeze.linalg.operators.{OpSub, BinaryOp}
import breeze.stats.distributions.Rand
import breeze.util.SerializableLogging

/**
 * Class that compares the computed gradient with an empirical gradient based on
 * finite differences. Essential for debugging dynamic programs.
 *
 * @author dlwh
 */
object GradientTester extends SerializableLogging {

  /**
   * Tests a gradient by comparing the gradient to the empirically calculated gradient from finite differences,
   * returning those that are bad, logging bad ones on WARN, ok ones on DEBUG, and overall statistics on INFO.
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
   * @return differences in each component
   */
  def test[K, T](
      f: DiffFunction[T],
      x: T,
      randFraction: Double = 0.01,
      skipZeros: Boolean = false,
      epsilon: Double = 1e-8,
      tolerance: Double = 1e-3,
      toString: K => String = { (_: K).toString }
  )(implicit
      view2: T <:< NumericOps[T],
      view: T <:< Tensor[K, Double],
      copy: CanCopy[T],
      canNorm: norm.Impl[T, Double],
      opSub: OpSub.Impl2[T, T, T]
  ) = {
    val indices = Rand.subsetsOfSize(x.keysIterator.toIndexedSeq, (x.size * randFraction + 1).toInt).get()
    testIndices(f, x, indices, skipZeros, toString, epsilon, tolerance)
  }

  def testIndices[T, K](
      f: DiffFunction[T],
      x: T,
      indices: Iterable[K],
      skipZeros: Boolean = false,
      toString: K => String = { (_: K).toString },
      epsilon: Double = 1e-8,
      tolerance: Double = 1e-3
  )(implicit
      view2: T <:< NumericOps[T],
      view: T <:< Tensor[K, Double],
      copy: CanCopy[T],
      canNorm: norm.Impl[T, Double],
      opSub: OpSub.Impl2[T, T, T]
  ): T = {
    val (fx, trueGrad) = f.calculate(x)
    val xx = copy(x)
    val differences = opSub(x, x)
    var ok, tried = 0
    val sz = indices.size
    for (k <- indices) {
      if (skipZeros && trueGrad(k) == 0.0) {
        logger.debug(s"Zero Grad: ${toString(k)}")
        print(toString(k) + " ")
      } else {
        xx(k) += epsilon
        val grad = (f(xx) - fx) / epsilon
        xx(k) -= epsilon
        val relDif = (grad - trueGrad(k)).abs / math.max(trueGrad(k).abs, grad.abs).max(1e-4)
        if (relDif < tolerance) {
          ok += 1
          logger.debug(s"OK: ${toString(k)} $relDif")
        } else {
          logger.warn(
            toString(k) + " relDif: %.3e [eps : %e, calculated: %4.3e empirical: %4.3e]"
              .format(relDif, epsilon, trueGrad(k), grad)
          )
        }
        differences(k) = relDif
        tried += 1
      }
      if (tried % 100 == 0 || tried == sz) {
        logger.info(f"Checked $tried of ${sz} (out of dimension ${x.size}). ${ok * 100.0 / tried}%.4g%% ok.")
      }
    }
    differences
  }
}
