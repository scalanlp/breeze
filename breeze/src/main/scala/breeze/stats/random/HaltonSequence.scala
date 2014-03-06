package breeze.stats.random

import breeze.stats.distributions.Rand
import breeze.linalg.DenseVector

/**
 * Generates a quasi-random sequence of dim-dimensional vectors
 * @author dlwh
 */
class HaltonSequence(dim: Int) extends Rand[DenseVector[Double]] {
  require(dim > 0, "dim must be positive!")

  private var count = 0
  val primes = Array.iterate(2L,dim) { last => new java.math.BigInteger(last.toString).nextProbablePrime().longValue() }

  def draw() = {
    count += 1
    val arr = primes.map { prime =>
      var h = 0.0
      var f = 1.0
      var k : Long = count
      while(k > 0) {
        f /= prime
        h += (k % prime) * f
        k /= prime
      }
      h % 1.0
    }
    new DenseVector[Double](arr)
  }
}