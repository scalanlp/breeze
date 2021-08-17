package breeze.stats.distributions

import breeze.numerics.constants.{γ, Pi}
import breeze.numerics.{exp, log}

case class Gumbel(location: Double, scale: Double)(implicit rand: RandBasis)
    extends ContinuousDistr[Double]
    with Moments[Double, Double]
    with HasCdf {
  def mean: Double = location + scale * γ

  def mode: Double = location

  def variance: Double = Pi * Pi / 6 * scale * scale

  def entropy: Double = log(scale) + γ + 1

  def logNormalizer: Double = math.log(scale)

  /**
   * Gets one sample from the distribution. Equivalent to sample()
   */
  def draw(): Double = {
    // from numpy
    val u = rand.uniform.draw()
    location - scale * log(-log(u))
  }

  def unnormalizedLogPdf(x: Double): Double = {
    val z = (x - location) / scale
    -(z + exp(-z))
  }

  def cdf(x: Double): Double = {
    math.exp(-math.exp(-(x - location) / scale))
  }

  override def probability(x: Double, y: Double): Double = {
    cdf(y) - cdf(x)
  }
}
