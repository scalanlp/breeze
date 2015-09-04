package breeze.stats.distributions

import breeze.numerics.{exp, log}

/**
 * http://en.wikipedia.org/wiki/Laplace_distribution
 *
 * @author dlwh
 **/
case class Laplace(location: Double, scale: Double)(implicit rand: RandBasis = Rand) extends ContinuousDistr[Double] with Moments[Double, Double] with HasCdf {
  def mean: Double = location

  def mode: Double = location

  def variance: Double = 2 * scale * scale

  def entropy: Double = 1 + log(2 * scale)

  def logNormalizer: Double = math.log(2 * scale)

  /**
   * Gets one sample from the distribution. Equivalent to sample()
   */
  def draw(): Double = {
    // from numpy
    val u = rand.uniform.draw()
    if(u < 0.5) {
      location + scale * log(2 * u)
    } else {
      location - scale * log(2 *(1 - u))
    }
  }

  def unnormalizedLogPdf(x: Double): Double = {
    -math.abs(x-location)/scale
  }

  def probability(x: Double, y: Double): Double = {
    cdf(y) - cdf(x)
  }

  def cdf(x: Double) = x match {
    case Double.NegativeInfinity => 0.0
    case Double.PositiveInfinity => 1.0
    case x if x < location =>
      0.5 * exp(unnormalizedLogPdf(x))
    case x =>
      1 - 0.5 * exp(unnormalizedLogPdf(x))
  }

}
