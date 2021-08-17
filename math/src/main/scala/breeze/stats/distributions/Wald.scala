package breeze.stats.distributions

import breeze.numerics.constants.Pi
import breeze.numerics.log

/**
 * Also known as the inverse Gaussian Distribution
 *
 * http://en.wikipedia.org/wiki/Inverse_Gaussian_distribution
 *
 * @author dlwh
 **/
case class Wald(mean: Double, shape: Double)(implicit rand: RandBasis)
    extends ContinuousDistr[Double]
    with Moments[Double, Double] {
  lazy val mode: Double = {
    // wiki
    val adjustment = {
      val x = mean / shape
      math.sqrt(1 + 9 * x * x / 4) - 1.5 * x
    }
    adjustment * mean
  }

  def variance: Double = mean * mean * mean / shape

  def entropy: Double = ???

  def logNormalizer: Double = 0.5 * math.log(2 * Pi / shape)

  /**
   * Gets one sample from the distribution. Equivalent to sample()
   */
  def draw(): Double = {
    // from numpy
    gen.draw()
  }

  def unnormalizedLogPdf(x: Double): Double = {
    val z = (x - mean) / mean
    -1.5 * log(x) - 0.5 * shape * z * z / x
  }

  private val gen = for {
    nu <- rand.gaussian(0, 1)
    y = nu * nu
    x = (mean
      + mean * mean * y * 0.5 / shape
      - 0.5 * mean / shape * math.sqrt(4 * mean * shape * y + mean * mean * y * y))
    z <- rand.uniform
  } yield {
    if (z <= mean / (mean + x)) x
    else mean * mean / x
  }

}
