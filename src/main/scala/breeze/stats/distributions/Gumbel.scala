package breeze.stats.distributions

import breeze.numerics.constants.{γ, Pi}
import breeze.numerics.{exp, log}


/**
 * TODO
 *
 * @author dlwh
 **/
case class Gumbel(location: Double, scale: Double)(implicit rand: RandBasis = Rand) extends ContinuousDistr[Double] with Moments[Double, Double] {
  def mean: Double = location + scale * γ

  def mode: Double = location

  def variance: Double = Pi * Pi / 6 * scale * scale

  def entropy: Double = log(scale) + γ + 1

  def logNormalizer: Double = scale

  /**
   * Gets one sample from the distribution. Equivalent to sample()
   */
  def draw(): Double = {
    // from numpy
    val u = rand.uniform.draw()
    location - scale * log(-log(u))
  }

  def unnormalizedLogPdf(x: Double): Double = {
    val z = (x-location)/scale
    -(z + exp(-z))
  }


}
