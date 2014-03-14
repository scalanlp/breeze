package breeze.stats.distributions

import breeze.numerics.constants.{γ, Pi}
import breeze.numerics.{sqrt, exp, log}

/**
 * TODO
 *
 * @author dlwh
 **/
case class Rayleigh(scale: Double)(implicit rand: RandBasis = Rand) extends ContinuousDistr[Double] with Moments[Double, Double] {
  def mean: Double = scale * sqrt(Pi/2)

  def mode: Double = scale

  def variance: Double = (4 - Pi)/2 * scale * scale

  def entropy: Double = log(scale/sqrt(2)) + γ/2 + 1

  def logNormalizer: Double = scale * scale

  /**
   * Gets one sample from the distribution. Equivalent to sample()
   */
  def draw(): Double = {
    // from numpy
    math.sqrt(2 * new Exponential(1).draw() * scale * scale)
  }

  def unnormalizedLogPdf(x: Double): Double = {
    log(x/(scale * scale)) - x * x / (2 * scale * scale)
  }


}
